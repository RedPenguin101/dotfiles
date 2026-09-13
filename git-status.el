;;; git-status.el --- A dired-like buffer over git status -*- lexical-binding: t; -*-

;; `git-status' presents the working tree the way `git status' does on
;; the command line - a line per file, grouped into staged, unstaged,
;; unmerged and untracked sections - and makes those lines actionable
;; the way `dired' makes its lines actionable. You mark files and then
;; run a command over the marked set.
;;
;; The point of the mode, and the reason it does not simply extend
;; vc-dir, is the index. vc's data model carries one status per file
;; and vc-git computes it with `git diff-index' without `--cached',
;; which by construction cannot tell a staged change from an unstaged
;; one. Staging is therefore invisible in vc-dir. Here the index is the
;; organising principle: which section a line sits in says whether that
;; change is staged, and `s' and `r' move a file between them.
;;
;; The buffer is rendered from `git status --porcelain=v2', not from
;; git's human output, and each line carries its parsed entry as a text
;; property. Commands read that property rather than re-parsing the
;; display, so paths that are quoted, escaped, renamed or relative in
;; the human output are never a concern.
;;
;; Entry points:
;;
;;   M-x git-status          open the buffer for the current repository
;;   m / u / U / t           mark, unmark, unmark all, toggle marks
;;   s / r                   stage (git add), unstage (git restore --staged)
;;   k                       discard working tree changes (asks first)
;;   =                       diff the marked files, or the file at point
;;   + / -  (in the diff)     stage / unstage just the hunk at point
;;   c                       commit the staged changes
;;   P / +                   push / pull, in the background
;;   RET                     visit the file
;;   g                       refresh

;;; Code:

(require 'cl-lib)
(require 'diff-mode)
(require 'log-edit)
(require 'rfc822)

(defgroup git-status nil
  "A dired-like buffer over `git status'."
  :group 'tools)

(defface git-status-section-heading
  '((t :inherit font-lock-keyword-face :weight bold))
  "Face for the section headings that group files by their index state."
  :group 'git-status)

(defface git-status-change-label
  '((t :inherit font-lock-type-face))
  "Face for the per-file change label, such as \"modified:\"."
  :group 'git-status)

(defface git-status-mark
  '((t :inherit warning :weight bold))
  "Face for the mark character in the left gutter."
  :group 'git-status)

(defface git-status-branch
  '((t :inherit font-lock-function-name-face :weight bold))
  "Face for the branch name in the buffer header."
  :group 'git-status)

;; A single file as reported by porcelain v2. INDEX-STATUS and
;; WORKTREE-STATUS are the two porcelain status characters; `?.' means
;; unchanged on that side and `??' means untracked. ORIG-PATH is set
;; only for a rename or copy. UNMERGED marks a conflict, where the two
;; status characters name the two sides of the merge instead.
(cl-defstruct git-status-entry
  path orig-path index-status worktree-status unmerged)

(defvar-local git-status--root nil
  "Absolute path to the root of the repository this buffer is showing.")

;; Set in the diff buffer so that a hunk command knows which side of the
;; index it is looking at, and can recompute the diff after changing it.
(defvar-local git-status-diff--section nil
  "Which side of the index this diff shows: `staged' or `unstaged'.")

(defvar-local git-status-diff--paths nil
  "The paths this diff was limited to.")

(defvar-local git-status-diff--status-buffer nil
  "The `git-status-mode' buffer this diff was opened from.")


;;; Running git

(defun git-status--run-git (&rest args)
  "Run git with ARGS in `git-status--root' and return its stdout as a string.
Signal an error carrying git's own diagnostics when the command fails.

A missing root is an error rather than a fallback to `default-directory':
these commands write to a repository, and guessing which one from ambient
state is how a write lands in the wrong tree."
  (unless git-status--root
    (error "No repository root recorded for buffer %s" (buffer-name)))
  (let ((directory git-status--root))
    (with-temp-buffer
      (let* ((default-directory directory)
             (status (apply #'process-file "git" nil t nil args)))
        (unless (eq status 0)
          (error "git %s: %s" (car args) (string-trim (buffer-string))))
        (buffer-string)))))


;;; Parsing

(defun git-status--parse-porcelain-v2 (output)
  "Parse OUTPUT of `git status --porcelain=v2 -z --branch' into a plist.
The plist is (:branch NAME :ahead N :behind N :entries (ENTRY...)) where
each ENTRY is a `git-status-entry'.  Pure: this reads no buffer and runs
no process, so it can be tested against a captured string."
  (let ((fields (split-string output "\0" t))
        (branch nil)
        (ahead 0)
        (behind 0)
        (entries '()))
    (while fields
      (let ((field (pop fields)))
        (cond
         ;; Header lines
         ((string-prefix-p "# branch.head " field)
          (setq branch (substring field (length "# branch.head "))))
         ((string-prefix-p "# branch.ab " field)
          (when (string-match "\\`# branch\\.ab \\+\\([0-9]+\\) -\\([0-9]+\\)\\'" field)
            (setq ahead (string-to-number (match-string 1 field))
                  behind (string-to-number (match-string 2 field)))))
         ((string-prefix-p "# " field)
          nil)

         ;; Ordinary change: 1 <XY> <sub> <mH> <mI> <mW> <hH> <hI> <path>
         ((string-match "\\`1 \\(.\\)\\(.\\) \\(?:[^ ]+ \\)\\{6\\}\\(.*\\)\\'" field)
          (push (make-git-status-entry
                 :path (match-string 3 field)
                 :index-status (aref (match-string 1 field) 0)
                 :worktree-status (aref (match-string 2 field) 0))
                entries))

         ;; Rename or copy carries one extra field, and the original path
         ;; arrives as the next NUL-separated field rather than inline.
         ((string-match "\\`2 \\(.\\)\\(.\\) \\(?:[^ ]+ \\)\\{7\\}\\(.*\\)\\'" field)
          (push (make-git-status-entry
                 :path (match-string 3 field)
                 :orig-path (pop fields)
                 :index-status (aref (match-string 1 field) 0)
                 :worktree-status (aref (match-string 2 field) 0))
                entries))

         ;; Unmerged: 1 more mode and 1 more hash than an ordinary change
         ((string-match "\\`u \\(.\\)\\(.\\) \\(?:[^ ]+ \\)\\{8\\}\\(.*\\)\\'" field)
          (push (make-git-status-entry
                 :path (match-string 3 field)
                 :index-status (aref (match-string 1 field) 0)
                 :worktree-status (aref (match-string 2 field) 0)
                 :unmerged t)
                entries))

         ((string-prefix-p "? " field)
          (push (make-git-status-entry
                 :path (substring field 2)
                 :index-status ?.
                 :worktree-status ?\?)
                entries)))))
    (list :branch branch :ahead ahead :behind behind :entries (nreverse entries))))

(defun git-status--change-label (status-character)
  "The `git status' wording for a porcelain STATUS-CHARACTER."
  (pcase status-character
    (?M "modified:")
    (?A "new file:")
    (?D "deleted:")
    (?R "renamed:")
    (?C "copied:")
    (?T "typechange:")
    (?U "unmerged:")
    (other (format "status %c:" other))))

(defun git-status--unmerged-label (entry)
  "The `git status' wording for the conflict ENTRY describes."
  (pcase (cons (git-status-entry-index-status entry)
               (git-status-entry-worktree-status entry))
    ('(?D . ?D) "both deleted:")
    ('(?A . ?U) "added by us:")
    ('(?U . ?D) "deleted by them:")
    ('(?U . ?A) "added by them:")
    ('(?D . ?U) "deleted by us:")
    ('(?A . ?A) "both added:")
    ('(?U . ?U) "both modified:")
    (other (format "conflict %c%c:" (car other) (cdr other)))))

(defun git-status--display-path (entry)
  "The path of ENTRY, written as a rename when it is one."
  (if (git-status-entry-orig-path entry)
      (format "%s -> %s"
              (git-status-entry-orig-path entry)
              (git-status-entry-path entry))
    (git-status-entry-path entry)))


;;; Rendering

(defun git-status-refresh ()
  "Re-read `git status' and redraw the buffer, preserving marks and point."
  (interactive)
  (unless (derived-mode-p 'git-status-mode)
    (user-error "Not in a git-status buffer"))
  (let* ((marked-paths (mapcar (lambda (marked) (git-status-entry-path (cdr marked)))
                               (git-status--marked-entries)))
         (previous-line (line-number-at-pos))
         (parsed (git-status--parse-porcelain-v2
                  (git-status--run-git "status" "--porcelain=v2" "-z"
                                       "--branch" "--untracked-files=all")))
         (entries (plist-get parsed :entries))
         (staged (seq-filter (lambda (entry)
                               (and (not (git-status-entry-unmerged entry))
                                    (not (memq (git-status-entry-index-status entry) '(?. ?\?)))))
                             entries))
         (unstaged (seq-filter (lambda (entry)
                                 (and (not (git-status-entry-unmerged entry))
                                      (not (memq (git-status-entry-worktree-status entry) '(?. ?\?)))))
                               entries))
         (unmerged (seq-filter #'git-status-entry-unmerged entries))
         (untracked (seq-filter (lambda (entry)
                                  (eq (git-status-entry-worktree-status entry) ?\?))
                                entries))
         (inhibit-read-only t))
    (erase-buffer)

    ;; Header
    (insert "On branch "
            (propertize (or (plist-get parsed :branch) "(detached)") 'face 'git-status-branch))
    (let ((ahead (plist-get parsed :ahead))
          (behind (plist-get parsed :behind)))
      (when (> ahead 0) (insert (format ", ahead %d" ahead)))
      (when (> behind 0) (insert (format ", behind %d" behind))))
    (insert "\n")

    ;; Section: staged
    (when staged
      (insert "\n" (propertize "Changes to be committed:" 'face 'git-status-section-heading) "\n")
      (dolist (entry staged)
        (let ((start (point)))
          (insert "  "
                  (propertize (format "%-12s" (git-status--change-label
                                               (git-status-entry-index-status entry)))
                              'face 'git-status-change-label)
                  (git-status--display-path entry)
                  "\n")
          (put-text-property start (point) 'git-status-entry entry)
          (put-text-property start (point) 'git-status-section 'staged))))

    ;; Section: unstaged
    (when unstaged
      (insert "\n" (propertize "Changes not staged for commit:" 'face 'git-status-section-heading) "\n")
      (dolist (entry unstaged)
        (let ((start (point)))
          (insert "  "
                  (propertize (format "%-12s" (git-status--change-label
                                               (git-status-entry-worktree-status entry)))
                              'face 'git-status-change-label)
                  (git-status-entry-path entry)
                  "\n")
          (put-text-property start (point) 'git-status-entry entry)
          (put-text-property start (point) 'git-status-section 'unstaged))))

    ;; Section: unmerged
    (when unmerged
      (insert "\n" (propertize "Unmerged paths:" 'face 'git-status-section-heading) "\n")
      (dolist (entry unmerged)
        (let ((start (point)))
          (insert "  "
                  (propertize (format "%-17s" (git-status--unmerged-label entry))
                              'face 'git-status-change-label)
                  (git-status-entry-path entry)
                  "\n")
          (put-text-property start (point) 'git-status-entry entry)
          (put-text-property start (point) 'git-status-section 'unmerged))))

    ;; Section: untracked
    (when untracked
      (insert "\n" (propertize "Untracked files:" 'face 'git-status-section-heading) "\n")
      (dolist (entry untracked)
        (let ((start (point)))
          (insert "  " (git-status-entry-path entry) "\n")
          (put-text-property start (point) 'git-status-entry entry)
          (put-text-property start (point) 'git-status-section 'untracked))))

    (unless entries
      (insert "\nnothing to commit, working tree clean\n"))

    ;; Restore the marks that survived, then put point back where it was
    (goto-char (point-min))
    (while (not (eobp))
      (let ((entry (get-text-property (line-beginning-position) 'git-status-entry)))
        (when (and entry (member (git-status-entry-path entry) marked-paths))
          (subst-char-in-region (line-beginning-position) (1+ (line-beginning-position)) ?\s ?*)
          (put-text-property (line-beginning-position) (1+ (line-beginning-position))
                             'face 'git-status-mark)))
      (forward-line 1))
    (goto-char (point-min))
    (forward-line (1- previous-line))
    (when (eobp) (git-status-previous-file))
    (set-buffer-modified-p nil)))


;;; Marking

(defun git-status--marked-entries ()
  "The marked lines as a list of (SECTION . ENTRY), in buffer order."
  (let ((marked '()))
    (save-excursion
      (goto-char (point-min))
      (while (not (eobp))
        (let ((entry (get-text-property (line-beginning-position) 'git-status-entry)))
          (when (and entry (eq (char-after (line-beginning-position)) ?*))
            (push (cons (get-text-property (line-beginning-position) 'git-status-section) entry)
                  marked)))
        (forward-line 1)))
    (nreverse marked)))

(defun git-status--selected-entries ()
  "The marked lines, or the line at point when nothing is marked.
Each element is (SECTION . ENTRY)."
  (or (git-status--marked-entries)
      (let ((entry (get-text-property (line-beginning-position) 'git-status-entry)))
        (unless entry
          (user-error "No file on this line"))
        (list (cons (get-text-property (line-beginning-position) 'git-status-section) entry)))))

(defun git-status-mark ()
  "Mark the file at point and move to the next one."
  (interactive)
  (let ((inhibit-read-only t))
    (when (get-text-property (line-beginning-position) 'git-status-entry)
      (subst-char-in-region (line-beginning-position) (1+ (line-beginning-position)) ?\s ?*)
      (put-text-property (line-beginning-position) (1+ (line-beginning-position))
                         'face 'git-status-mark)))
  (git-status-next-file))

(defun git-status-unmark ()
  "Unmark the file at point and move to the next one."
  (interactive)
  (let ((inhibit-read-only t))
    (when (get-text-property (line-beginning-position) 'git-status-entry)
      (subst-char-in-region (line-beginning-position) (1+ (line-beginning-position)) ?* ?\s)))
  (git-status-next-file))

(defun git-status-unmark-all ()
  "Remove every mark in the buffer."
  (interactive)
  (let ((inhibit-read-only t))
    (save-excursion
      (goto-char (point-min))
      (while (not (eobp))
        (when (get-text-property (line-beginning-position) 'git-status-entry)
          (subst-char-in-region (line-beginning-position) (1+ (line-beginning-position)) ?* ?\s))
        (forward-line 1)))))

(defun git-status-toggle-marks ()
  "Mark every unmarked file and unmark every marked one."
  (interactive)
  (let ((inhibit-read-only t))
    (save-excursion
      (goto-char (point-min))
      (while (not (eobp))
        (when (get-text-property (line-beginning-position) 'git-status-entry)
          (if (eq (char-after (line-beginning-position)) ?*)
              (subst-char-in-region (line-beginning-position) (1+ (line-beginning-position)) ?* ?\s)
            (subst-char-in-region (line-beginning-position) (1+ (line-beginning-position)) ?\s ?*)
            (put-text-property (line-beginning-position) (1+ (line-beginning-position))
                               'face 'git-status-mark)))
        (forward-line 1)))))


;;; Movement

(defun git-status-next-file ()
  "Move point to the next line that names a file."
  (interactive)
  (let ((start (point)))
    (forward-line 1)
    (while (and (not (eobp))
                (not (get-text-property (line-beginning-position) 'git-status-entry)))
      (forward-line 1))
    (when (eobp) (goto-char start))))

(defun git-status-previous-file ()
  "Move point to the previous line that names a file."
  (interactive)
  (let ((start (point)))
    (forward-line -1)
    (while (and (not (bobp))
                (not (get-text-property (line-beginning-position) 'git-status-entry)))
      (forward-line -1))
    (unless (get-text-property (line-beginning-position) 'git-status-entry)
      (goto-char start))))


;;; Index commands

(defun git-status-stage ()
  "Stage the marked files, or the file at point."
  (interactive)
  (let ((paths (mapcar (lambda (selected) (git-status-entry-path (cdr selected)))
                       (git-status--selected-entries))))
    (apply #'git-status--run-git "add" "--" paths)
    (git-status-unmark-all)
    (git-status-refresh)
    (message "Staged %d file(s)" (length paths))))

(defun git-status-unstage ()
  "Unstage the marked files, or the file at point."
  (interactive)
  (let ((paths (mapcar (lambda (selected) (git-status-entry-path (cdr selected)))
                       (git-status--selected-entries))))
    (apply #'git-status--run-git "restore" "--staged" "--" paths)
    (git-status-unmark-all)
    (git-status-refresh)
    (message "Unstaged %d file(s)" (length paths))))

(defun git-status-discard ()
  "Discard working tree changes to the marked files, or the file at point.
An untracked file is deleted.  This cannot be undone, so it asks first."
  (interactive)
  (let* ((selected (git-status--selected-entries))
         (untracked (mapcar (lambda (s) (git-status-entry-path (cdr s)))
                            (seq-filter (lambda (s) (eq (car s) 'untracked)) selected)))
         (tracked (mapcar (lambda (s) (git-status-entry-path (cdr s)))
                          (seq-remove (lambda (s) (eq (car s) 'untracked)) selected))))
    (unless (yes-or-no-p
             (format "Discard changes to %d file(s), deleting %d untracked file(s)? "
                     (length tracked) (length untracked)))
      (user-error "Not discarding"))
    (when tracked
      (apply #'git-status--run-git "restore" "--worktree" "--" tracked))
    (dolist (path untracked)
      (delete-file (expand-file-name path git-status--root)))
    (git-status-unmark-all)
    (git-status-refresh)))


;;; Viewing

(defvar-keymap git-status-hunk-mode-map
  :doc "Keymap for `git-status-hunk-mode'."
  "+" #'git-status-stage-hunk
  "-" #'git-status-unstage-hunk)

(define-minor-mode git-status-hunk-mode
  "Stage and unstage individual hunks of a `git-status' diff.

\\{git-status-hunk-mode-map}"
  :lighter " Hunk")

(defun git-status-diff--redisplay (root section paths status-buffer)
  "Draw the diff of PATHS for SECTION of the repository at ROOT.
STATUS-BUFFER is the `git-status-mode' buffer to refresh after a hunk is
staged.  The four are taken as arguments rather than read from the buffer
because `diff-mode' runs `kill-all-local-variables'."
  (let ((diff (let ((git-status--root root))
                (apply #'git-status--run-git
                       (append '("diff" "--no-ext-diff" "--no-color"
                                 "--src-prefix=a/" "--dst-prefix=b/")
                               (when (eq section 'staged) '("--cached"))
                               '("--") paths))))
        (inhibit-read-only t))
    (erase-buffer)
    (insert diff)
    (setq default-directory root)
    ;; Read-only before `diff-mode', not after: the mode only turns on
    ;; its single-key navigation for a buffer that is already read-only
    ;; when it starts.
    (setq buffer-read-only t)
    (diff-mode)
    ;; After `diff-mode', which runs `kill-all-local-variables'.  These
    ;; assignments must not sit inside a `let' of the same names, or they
    ;; would set the dynamic binding and leave the buffer-local unset.
    (setq git-status--root root
          git-status-diff--section section
          git-status-diff--paths paths
          git-status-diff--status-buffer status-buffer)
    (git-status-hunk-mode 1)
    (goto-char (point-min))))

(defun git-status-diff ()
  "Diff the marked files, or the file at point.
A line in the staged section diffs the index against HEAD; a line in the
unstaged section diffs the working tree against the index."
  (interactive)
  (let* ((selected (git-status--selected-entries))
         (section (car (car selected)))
         (paths (mapcar (lambda (s) (git-status-entry-path (cdr s))) selected))
         (root git-status--root)
         (status-buffer (current-buffer))
         (buffer (get-buffer-create "*git-status-diff*")))
    (when (eq section 'untracked)
      (user-error "No diff for an untracked file"))
    (with-current-buffer buffer
      (git-status-diff--redisplay root section paths status-buffer))
    (pop-to-buffer buffer)))


;;; Staging a single hunk
;;
;; `git apply --cached' patches the index and leaves the working tree
;; alone, which is staging.  The two sides line up because an unstaged
;; diff is working tree against index, so its "old" side is exactly what
;; `--cached' applies against; a staged diff is index against HEAD, so
;; reverse-applying one of its hunks removes it from the index.

(defun git-status-diff--patch-for-hunk-at-point ()
  "The hunk at point rewritten as a patch that git can apply on its own.
A hunk by itself is not a valid patch - it needs the \"diff --git\" and
\"---\"/\"+++\" lines of the file it belongs to, which this prepends."
  (save-excursion
    (let* ((bounds (diff-bounds-of-hunk))
           (hunk (buffer-substring-no-properties (nth 0 bounds) (nth 1 bounds)))
           (file-header (progn
                          (goto-char (nth 0 bounds))
                          (diff-beginning-of-file-and-junk)
                          (let ((header-start (point)))
                            (re-search-forward diff-hunk-header-re)
                            (buffer-substring-no-properties header-start
                                                            (match-beginning 0)))))
           (patch (concat file-header hunk)))
      (if (string-suffix-p "\n" patch) patch (concat patch "\n")))))

(defun git-status-diff--apply-hunk-to-index (reverse)
  "Apply the hunk at point to the index, reverse-applying it when REVERSE.
Redraws this buffer and the status buffer it came from, and leaves point
on the hunk that took the applied one's place."
  (let* ((patch (git-status-diff--patch-for-hunk-at-point))
         (patch-file (make-temp-file "git-status-hunk" nil ".patch" patch))
         (hunk-ordinal 0)
         (hunk-start (nth 0 (diff-bounds-of-hunk))))
    (save-excursion
      (goto-char (point-min))
      (while (and (re-search-forward diff-hunk-header-re nil t)
                  (< (match-beginning 0) hunk-start))
        (setq hunk-ordinal (1+ hunk-ordinal))))
    (unwind-protect
        (apply #'git-status--run-git
               (append '("apply" "--cached")
                       (when reverse '("-R"))
                       (list patch-file)))
      (delete-file patch-file))
    (git-status-diff--redisplay git-status--root
                                git-status-diff--section
                                git-status-diff--paths
                                git-status-diff--status-buffer)
    (when (buffer-live-p git-status-diff--status-buffer)
      (with-current-buffer git-status-diff--status-buffer
        (git-status-refresh)))
    ;; Land on the hunk before the one just applied, or on the hunk that
    ;; took its place when it was the first: at least one `diff-hunk-next'
    ;; from point-min, so point is never left on the file header.
    (if (eq (point-min) (point-max))
        (message "No changes left on this side of the index")
      (dotimes (_ (max 1 hunk-ordinal))
        (ignore-errors (diff-hunk-next))))))

(defun git-status-stage-hunk ()
  "Stage just the hunk at point, leaving the working tree untouched."
  (interactive)
  (unless (eq git-status-diff--section 'unstaged)
    (user-error "This diff is of the index; `-' unstages a hunk from it"))
  (git-status-diff--apply-hunk-to-index nil))

(defun git-status-unstage-hunk ()
  "Unstage just the hunk at point, leaving the working tree untouched."
  (interactive)
  (unless (eq git-status-diff--section 'staged)
    (user-error "This diff is of the working tree; `+' stages a hunk from it"))
  (git-status-diff--apply-hunk-to-index t))

(defun git-status-visit-file ()
  "Visit the file on this line."
  (interactive)
  (let ((entry (get-text-property (line-beginning-position) 'git-status-entry)))
    (unless entry
      (user-error "No file on this line"))
    (find-file (expand-file-name (git-status-entry-path entry) git-status--root))))


;;; Talking to the remote

(defun git-status--run-git-asynchronously (arguments)
  "Run git with ARGUMENTS in the background, collecting output in a buffer.
Push and pull reach the network: run synchronously they would block Emacs
for as long as the remote takes, and hang outright on a credential
prompt.  Refreshes the status buffer when the command finishes, so that
the ahead/behind counts in the header catch up."
  (let* ((root git-status--root)
         (status-buffer (current-buffer))
         (buffer (get-buffer-create (format "*git-status %s*" (car arguments)))))
    (with-current-buffer buffer
      (special-mode)
      (setq default-directory root)
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (format "cd %s\ngit %s\n\n" root (string-join arguments " ")))))
    (display-buffer buffer)
    (let ((process (let ((default-directory root)
                         ;; A pipe rather than a pty: over a pty git decides it
                         ;; is talking to a terminal and writes progress bars
                         ;; full of carriage returns, and a credential prompt
                         ;; would hang on input nobody can type.  Over a pipe it
                         ;; prints only the result, and fails fast instead.
                         (process-connection-type nil))
                     (apply #'start-file-process (format "git-%s" (car arguments))
                            buffer "git" arguments))))
      (set-process-query-on-exit-flag process t)
      ;; `special-mode' left the buffer read-only, so the filter has to
      ;; lift that itself; the default filter would just fail.
      (set-process-filter
       process
       (lambda (process output)
         (when (buffer-live-p (process-buffer process))
           (with-current-buffer (process-buffer process)
             (let ((inhibit-read-only t))
               (goto-char (point-max))
               (insert output))))))
      (set-process-sentinel
       process
       (lambda (process event)
         (when (memq (process-status process) '(exit signal))
           (when (buffer-live-p (process-buffer process))
             (with-current-buffer (process-buffer process)
               (let ((inhibit-read-only t))
                 (goto-char (point-max))
                 (insert (format "\ngit %s %s" (car arguments) (string-trim event))))))
           (if (eq (process-exit-status process) 0)
               (message "git %s finished" (car arguments))
             (message "git %s failed, see %s"
                      (car arguments) (buffer-name (process-buffer process))))
           (when (buffer-live-p status-buffer)
             (with-current-buffer status-buffer
               (git-status-refresh)))))))))

(defun git-status-push (&optional edit)
  "Push the current branch, in the background.
With a prefix argument EDIT, edit the git command line before running it."
  (interactive "P")
  (git-status--run-git-asynchronously
   (if edit (split-string-and-unquote (read-string "git " "push ")) '("push"))))

(defun git-status-pull (&optional edit)
  "Pull the current branch, in the background.
With a prefix argument EDIT, edit the git command line before running it."
  (interactive "P")
  (git-status--run-git-asynchronously
   (if edit (split-string-and-unquote (read-string "git " "pull ")) '("pull"))))


;;; Committing

(defun git-status-commit ()
  "Write a commit message for the staged changes and commit them.
The `log-edit' header lines are honoured: \"Summary:\" becomes the first
line of the message, and \"Author:\", \"Date:\", \"Amend: yes\",
\"Sign-Off: yes\" and \"No-Verify: yes\" become the matching git options."
  (interactive)
  (let ((root git-status--root)
        (status-buffer (current-buffer)))
    (log-edit
     ;; `log-edit-done' runs this with `call-interactively', so it has to
     ;; be a command rather than a plain function.
     (lambda ()
       (interactive)
       (let* ((message-and-arguments
               (log-edit-extract-headers
                `(("Author" . "--author")
                  ("Date" . "--date")
                  ("Amend" . ,(lambda (value) (and (equal value "yes") (list "--amend"))))
                  ("Sign-Off" . ,(lambda (value) (and (equal value "yes") (list "--signoff"))))
                  ("No-Verify" . ,(lambda (value) (and (equal value "yes") (list "--no-verify")))))
                (buffer-string)))
              (message-file (make-temp-file "git-status-commit"))
              (log-buffer (current-buffer)))
         (write-region (car message-and-arguments) nil message-file nil 'silent)
         (unwind-protect
             (with-temp-buffer
               (let ((default-directory root))
                 (unless (eq 0 (apply #'process-file "git" nil t nil
                                      "commit" "-F" message-file
                                      (cdr message-and-arguments)))
                   (error "git commit: %s" (string-trim (buffer-string))))))
           (delete-file message-file))
         (kill-buffer log-buffer)
         (when (buffer-live-p status-buffer)
           (with-current-buffer status-buffer
             (git-status-refresh)))))
     t nil (get-buffer-create "*git-status-commit*"))
    (with-current-buffer "*git-status-commit*"
      (setq default-directory root)
      (setq git-status--root root)
      (keymap-local-set "C-c C-e" #'git-status-toggle-amend))))

(defun git-status-toggle-amend ()
  "Toggle amending the previous commit from within the commit buffer.
Turning it on seeds an otherwise empty buffer with the message being
amended, so that editing it in place is the common case."
  (interactive)
  (when (and (log-edit-toggle-header "Amend" "yes")
             (save-excursion (rfc822-goto-eoh) (skip-chars-forward " \t\n") (eobp)))
    (goto-char (point-max))
    (insert (string-trim-right (git-status--run-git "log" "-1" "--pretty=format:%B")))))


;;; Mode

(defvar-keymap git-status-mode-map
  :doc "Keymap for `git-status-mode'."
  "n"   #'git-status-next-file
  "p"   #'git-status-previous-file
  "m"   #'git-status-mark
  "u"   #'git-status-unmark
  "U"   #'git-status-unmark-all
  "t"   #'git-status-toggle-marks
  "s"   #'git-status-stage
  "r"   #'git-status-unstage
  "k"   #'git-status-discard
  "="   #'git-status-diff
  "c"   #'git-status-commit
  "P"   #'git-status-push
  "+"   #'git-status-pull
  "RET" #'git-status-visit-file
  "f"   #'git-status-visit-file)

(define-derived-mode git-status-mode special-mode "Git-Status"
  "Major mode for acting on the files reported by `git status'.

\\{git-status-mode-map}"
  :group 'git-status
  (setq-local revert-buffer-function (lambda (&rest _) (git-status-refresh)))
  (setq-local truncate-lines t)
  (hl-line-mode 1))

;;;###autoload
(defun git-status ()
  "Show the status of the current repository in a `git-status-mode' buffer."
  (interactive)
  (let* ((root (file-name-as-directory
                (string-trim
                 (with-temp-buffer
                   (unless (eq 0 (process-file "git" nil t nil "rev-parse" "--show-toplevel"))
                     (user-error "Not inside a git repository"))
                   (buffer-string)))))
         (buffer (get-buffer-create
                  (format "*git-status: %s*"
                          (file-name-nondirectory (directory-file-name root))))))
    (with-current-buffer buffer
      (git-status-mode)
      (setq default-directory root)
      (setq git-status--root root)
      (git-status-refresh))
    (pop-to-buffer-same-window buffer)))

(provide 'git-status)

;;; git-status.el ends here
