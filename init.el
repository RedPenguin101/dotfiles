;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Joe's Emacs init.el
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Things to do
;; ============
;; - Look at having a 'repeat' function for modal leaders, so when
;;   you SPC-<x> <x> it does the SPC-<x> command twice.
;; - surround next sexp with brackets functions / shortcuts
;; - eat terminal? https://codeberg.org/akib/emacs-eat
;;
;; Interesting sounding stuff from
;; https://github.com/jamescherti/minimal-emacs.d/blob/main/init.el
;; (setq custom-buffer-done-kill t)
;; (setq uniquify-buffer-name-style 'forward)
;; (setq comment-multi-line t)
;; (setq comment-empty-lines t)
;;
;; mode-line-collapse-minor-modes coming in emacs 31 will allow you to hide lighters for particular minor modes
;; https://www.reddit.com/r/emacs/comments/1k7zxjv/fyi_modelinecollapseminormodes/
;;
;; Look into Shift Selection - what is it?
;;
;; Stuff I usually forget
;; ======================
;;
;; Shell
;; -----
;; M-n, M-p: cycle command history
;; M-r: History search
;;
;; Ansi Term
;; ---------
;; C-cj Buffer mode, C-ck Terminal Mode
;;
;; dired
;; -----
;; - i : include subdir
;; - F : open all marked
;; - `find-name-dired' for to pipe searched files to buffer
;;
;; Other stuff
;; -----------
;; align-regexp RET =    - line up multiline definition
;;
;; Maybe add back keybinds
;; =======================
;; - register set/jump. was on vSPC/vj (C-x r SPC, C-x r j)
;; - whitespace cleanup
;; - Occur (M-s o) (maybe, this isn't so bad)
;;
;; In the terminal
;; ===============
;; The following keybinds have problems when being used in terminals
;; C-v (or M-v on Mac) paste
;; C-/ and M-/
;; M-BACKSPACE
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Basic editor functionality ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setopt inhibit-splash-screen t)

;; This is required on older versions of emacs because (according to
;; magit error messages) "Due to bad defaults, Emac's package manager
;; refuses to update ... build-in [sic] packages..."
(setopt package-install-upgrade-built-int t)

;; Remove noise at startup
(setopt initial-scratch-message nil)
;; No beeping or blinking
(setopt visible-bell nil)
(setopt ring-bell-function #'ignore)

;; Backups, lockfiles and trash
(setopt create-lockfiles nil)
(setopt make-backup-files nil)
(setopt backup-inhibited t)
(setopt delete-by-moving-to-trash t)

;; Autoreverts
(setopt auto-revert-avoid-polling t)
(setopt auto-revert-interval 5)
(setopt auto-revert-check-vc-info t)
(setopt global-auto-revert-non-file-buffers t) ; for dired
(setopt auto-revert-verbose nil) ;; don't message me
(global-auto-revert-mode 1)

;; line numbers display
(setq-default display-line-numbers-type 'relative)
(global-display-line-numbers-mode 1)

;; start fullscreen
(add-to-list 'initial-frame-alist '(fullscreen . maximized))

;; Editing preferences
(setq delete-selection-mode 1)
(setq kill-do-not-save-duplicates t) ;; doesn't duplicate things in the kill ring

;; Whitespace and spaces
(setq whitespace-line-column 120)
(setq whitespace-style '(face tabs empty trailing lines-tail))
(add-hook 'prog-mode-hook 'whitespace-mode)
(add-hook 'text-mode-hook 'whitespace-mode)
(setq sentence-end-double-space nil)
(setopt indicate-buffer-boundaries 'left)  ; Show buffer top and bottom in the margin

(setq-default truncate-lines t)

;; `hl-line-mode' highlights the currently selected line
;; Restrict `hl-line-mode' highlighting to the current window, reducing visual
;; clutter and slightly improving `hl-line-mode' performance.
(setq hl-line-sticky-flag nil)
(setq global-hl-line-sticky-flag nil)
(global-hl-line-mode +1)

;; FIDO/Minibuffer
;; https://www.masteringemacs.org/article/understanding-minibuffer-completion
(fido-vertical-mode)

;; Keep the cursor out of the read-only portions of the.minibuffer
(setq minibuffer-prompt-properties
      '(read-only t intangible t cursor-intangible t face minibuffer-prompt))

(add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

;; Savehist
(setq savehist-additional-variables
      '(kill-ring                            ; clipboard
        register-alist                       ; macros
        mark-ring global-mark-ring           ; marks
        search-ring regexp-search-ring))     ; searches
(savehist-mode)
(setq save-place-file (expand-file-name "saveplace" user-emacs-directory))
(setq save-place-limit 600)
(save-place-mode 1)

(setq read-answer-short t) ;; always accepts 'y' instead of 'yes'
(setq use-short-answers t)

;; Can use C-u C-SPC C-SPC C-SPC... instead of C-u C-SPC C-u C-SPC...
;; (or SPC-. t t t t..)
(setq set-mark-command-repeat-pop t)

(transient-mark-mode t)

;; Show current project name and menu in modeline
(when (>= emacs-major-version 30)
  (setopt project-mode-line t))

;; Reduce rendering/line scan work by not rendering cursors or regions in
;; non-focused windows.

(setq-default cursor-in-non-selected-windows nil)
(setq highlight-nonselected-windows nil)

(setq-default left-fringe-width  8)
(setq-default right-fringe-width 8)

;; Disable visual indicators in the fringe for buffer boundaries and empty lines
(setq-default indicate-buffer-boundaries nil)
(setq-default indicate-empty-lines nil)

;; Eliminate delay before highlighting search matches
(setq lazy-highlight-initial-delay 0)

;;;;;;;;;;;;;;;;;
;; mac
;;;;;;;;;;;;;;;;;

;;; Use command key for meta
(when (eq system-type 'darwin)
  (setq mac-option-key-is-meta nil
        mac-command-key-is-meta t
        mac-command-modifier 'meta
        mac-option-modifier 'none)
  ;;; adding brew llvm path
  (add-to-list 'exec-path "/opt/homebrew/opt/llvm/bin")
  (setenv "PATH" (format "%s:%s" "/opt/homebrew/opt/llvm/bin" (getenv "PATH")))
  (setq ispell-program-name "/opt/homebrew/bin/aspell"))

;;;;;;;;;;;
;; dired ;;
;;;;;;;;;;;
;; How to actually use dired to manage files
;; - use wdired to rename stuff (including rect mode)
;; - for mass delete, copy, open use marks
;; - use marks, including `* %` to mark by regex and t to invert
;; - F to open all marked files
;; - i when on a dir to put the content of the dir into the same dired buffer.
;; - collapse the subdir with $
;; - use `find-name-dired' to build a dired buffer with all files that fit a pattern
;;
;; dired-collapse
;; dired-dwim-target

(put 'dired-find-alternate-file 'disabled nil)
(setq dired-kill-when-opening-new-dired-buffer t)

;; if the option dired-dwim-target is non-nil, and if there is another
;; Dired buffer displayed in some window, that other buffer’s
;; directory is suggested instead.
(setq dired-dwim-target 1)

(require 'dired-x)

;; omit mode exludes noise like . and ..
(add-hook 'dired-mode-hook 'dired-omit-mode)
(add-hook 'dired-mode-hook 'dired-hide-details-mode)
(setq dired-omit-verbose nil)

(use-package wdired
  :ensure nil
  :commands (wdired-change-to-wdired-mode)
  :config
  (setq wdired-allow-to-change-permissions t)
  (setq wdired-create-parent-directories t))

;;;;;;;;;;;;;;;;
;; recent mode
;;;;;;;;;;;;;;;;

(recentf-mode 1)
(setq recentf-max-menu-items 300)
(setq recentf-max-saved-items 15)
;; I used to have this as a separate buffer which opened. But now I
;; just use a mini-buffer with FIDO, per here:
;; https://www.masteringemacs.org/article/find-files-faster-recent-files-package

(defun recentf-open-minibuff ()
  "Use `ido-completing-read' to \\[find-file] a recent file"
  (interactive)
  (if (find-file (completing-read "Find recent file: " recentf-list))
      (message "Opening file...")
    (message "Aborting")))

;;;;;;;;;;;;;;;;;;;;;;;;;
;; General programming ;;
;;;;;;;;;;;;;;;;;;;;;;;;;

;; Toggle visualization of matching parens. Matching parenthesis is
;; highlighted in ‘show-paren-style’ after ‘show-paren-delay’ seconds
;; of Emacs idle time.
(show-paren-mode +1)

;; When enabled, typing an open parenthesis automatically inserts the
;; corresponding closing parenthesis, and vice versa. If the region is
;; active, the parentheses (brackets, etc.) are inserted around the
;; region instead.
;; (electric-pair-mode)

;; In subword mode, FooBar, foo_bar, foo-bar, are all 2 words
(global-subword-mode 1)

;; which-function-mode shows you which function you're in in the
;; modeline. Useful when you have massive function, which I do a lot
;; when game programming.
(which-function-mode 1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; TAB: Indentation and Autocomplete ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setopt completion-cycle-threshold 1)                  ; TAB cycles candidates
(setopt completions-detailed t)

;; Indentation can insert tabs if this is non-nil.
(setq-default indent-tabs-mode t)

(setq-default tab-width 4)

;; Controls the operation of the TAB key. If ‘complete’: indent if not
;; indented, complete if already indented
(setq tab-always-indent 'complete)

;; Configure automatic indentation to be triggered exclusively by newline and
;; DEL (backspace) characters.
(setq-default electric-indent-chars '(?\n ?\^?))

;; tab-first-completion governs the behavior of TAB completion on the first press of the key.
;; - nil: complete.
;; - ‘eol’: only complete if point is at the end of a line.
;; - ‘word’ ‘word-or-paren’ ‘word-or-paren-or-punct’ complete unless the next character has word syntax
;;   (according to ‘syntax-after’) / is paren / is punctuation
;; Typing TAB a second time always results in completion.
;; has no effect unless ‘tab-always-indent’ is ‘complete’.

(setq tab-first-completion 'word)

;; This stuff is taken from Bedrock
(setopt completion-auto-help 'always)                  ; Open completion always; `lazy' another option
(setopt completions-max-height 20)                     ; This is arbitrary
(setopt completions-format 'one-column)
(setopt completions-group t)
(setopt completion-auto-select 'second-tab)            ; Much more eager
(keymap-set minibuffer-mode-map "TAB" 'minibuffer-complete) ; TAB acts more like how it does in the shell

;;;;;;;;;;;;;;;;
;; compilation
;;;;;;;;;;;;;;;;
;; Taken from emacs-solo

(use-package compile
  :ensure nil
  :custom
  (compilation-ask-about-save nil)
  (compilation-scroll-output 'first-error)
  (compilation-always-kill t)
  (compilation-scroll-output t)
  (ansi-color-for-compilation-mode t)
  :config
  (add-hook 'compilation-filter-hook #'ansi-color-compilation-filter))

;;;;;;;;;;;;;;;;;
;; Imenu buffer
;;;;;;;;;;;;;;;;;
;; The Imenu facility offers a way to find the major definitions in a
;; file by name. In programming-language modes the definitions are
;; variables and functions, and in text modes, they are chapter,
;; section, etc.

;; `imenu' command displays the list of matching valid names in the
;; minibuffer.

;; The index can be hierarchical. if you set `imenu-flatten' to a
;; non-nil value (prefix, annotation, group) the minibuffer list will
;; be flattened.

;; When you change the contents of a buffer, if you add or delete
;; definitions, you can update the buffer’s index based on the new
;; contents by invoking the ‘*Rescan*’ item in the menu. Rescanning
;; happens automatically if you set `imenu-auto-rescan' to a non-nil
;; value. There is no need to rescan because of small changes in the
;; text.

(setopt imenu-auto-rescan t)

;; Function to pipe imenu results to a full buffer as opposed to
;; displaying them in a mini buffer

(defun my/imenu-to-compilation-buffer ()
  (interactive)

  (let* ((index (imenu--make-index-alist))
         (source-buffer (current-buffer))
         (buf (get-buffer-create "*Imenu*"))
         (lines '()))

    (cl-labels
        ((flatten-imenu (alist)
           (seq-mapcat (lambda (item)
                         (cond ((and (stringp (car item)) (string-prefix-p "*" (car item))) nil)

                               ((imenu--subalist-p item) (flatten-imenu (cdr item)))

                               ((consp item) (list item))

                               (t nil)))
                       alist)))
      (let ((items (flatten-imenu index)))
        (dolist (item items)
          (let ((pos (cdr item)))
            (when (and (markerp pos) (eq (marker-buffer pos) source-buffer))
              (let ((line (line-number-at-pos pos)))
                (push (format "%s:%d: %s"
                              (buffer-file-name source-buffer)
                              line
                              (car item))
                      lines)))))
        (with-current-buffer buf
          (let ((inhibit-read-only t))
            (erase-buffer)
            (insert (format "- Imenu index from: %s\n\n" (buffer-name source-buffer)))
            (insert (mapconcat #'identity (nreverse lines) "\n"))
            (insert "\n")  )
          (compilation-mode))))

    (display-buffer buf)
    (goto-char (point-min))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; non-native / external packages ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'package)

(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

(use-package diff-hl
  :config
  (diff-hl-margin-mode))

(use-package magit)
(use-package ag)
(use-package visible-mark)

(use-package cape
  ;; "Completion At Point Extensions". out of the box
  ;; completion-at-point is pretty useless without a tags table or
  ;; lsp. cape adds the ability to use dabbrev as a
  ;; completion-at-point function. Also other stuff, but this is fine
  ;; for me.
  :init
  (add-hook 'completion-at-point-functions #'cape-dabbrev))

(use-package dimmer
  ;; dims non-active windows
  ;; config is from https://www.gnu.org/software/emacs/manual/html_node/modus-themes/Note-on-dimmerel.html
  :config
  (setq dimmer-fraction 0.3)
  (setq dimmer-adjustment-mode :foreground)
  (setq dimmer-use-colorspace :rgb)
  (dimmer-mode 1))

;; Additional cool packages not included, but which I use and like
;; (excluding language specific ones defined later)
;;
;; - aggressive indent (sometimes)
;; - csv-mode
;; - markdown-mode
;; - math-preview (view TeX)

;;;;;;;;;;;;;;;;
;; Bad Habits ;;
;;;;;;;;;;;;;;;;

(require 'keyfreq)
(keyfreq-mode 1)
(keyfreq-autosave-mode 1)

(setq keyfreq-excluded-commands
      '(self-insert-command
		disable-mouse--handle
		modal-mode--command-mode-init
		modal-mode--insert-mode-init))

(require 'disable-mouse)
(global-disable-mouse-mode)

(defun shame () (interactive) (message "Shame!"))
(global-set-key (kbd "<left>") #'shame)
(global-set-key (kbd "<right>") #'shame)
(global-set-key (kbd "<up>") #'shame)
(global-set-key (kbd "<down>") #'shame)

;;;;;;;;;;;;;;;;;;;;;;
;; Custom functions ;;
;;;;;;;;;;;;;;;;;;;;;;

(defun scroll-down-half-page ()
  (interactive)
  (let ((ln (line-number-at-pos (point)))
    (lmax (line-number-at-pos (point-max))))
    (cond ((= ln 1) (move-to-window-line nil))
      ((= ln lmax) (recenter (window-end)))
      (t (progn
           (move-to-window-line -1)
           (recenter))))))

(defun scroll-up-half-page ()
  (interactive)
  (let ((ln (line-number-at-pos (point)))
    (lmax (line-number-at-pos (point-max))))
    (cond ((= ln 1) nil)
      ((= ln lmax) (move-to-window-line nil))
      (t (progn
           (move-to-window-line 0)
           (recenter))))))

(defun kill-inner-word ()
  "Kills the entire word your cursor is in. Equivalent to ciw in vim."
  (interactive)
  (forward-char 1)
  (backward-word)
  (kill-word 1))

(defun kill-inner-sexp ()
  (interactive)
  (forward-char 1)
  (backward-sexp)
  (kill-sexp 1))

(defun backward-down-list (&optional arg)
  (interactive)
  (or arg (setq arg 1))
  (down-list (- arg)))

(defun start-of-sexp ()
  (interactive)
  (backward-up-list)
  (down-list))

(defun my--end-of-sexp ()
  (interactive)
  (up-list)
  (backward-down-list))

(defun next-line-required-arg (&optional arg)
  (interactive "P")
  (if (null arg) (message "arg required") (next-line arg)))

(defun previous-line-required-arg (&optional arg)
  (interactive "P")
  (if (null arg) (message "arg required") (previous-line arg)))

(defun surround-sexp ()
  (interactive)
  (set-mark (point))
  (forward-sexp)
  (insert-char ?\) )
  (exchange-point-and-mark)
  (insert-char ?\( ))

(defun kill-in-sexp ()
  ;; equivalent to ci( in vi
  (interactive)
  (start-of-sexp)
  (set-mark (point))
  (my--end-of-sexp)
  (kill-region nil nil t))

;;;;;;;;;;;;;;;;;;;;
;; Modal keybinds ;;
;;;;;;;;;;;;;;;;;;;;

;; default scroll-up/downs are a bit much. These are a bit less
;; jarring

(global-set-key (kbd "C-v") 'scroll-down-half-page) ;; replace scroll-up-command
(global-set-key (kbd "M-v") 'scroll-up-half-page) ;; replace scroll-down-command

(load "~/.emacs.d/lisp/modal.el")

(add-hook 'prog-mode-hook 'modal-mode)
(add-hook 'text-mode-hook 'modal-mode)
(add-hook 'conf-mode-hook 'modal-mode)

(define-modal-command-keys
 '(;; LEFT HAND
   ;; s: SEARCH LEADER
   ("d" . down-list)                ;; C-M-d
   ("D" . backward-down-list)
   ("u" . backward-up-list)
   ("U" . up-list)

   ("n" . scroll-down-half-page)
   ("p" . scroll-up-half-page)

   ("w" . delete-other-windows)
   ("x" . execute-extended-command)

   ("l" . recenter-top-bottom)      ;; C-l
   ("L" . move-to-window-line-top-bottom) ;; M-r
   (";" . comment-line)

   ("i" . modal-mode--insert-mode-init)
   ("o" . insert-overwrite)

   ("/" . undo)                     ;; C-/
   ))

(define-modal-leader-keys
 '(
   ("f" . find-file)                ;; C-x C-f
   ("s" . save-buffer)              ;; C-x C-s
   ("d" . dired-jump)               ;; C-x C-d (sort of)
   ("r" . recentf-open-minibuff)
   ("b" . switch-to-buffer)         ;; C-x b

   ("[" . kmacro-start-macro)       ;; C-x (
   ("]" . kmacro-end-macro)         ;; C-x )
   ("m" . kmacro-call-macro)        ;; none, weirdly
   ))

(define-modal-kill-keys
 '(
   ("m" . kill-in-sexp)
   ("n" . kill-inner-sexp)
   ("l" . kill-whole-line)          ;; C-S-<backspace>
   ("i" . kill-inner-word)
   ))

(define-modal-search-keys
 '(
   ("a" . ag-project)
   ("q" . query-replace)            ;; M-%
   ("h" . highlight-phrase)
   ))

;; I don't use these any more
(define-modal-project-keys '()) ;; just use C-x p
(define-modal-eval-keys '())

;; globals
(global-set-key (kbd "M-o") 'other-window)
(global-set-key (kbd "C-=") 'text-scale-increase)
;; decrease is increase with negative arg. C-- C-=

;; Keys I always hit accidentally
(global-unset-key (kbd "C-<wheel-up>")) ;; stop zooming by mistake
(global-unset-key (kbd "C-<wheel-down>"))

(global-unset-key (kbd "C-z")) ;; suspend
(global-unset-key (kbd "C-x C-z")) ;; suspend
(global-unset-key (kbd "C-x f")) ;; col fill

;;;;;;;;;;;;;;;;;;;;;;;
;; clojure (and elisp)
;;;;;;;;;;;;;;;;;;;;;;;

;; https://www.youtube.com/watch?v=KMWLIgG986I
;; https://cider.mx
;; https://www.cognitect.com/blog/2013/06/04/clojure-workflow-reloaded

(setq clojure-toplevel-inside-comment-form t)

;; builtin cider commands:
;;   C-c C-x j j: cider-jack in
;;   C-c M-j:     cider-jack in
;;   C-c C-q:     cider-quit
;;   C-c C-p:     cider-pprint-eval-last-sexp
;;   C-c C-e:     cider-eval-last-sexp
;;   C-c C-c:     cider-eval-defun-at-point (defun = top level)
;;   C-M-x:       cider-eval-defun-at-point
;;   C-c C-f:     cider-pprint-eval-defun-at-point
;;   C-c M-n r:   cider-ns-refresh and reload all modified Clojure files on the classpath.

;; some options
;; cider-docstring-max-lines (default 20)
;; enlighten-mode: show local values inline

;; Debugging
;; https://docs.cider.mx/cider/debugging/debugger.html
;; cider-eval-defun with prefix arg: instrument for debug
;; (run ced without prefix arg to remove debug)
;; or, put `#dbg' in front of the form you want to instument
;; cider-browse-instrumented-defs to see what's currently instrumented
;;

(add-hook 'cider--debug-mode-hook (lambda () (modal-mode -1)))

;; This is a (I think) quite hacky way of abusing `advice-add' to add
;; an interceptor around the symbol for cider--debug-mode.

(defvar cider-debug-mode-exit-hook nil)

(defun cider-debug-toggle-advice-hack (orig-fn &optional arg)
  (let ((was-on cider--debug-mode))
    (funcall orig-fn arg)
    (when (and was-on (not cider--debug-mode))
      (run-hooks 'cider-debug-mode-exit-hook))))

(advice-add 'cider--debug-mode :around #'cider-debug-toggle-advice-hack)

(add-hook 'cider-debug-mode-exit-hook (lambda () (modal-mode 1)))

;; Hitting a breakpoint will drop into the cider debugger, which has the following commands
;; n: step
;; i: step into
;; o: step out of (list)
;; h: step to cursor position
;; e: eval
;; p: inspect the current symbol
;; l: inspect locals
;; L: toggle locals display
;; s: show call stack
;; q: quit

;; conditional breakpoints:
;; (dotimes [i 10]
;;   #dbg ^{:break/when (= i 7)}
;;   (prn i))

;; inspector: cider-inspect, cider-inspect-last-result
;; n/p: navigate inspector window
;; RET: drill down
;; l:   up / pop
;; g:   refresh
;; SPC: next page (M-SPC: prv)
;; v: view toggle :normal, :table, :object
;; P: toggle pprint
;; d: def inspector value in repl

;;;;;;;;;;;;;;;;;;;;;;;
;; C
;;;;;;;;;;;;;;;;;;;;;;;

(load "~/.emacs.d/lisp/simple-c-mode.el")

;; create a format file with
;; clang-format -style=llvm -dump-config > .clang-format
(setq clang-format-style "file")
(setq clang-format-fallback-style "llvm")

(add-hook 'simple-c-mode-hook
          (lambda ()
            (progn
              (local-set-key (kbd "C-c C-c") 'recompile))))

; best way to hook LSP up properly is to use bear
; (https://github.com/rizsotto/Bear) to generate a
; compile_commands.json file (just run `bear -- make`) clangd will
; pick up on that automatically then just `eglot` to launch the lsp

;;;;;;;;;;;;;;;;;;;;;;;;
;; Odin
;;;;;;;;;;;;;;;;;;;;;;;;

;; There's a package here https://git.sr.ht/~mgmarlow/odin-mode
;;; install with M-x package-vc-install RET https://git.sr.ht/~mgmarlow/odin-mode
;; But it uses a crappy js indenter. Mine is basically the same but
;; with a much simpler indentation routine.

(load "~/.emacs.d/lisp/odin-mode.el")

(add-hook 'odin-mode-hook
          (lambda ()
            (progn
              (local-set-key (kbd "C-c C-c") 'recompile))))

(add-hook 'odin-mode-hook
          (lambda ()
            (push '("<=" . ?≤) prettify-symbols-alist)
            (push '(">=" . ?≥) prettify-symbols-alist)
			(push '("->" . ?→) prettify-symbols-alist)
			(push '("!=" . ?≠) prettify-symbols-alist)
			(push '(":=" . ?≔) prettify-symbols-alist)

			(prettify-symbols-mode 1)))

(with-eval-after-load 'eglot
  (add-to-list 'eglot-server-programs
               '(odin-mode . ("ols"))))

;; These allow you to jump from the compilation error to the
;; associated line where the error occured.

(add-to-list 'compilation-error-regexp-alist 'odin-error)

(add-to-list 'compilation-error-regexp-alist-alist
             '(odin-error "^\\(/.*\\.odin\\)(\\([0-9]+\\):\\([0-9]+\\))" 1 2 3))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; automatically generated config
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq custom-file (concat user-emacs-directory "custom.el"))
(load custom-file 'noerror 'nomessage)
