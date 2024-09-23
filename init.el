;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Joe's Emacs init.el
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Things to do or try
;;
;; - dumb-jump:a package which jumps from symbol to definition without
;;   being language specific. Though C-. does this OK.
;; - from https://www.youtube.com/watch?v=51eSeqcaikM
;;   - save place mode
;; - from C.Meier's config https://github.com/gigasquid/emacs-config
;;     (setq make-backup-files nil)
;;     (setq auto-save-default nil)
;; - make dired open in same buffer (on ENTER - hitting 'a' instead
;;   reuses the buffer)
;; - Shortcut for duplicate line? useful in C
;; - shortcuts for commenting. Especially I would like next-sexp comment
;;   #_ for Clojure
;; - M-o should be C-o maybe? C-o is insertline, never use it
;; - some shortcut about deleting all the whitespace - fixup-whitespace maybe
;;   or delete-horizontal-whitespace (M-\) or just-one-space (M-SPC, but
;;   that's spotlight on mac). None of these do exactly what I want, which is
;;   to make a block like this:
;;     (defn [hello] (expression number 1)
;;       (expression number 2))
;;   into this in a single keystroke.
;;     (defn [hello] (expression number 1) (expression number 2))
;;   actually, delete-indentation M-^ seems to do what I want.
;; - Multiple cursors https://github.com/magnars/multiple-cursors.el
;; - Misc stuff from Batsov https://github.com/bbatsov/emacs.d/blob/master/init.el
;; - avy - stoped using it for some reason, not sure why
;; - use mark-pops more?
;;   https://www.masteringemacs.org/article/fixing-mark-commands-transient-mark-mode
;; - https://www.masteringemacs.org/article/demystifying-emacs-window-manager
;;
;; Things I tried and didn't like
;;   (setq-default show-trailing-whitespace t)
;;   lsp mode for Clojure. Bit too much
;;   hippie-expand in place of dabbrev-expand - it took too many liberties.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(global-unset-key (kbd "<right>"))
(global-unset-key (kbd "<left>"))
(global-unset-key (kbd "<up>"))
(global-unset-key (kbd "<down>"))
(global-unset-key (kbd "<down-mouse-1>"))
(global-unset-key (kbd "<mouse-1>"))
(global-unset-key (kbd "<down-mouse-3>"))
(global-unset-key (kbd "<mouse-3>"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Basic editor functionality ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

(setq inhibit-startup-message t)
(setq initial-scratch-message nil)

;; fix temp file creation
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

;; Watches the files and reverts them when they are changed by another
;; process.  Useful for editing things in a shared folder (GDrive) or
;; for git pulls. (Instead of manually revert-buffer)
(global-auto-revert-mode 1)
;; Same idea for dired
(setq global-auto-revert-non-file-buffers t)

;; prefer spaces over tabs
(setq-default indent-tabs-mode nil)

(setq sentence-end-double-space nil)

(add-to-list 'initial-frame-alist '(fullscreen . maximized))
(split-window-horizontally)

;; (defun no-split-window ()
;;   (interactive)
;;   nil)

;; (setq split-window-preferred-function 'no-split-window)

(setq visible-bell 1)

;; Always linenumbers in programming modes, and _relative_ line
;; numbers
(add-hook 'prog-mode-hook 'display-line-numbers-mode)
(setq-default display-line-numbers-type 'relative)

;; programming visibility
(show-paren-mode +1)
(global-hl-line-mode +1)
(setq whitespace-line-column 120)
(setq whitespace-style '(face tabs empty trailing lines-tail))
(add-hook 'prog-mode-hook 'whitespace-mode)

;; Auto-completion in minibuffs - FIDO is great, no need for IVY any friends
;; https://www.masteringemacs.org/article/understanding-minibuffer-completion

(fido-vertical-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Visuals
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(let ((bg (face-attribute 'mode-line :background)))
  (set-face-attribute 'mode-line nil
                      :box (list :line-width 4 :color bg :style nil)))

(let ((bg (face-attribute 'mode-line-inactive :background)))
  (set-face-attribute 'mode-line-inactive nil
                      :box (list :line-width 4 :color bg :style nil)))

;;;;;;;;;;;;;;;;
;; Mode line
;;;;;;;;;;;;;;;;
;; Minimal mode line with just the file name and the status indicator

(setq-default mode-line-format
  '("%e" " " mode-line-buffer-identification "%* "))

;;;;;;;;;;;;;;;;
;; recent mode
;;;;;;;;;;;;;;;;

(recentf-mode 1)
(setq recentf-max-menu-items 25)
(setq recentf-max-saved-items 25)
;; I used to have this as a separate buffer which opened. But now I
;; just use a mini-buffer with FIDO, per here:
;; https://www.masteringemacs.org/article/find-files-faster-recent-files-package

;;(global-set-key "\C-x\ \C-r" 'recentf-open-files)

(global-set-key (kbd "C-x C-r") 'recentf-open-minibuff)

(defun recentf-open-minibuff ()
  "Use `ido-completing-read' to \\[find-file] a recent file"
  (interactive)
  (if (find-file (completing-read "Find recent file: " recentf-list))
      (message "Opening file...")
    (message "Aborting")))

;;;;;;;;;;;;;;;;;
;; mac
;;;;;;;;;;;;;;;;;

;;; Use command key for meta
(setq mac-option-key-is-meta nil
      mac-command-key-is-meta t
      mac-command-modifier 'meta
      mac-option-modifier 'none)

;;; adding brew llvm path

(add-to-list 'exec-path "/opt/homebrew/opt/llvm/bin")
(setenv "PATH" (format "%s:%s" "/opt/homebrew/opt/llvm/bin" (getenv "PATH")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; universal keybind changes ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; C-m should not be interpreted as RET
(define-key input-decode-map [?\C-m] [C-m])

;; Moves and kills
;; |             | C-         | M-        | C-M- (code) |
;; |:------------|:-----------|:----------|:------------|
;; | a/start     | line*      | buff      | defn*       |
;; | e/end       | line*      | buff      | defn*       |
;; | h/back      | word       | sent      | sexp        |
;; | l/fwd       | word       | sent      | sexp        |
;; | j/nxt       | line       | pg down   | down-in     |
;; | k/prv       | line       | pg up     | up-out      |
;; | ;           | recenter*  |           |             |
;; |:------------|:-----------|:----------|:------------|
;; | SPACE       | set mark*  |           | mark sexp*  |
;; |:------------|:-----------|:----------|:------------|
;; | w           | kill rgn*  | KR save*  | KR append*  |
;; | n/kill back | word       | sentence  | sexp        |
;; | m/kill fwd  | word       | sentence  | sexp        |
;; | p/line      | rest line  | back line | whole line  |
;; | y/yank      | yank last* | KR Cycle* |             |

(global-set-key (kbd "C-l") 'forward-word) ;; replaces recenter-top-bottom
(global-set-key (kbd "C-h") 'backward-word) ;; replaces help :(

(global-set-key (kbd "C-j") 'next-line) ;; replaces electric-newline-and-maybe-indent
(global-set-key (kbd "C-k") 'previous-line) ;; replaces kill line

(global-set-key (kbd "C-M-l") 'forward-sexp) ;; replaces reposition-window
(global-set-key (kbd "C-M-h") 'backward-sexp) ;; replace mark-defun

(global-set-key (kbd "C-M-k") 'backward-up-list) ;; also default-indent-new-line
(global-set-key (kbd "C-M-j") 'down-list) ;; replaces kill

(global-set-key (kbd "M-e") 'end-of-buffer) ;; replaces fwd sentence
(global-set-key (kbd "M-a") 'beginning-of-buffer) ;; replaces backward sentence

(global-set-key (kbd "M-l") 'forward-sentence) ;; replaces downcase-word
(global-set-key (kbd "M-h") 'backward-sentence) ;; replaces mark-paragraph

(global-set-key (kbd "M-j") 'scroll-up-command) ;; replaces default-indent-new-line
(global-set-key (kbd "M-k") 'scroll-down-command) ;; replaces kill sentence

(global-set-key (kbd "C-w") 'backward-kill-word) ;; replaces kill region
(global-set-key (kbd "C-n") 'backward-kill-word) ;; replaces next line
(global-set-key (kbd "<C-m>") 'kill-word)
(global-set-key (kbd "M-n") 'backward-kill-sentence)
(global-set-key (kbd "M-m") 'kill-sentence) ;; replaces first whitespace
(global-set-key (kbd "C-M-n") 'backward-kill-sexp) ;; replaces fwd list
(global-set-key (kbd "C-M-m") 'kill-sexp) ;; aka M-RET

(defun backward-kill-line ()
  (interactive)
  (kill-line 0))

(global-set-key (kbd "C-p") 'kill-line) ;; replaces prv line.

(global-set-key (kbd "M-p") 'backward-kill-line)
(global-set-key (kbd "C-M-p") 'kill-whole-line) ;; replaces prv line

(global-set-key (kbd "C-M-y") 'yank) ;; for maintaining tempo

;; other stuff

(global-set-key (kbd "M-o") 'other-window)
(global-set-key (kbd "C-d") 'delete-other-windows) ;; replaces transpose char

(global-set-key (kbd "C-f") 'project-find-file)
(global-set-key (kbd "C-b") 'switch-to-buffer) ; maybe project STB?

;; Use this for command, in place of M-x, avoiding the meta stretch.
(global-set-key (kbd "C-x <C-m>") 'execute-extended-command)

;; Rebind
(global-set-key (kbd "C-;") 'recenter-top-bottom)
(global-set-key (kbd "C-'") 'dabbrev-expand)
;(global-set-key (kbd "C-M-SPC") 'set-mark-command) ;for tempo

;(global-set-key (kbd "C-x v p") 'vc-pull)
;; to match C-x v P for push. + is generally pull

(global-set-key (kbd "C-+") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)

;; stop accidentally zooming
(global-set-key (kbd "C-<wheel-up>") nil)
(global-set-key (kbd "C-<wheel-down>") nil)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Package / Mode specific configuration ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(defvar my-packages
  '(which-key diff-hl))

(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p))
  (require p))

(which-key-mode)
(global-diff-hl-mode)

;;;;;;;;;;;;;;
;; markdown
;;;;;;;;;;;;;;

(setq markdown-fontify-code-blocks-natively t)
;(setq markdown-hide-markup t)
(setq markdown-max-image-size '(1500 . 1500))
(add-hook 'markdown-mode-hook 'visual-line-mode)
(add-hook 'markdown-mode-hook 'auto-fill-mode)

;;;;;;;;;;;;;;
;; Org mode
;;;;;;;;;;;;;;

(setq org-hide-emphasis-markers t)
(setq org-hide-leading-stars t)
(add-hook 'org-mode-hook 'visual-line-mode)
(add-hook 'org-mode-hook 'visual-fill-column-mode)

;;;;;;;;;;;;;;;;;;;;;;;
;; clojure (and elisp)
;;;;;;;;;;;;;;;;;;;;;;;

(require 'smartparens-config)

(defun sexp-bindings ()
  (progn
    (local-set-key (kbd "C-l") 'forward-sexp)
    (local-set-key (kbd "C-h") 'backward-sexp)
    (local-set-key (kbd "C-k") 'sp-end-of-sexp)
    (local-set-key (kbd "C-j") 'sp-beginning-of-sexp)

    (local-set-key (kbd "C-M-k") 'sp-down-sexp)
    (local-set-key (kbd "C-M-h") 'sp-backward-up-sexp)
    (local-set-key (kbd "C-M-j") 'sp-backward-down-sexp)
    (local-set-key (kbd "C-M-l") 'sp-up-sexp)))

;; other options for sexps to think about using smart-parens
;; - wrapping / unwrapping
;; - slurp / barf
;; - transpose
;; - kill

;(add-hook 'emacs-lisp-mode-hook #'smartparens-mode)
;(add-hook 'emacs-lisp-mode-hook 'sexp-bindings)
(add-hook 'emacs-lisp-mode-hook
	  (lambda () (electric-pair-local-mode)))

;(add-hook 'clojure-mode-hook #'smartparens-mode)
;(add-hook 'clojure-mode-hook 'sexp-bindings)
(add-hook 'clojure-mode-hook 'subword-mode)
(add-hook 'cider-mode-hook
	  (lambda () (local-set-key (kbd "C-c f") 'cider-format-defun)))

;; builtin cider commands:
;;   C-c C-p cider-pprint-eval-last-sexp
;;   C-c C-e cider-eval-last-sexp
;;   C-c C-c cider-eval-defun-at-point (defun = top level)
;;   C-c C-f cider-pprint-eval-defun-at-point

;;;;;;;;;;;;;;;;;;;;;;;
;; C
;;;;;;;;;;;;;;;;;;;;;;;

;; create a format file with
;; clang-format -style=llvm -dump-config > .clang-format
(setq clang-format-style "file")
(setq clang-format-fallback-style "llvm")

(add-hook 'c-mode-hook
          (lambda ()
            (progn
              (electric-pair-local-mode)
              (c-toggle-comment-style -1)
              (local-set-key (kbd "C-M-h") 'backward-sexp)
              (local-set-key (kbd "C-c C-c") 'recompile)
              (local-set-key (kbd "C-c f") 'clang-format-buffer)
              (local-set-key (kbd "C-d") 'delete-other-windows))))

; best way to hook LSP up properly is to use bear
; (https://github.com/rizsotto/Bear) to generate a
; compile_commands.json file (just run `bear -- make`) clangd will
; pick up on that automatically then just `eglot` to launch the lsp

;;;;;;;;;;;;;;;;;;;;;;;;
;; Common lisp and Slime
;;;;;;;;;;;;;;;;;;;;;;;;

(load (expand-file-name "~/.quicklisp/slime-helper.el"))
(setq inferior-lisp-program "sbcl")

;;;;;;;;;;;;;;;;;;;;;;;;
;; Ag
;;;;;;;;;;;;;;;;;;;;;;;;

(setq ag-reuse-buffers 't)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; automatically generated config
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq custom-file (concat user-emacs-directory "custom.el"))
(load custom-file 'noerror)

(put 'dired-find-alternate-file 'disabled nil)
