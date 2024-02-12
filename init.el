;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Joe's Emacs init.el
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Things to do or try
;;
;; - dumb-jump:a package which jumps from symbol to definition without
;;   being language specific. Though C-. does this OK.
;; - LSP
;; - from https://www.youtube.com/watch?v=51eSeqcaikM
;;   - save hist mode: a history for minibuffs. Lighter weight than Ivy
;;   - save place mode
;;   - custom vars file location
;; - from C.Meier's config https://github.com/gigasquid/emacs-config
;;     (setq make-backup-files nil)
;;     (setq auto-save-default nil)
;; - change sexp bindings: up should be up-and-in, down should be
;;   down-and-out
;; - make dired open in same buffer (on ENTER - hitting 'a' instead
;;   reuses the buffer)
;; - Shortcut for kill line
;; - Smartparens and change up sexp navigation
;; - Shortcut for C-x 1 (full screen) I use this all the time.
;; - maybe get rid of recursive minibuffers? I don't really understand
;;   what they do TBH.
;; - shortcuts for commenting. Especially I would like next-sexp comment
;;   #_ for Clojure
;; - Think of how to phase out meta from flow. Maybe like C-; or C-'
;;   (i.e. double-pinky) could be a C-M replace
;; - maybe kill and copy should be done with modifiers.
;;   like if C-l is next word, C-u C-l is kill next word
;;   C-u C-u C-l is copy next word. This would break the pattern of
;;   C-u <thing> being do thing 4 times, which probably disqualifies
;;   this idea.
;;   Maybe C-p C-l for delete word. or C-' C-l
;; - M-o should be C-o maybe?
;; - some shortcut about deleting all the whitespace - fixup-whitespace maybe
;;   or delete-horizontal-whitespace (M-\) or just-one-space (M-SPC, but
;;   that's spotlight on mac). None of these do exactly what I want, which is
;;   to make a block like this:
;;     (defn [hello] (expression number 1)
;;       (expression number 2))
;;   into this in a single keystroke. 
;;     (defn [hello] (expression number 1) (expression number 2))
;;   actually, delete-indentation M-^ seems to do what I want.
;;
;;
;; Things I tried and didn't like
;;   (setq-default show-trailing-whitespace t)
;;   lsp mode for Clojure. Bit too much
;;   hippie-expand in place of dabbrev-expand - it took too many liberties.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Package installation and initialization
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

(defvar my-packages
  '(markdown-mode
    org
    ivy ivy-prescient
    beacon
    which-key
    diff-hl))

(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p))
  (require p))

(ivy-mode)
(ivy-prescient-mode)
(which-key-mode)
(beacon-mode 1)
(global-diff-hl-mode)
(setq inhibit-startup-message t) 
(setq initial-scratch-message nil)

;; fix temp file creation
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

;; dired-x enables `dired-do-find-marked-files` with `F`, which opens
;; all marked files in new windows
(require 'dired-x)

;; Watches the files and reverts them when they are changed by another
;; process.  Useful for editing things in a shared folder (GDrive) or
;; for git pulls. (Instead of manually revert-buffer)
(global-auto-revert-mode 1)
;; Same idea for dired
(setq global-auto-revert-non-file-buffers t)

(setq enable-recursive-minibuffers t)

;; prefer spaces over tabs
(setq-default indent-tabs-mode nil)

(setq sentence-end-double-space nil)

(add-to-list 'initial-frame-alist '(fullscreen . maximized))

(setq visible-bell 1)

;;;;;;;;;;;;;;;;
;; Mode line
;;;;;;;;;;;;;;;;

(setq-default mode-line-format
  '("%e" " " mode-line-buffer-identification "%* "))

;;;;;;;;;;;;;;;;
;; recent mode
;;;;;;;;;;;;;;;;

(recentf-mode 1)
(setq recentf-max-menu-items 25)
(setq recentf-max-saved-items 25)
(global-set-key "\C-x\ \C-r" 'recentf-open-files)

;;;;;;;;;;;;;;;;;
;; mac
;;;;;;;;;;;;;;;;;

;;; Use command key for meta
(setq mac-option-key-is-meta nil
      mac-command-key-is-meta t
      mac-command-modifier 'meta
      mac-option-modifier 'none)

;;;;;;;;;;;;;;;;;
;; keybinds
;;;;;;;;;;;;;;;;;

;; Changes to basic moves
;;       H/L    K/J
;; C     word   line
;; CM    sent   para
;; M     page   buffer

(global-set-key (kbd "C-l") 'forward-word) ;; replaces recenter-top-bottom
(global-set-key (kbd "C-h") 'backward-word) ;; replaces help :(

(global-set-key (kbd "C-j") 'next-line) ;; replaces electric-newline-and-maybe-indent
(global-set-key (kbd "C-k") 'previous-line) ;; replaces kill line

(global-set-key (kbd "C-M-l") 'forward-sentence) ;; aka page down replaces reposition-window
(global-set-key (kbd "C-M-h") 'backward-sentence) ;; replace mark-defun

(global-set-key (kbd "C-M-j") 'forward-paragraph) ;; also default-indent-new-line
(global-set-key (kbd "C-M-k") 'backward-paragraph) ;; replaces kill

(global-set-key (kbd "M-l") 'end-of-buffer) ;; replaces downcase-word
(global-set-key (kbd "M-h") 'beginning-of-buffer) ;; replaces mark-paragraph

(global-set-key (kbd "M-j") 'scroll-up-command) ;;replaces default-indent-new-line
(global-set-key (kbd "M-k") 'scroll-down-command) ;; replaces kill sentence

;; kill rebinds
;; p=cut, n=copy

(global-set-key (kbd "C-w") 'backward-kill-word) ;; replaces kill-region

(global-set-key (kbd "C-p") 'kill-region)
(global-set-key (kbd "C-M-p") 'kill-region)
(global-set-key (kbd "M-p") 'kill-region)

(global-set-key (kbd "C-n") 'kill-ring-save)
(global-set-key (kbd "C-M-n") 'kill-ring-save)
(global-set-key (kbd "M-n") 'kill-ring-save)

;; This allows retention of tempo when kill-yanking sexps
(global-set-key (kbd "C-M-y") 'yank)

;; page up and down are unbound because I kept hitting them accidentally
;; on my XPS.
(global-set-key (kbd "<prior>") nil)
(global-set-key (kbd "<next>") nil)

(global-set-key (kbd "M-o") 'other-window)
(global-set-key (kbd "C-d") 'delete-other-windows) ;; replaces transpose char

(global-set-key (kbd "C-f") 'project-find-file)
(global-set-key (kbd "C-b") 'switch-to-buffer)

;; Use this for command, in place of M-x, avoiding the meta stretch.
(global-set-key (kbd "C-x C-m") 'execute-extended-command)

;; Rebind
(global-set-key (kbd "C-;") 'recenter-top-bottom)
(global-set-key (kbd "C-'") 'dabbrev-expand)
(global-set-key (kbd "C-M-SPC") 'set-mark-command) ;for tempo

(global-set-key (kbd "C-x v p") 'vc-pull)
;; to match C-x v P for push

(global-set-key (kbd "C-+") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)

;; stop accidentally zooming
(global-set-key (kbd "C-<wheel-up>") nil)
(global-set-key (kbd "C-<wheel-down>") nil)

;;;;;;;;;;;;;;
;; markdown
;;;;;;;;;;;;;;

(setq markdown-fontify-code-blocks-natively t)
(setq markdown-hide-markup t)
(setq markdown-max-image-size '(1500 . 1500))
(add-hook 'markdown-mode-hook 'visual-line-mode)

;;;;;;;;;;;;;;
;; Org mode
;;;;;;;;;;;;;;

(setq org-hide-emphasis-markers t)
(setq org-hide-leading-stars t)
(add-hook 'org-mode-hook 'visual-line-mode)
(add-hook 'org-mode-hook 'visual-fill-column-mode)
(add-hook 'org-mode-hook 'adaptive-wrap-prefix-mode)

;;;;;;;;;;;;;;;;;;;;;;;
;; clojure (and elisp)
;;;;;;;;;;;;;;;;;;;;;;;

(require 'smartparens-config)

(defun sexp-bindings ()
  (progn
    (local-set-key (kbd "C-l") 'forward-sexp)
    (local-set-key (kbd "C-h") 'backward-sexp)
    (local-set-key (kbd "C-j") 'sp-end-of-sexp)
    (local-set-key (kbd "C-k") 'sp-beginning-of-sexp)

    (local-set-key (kbd "C-M-l") 'sp-down-sexp)
    (local-set-key (kbd "C-M-h") 'sp-backward-up-sexp)
    (local-set-key (kbd "C-M-j") 'sp-backward-down-sexp)
    (local-set-key (kbd "C-M-k") 'sp-up-sexp)))

;; other options for sexps to think about using smart-parens
;; - wrapping / unwrapping
;; - slurp / barf
;; - transpose
;; - kill

(add-hook 'emacs-lisp-mode-hook #'smartparens-mode)
(add-hook 'emacs-lisp-mode-hook 'sexp-bindings)
(add-hook 'emacs-lisp-mode-hook
	  (lambda () (electric-pair-local-mode)))

(add-hook 'clojure-mode-hook #'smartparens-mode)
(add-hook 'clojure-mode-hook 'electric-pair-local-mode)
(add-hook 'clojure-mode-hook 'sexp-bindings)
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
	  (lambda () (electric-pair-local-mode)))


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
