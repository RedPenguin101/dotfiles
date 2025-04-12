;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Joe's Emacs init.el
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Things to do or try
;;
;; - from https://www.youtube.com/watch?v=51eSeqcaikM
;;   - save place mode
;; - from C.Meier's config https://github.com/gigasquid/emacs-config
;;     (setq make-backup-files nil)
;;     (setq auto-save-default nil)
;; - Shortcut for duplicate line? useful in C duplicate-dwim command
;; - shortcuts for commenting. Especially I would like next-sexp comment
;;   #_ for Clojure
;; - Misc stuff from Batsov https://github.com/bbatsov/emacs.d/blob/master/init.el
;; - avy - stoped using it for some reason, not sure why
;; - Solo (https://www.youtube.com/watch?v=j_2QkCcf8zE, https://github.com/LionyxML/emacs-solo/).
;;    Go through and steal the stuff
;;    (What does narrow to region do?)
;;     - savehist / save-place
;; - Check out https://github.com/adityaathalye/dotemacs/blob/master/init.el
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Basic editor functionality ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Remove most of the initial noisy stuff at startup
(setq inhibit-startup-message t)
(setq initial-scratch-message nil)
(setq ring-bell-function 'ignore)
(setq visible-bell 1)

;; File operations
(setq create-lockfiles nil)
(setq make-backup-files nil)
(setq backup-inhibited t)
(setq delete-by-moving-to-trash t)

;; Autoreverts
(global-auto-revert-mode 1)
(setq global-auto-revert-non-file-buffers t) ; for dired
(setq auto-revert-verbose nil)

;; scrolling
(setq pixel-scroll-precision-mode t)
(setq pixel-scroll-precision-use-momentum nil)
(setq scroll-conservatively 101)
(setq scroll-margin 5)

;; line numbers display
(add-hook 'prog-mode-hook 'display-line-numbers-mode)
(setq-default display-line-numbers-type 'relative)

;; Windows and splitting
(setq split-width-threshold 170)     ; So vertical splits are preferred
(setq split-height-threshold nil)
(add-to-list 'initial-frame-alist '(fullscreen . maximized))
(setq frame-resize-pixelwise t)
(setq resize-mini-windows 'grow-only)

;; Editing preferences
(setq delete-selection-mode 1)
(setq kill-do-not-save-duplicates t) ;; doesn't duplicate things in the kill ring

;; Whitespace, tabs and spaces
(setq whitespace-line-column 120)
(setq whitespace-style '(face tabs empty trailing lines-tail))
(add-hook 'prog-mode-hook 'whitespace-mode)
(setq-default indent-tabs-mode nil)
(setq sentence-end-double-space nil)
(setq tab-width 2)

;; parens
(show-paren-mode +1)
(electric-pair-mode)

;; Diffs and version control
(global-hl-line-mode +1)

;; completions
(setq completion-ignore-case t)
(setq completions-detailed t)
;; TAB first tries to indent the current line, and if the line was
;; already indented, then try to complete the thing at point.
(setq tab-always-indent 'complete)

;; FIDO/Minibuffer
;; https://www.masteringemacs.org/article/understanding-minibuffer-completion
(fido-vertical-mode)

;; Savehist
(setq savehist-additional-variables
      '(kill-ring                            ; clipboard
        register-alist                       ; macros
        mark-ring global-mark-ring           ; marks
        search-ring regexp-search-ring))     ; searches
(setq save-place-file (expand-file-name "saveplace" user-emacs-directory))
(setq save-place-limit 600)
(save-place-mode 1)

(desktop-save-mode 1)
(setq desktop-save t)
(setq desktop-load-locked-desktop t)
(setq desktop-restore-frames t)
(setq desktop-auto-save-timeout 300)
(setq desktop-globals-to-save nil)

;; Stuff I don't have a place for currently
(setq read-answer-short t) ;; always accepts 'y' instead of 'yes'
(setq use-short-answers t)
(setq set-mark-command-repeat-pop t) ;; So we can use C-u C-SPC C-SPC C-SPC... instead of C-u C-SPC C-u C-SPC...

;;;;;;;;;;;
;; dired ;;
;;;;;;;;;;;

(put 'dired-find-alternate-file 'disabled nil)

(use-package wdired
  :ensure nil
  :commands (wdired-change-to-wdired-mode)
  :config
  (setq wdired-allow-to-change-permissions t)
  (setq wdired-create-parent-directories t))

;;;;;;;;;;;;;;;;
;; Mode line
;;;;;;;;;;;;;;;;
;; Minimal mode line with just the file name and the status indicator

(let ((bg (face-attribute 'mode-line :background)))
  (set-face-attribute 'mode-line nil
                      :box (list :line-width 4 :color bg :style nil)))

(let ((bg (face-attribute 'mode-line-inactive :background)))
  (set-face-attribute 'mode-line-inactive nil
                      :box (list :line-width 4 :color bg :style nil)))

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
  (setenv "PATH" (format "%s:%s" "/opt/homebrew/opt/llvm/bin" (getenv "PATH"))))

;;;;;;;;;;;;;;;;;;;;;
;; Xah's functions ;;
;;;;;;;;;;;;;;;;;;;;;
;; https://github.com/xahlee/xah-fly-keys/blob/master/xah-fly-keys.el

(load "~/.emacs.d/lisp/xah.el")

;;;;;;;;;;;;;;;;;;;;
;; Modal keybinds ;;
;;;;;;;;;;;;;;;;;;;;
;; inspired by
;; https://github.com/xahlee/xah-fly-keys/blob/master/xah-fly-keys.el
;; things to bind:
;; - sexp expressions
;; - page up/down
;; - more kills, expecially sexp kills
;; - steal xah's dwim copy and paste commands
;; - uppercase (maybe some better case stuff?)
;; - add global versions of basic moves with C-
;; - query replace
;; - write-file (save as)
;; - take q off command - too used in dired and friends
;; - mark popping

(load "~/.emacs.d/lisp/modal.el")

(add-hook 'prog-mode-hook 'modal-mode)

(define-command-keys
 '(;; general
   ("a" . execute-extended-command)
   ("t" . set-mark-command)

   ;; MOVES
   ("i" . previous-line)
   ("k" . next-line)
   ("j" . backward-word)
   ("l" . forward-word)
   ("u" . move-beginning-of-line)
   ("o" . move-end-of-line)
   ("h" . xah/beginning-of-line-or-block)
   (";" . xah/end-of-line-or-block)

   ;; WINDOWS
   ("," . other-window)
   ("3" . split-window-right)
   ("2" . split-window-below)
   ("1" . delete-other-windows)

   ;; KILLS
   ("e" . backward-kill-word)
   ("r" . kill-word)

   ;; CUA
   ("x" . xah/cut-line-or-region)
   ("c" . xah/copy-line-or-region)
   ("v" . yank)
   ("/" . undo)

   ;; EDITS
   ("q" . fill-paragraph)
   ("z" . comment-dwim)
   ("w" . xah/shrink-whitespace)

   ;; FILES AND BUFFERS
   ("b" . switch-to-buffer)

   ;; SEARCH
   ("n" . isearch-forward)))

(define-leader-keys
 '(("s" . save-buffer)
   ("f" . project-find-file)
   ("a" . ag-project)
   ("g" . magit-status)
   ("n" . avy-goto-char-2)
   ("w" . whitespace-cleanup)
   ("b" . project-switch-to-buffer)
   ("." . universal-argument)
   ("o" . occur)
   ("d" . dired-jump)
   ("r" . recentf-open-minibuff)))

;; globals
(global-set-key (kbd "C-;") 'dabbrev-expand)
(global-set-key (kbd "C-b") 'switch-to-buffer)
(global-set-key (kbd "C-+") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)

;; Keys I always hit accidentally and things I have mapped to other things
(global-unset-key (kbd "C-l")) ;; recenter

(global-unset-key (kbd "C-<wheel-up>")) ;; stop zooming by mistake
(global-unset-key (kbd "C-<wheel-down>"))

(global-unset-key (kbd "C-z")) ;; suspend
(global-unset-key (kbd "C-x C-z")) ;; suspend
(global-unset-key (kbd "C-x f")) ;; col fill
(global-unset-key (kbd "C-t")) ;; transpose char
(global-unset-key (kbd "M-c"))   ;; capitalize

;;;;;;;;;;;;;;;;
;; compilation
;;;;;;;;;;;;;;;;
;; Taken from emacs-solo

(use-package compile
  :ensure nil
  :custom
  (compilation-always-kill t)
  (compilation-scroll-output t)
  (ansi-color-for-compilation-mode t)
  :config
  (add-hook 'compilation-filter-hook #'ansi-color-compilation-filter))

;;;;;;;;;;;;;;;;;;;;;;;;;;
;; None native packages ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;

(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

(use-package diff-hl
  :config
  (global-diff-hl-mode))

;;;;;;;;;;;;;;
;; markdown
;;;;;;;;;;;;;;

(setq markdown-fontify-code-blocks-natively t)
;(setq markdown-hide-markup t)
(setq markdown-max-image-size '(1500 . 1500))
(add-hook 'markdown-mode-hook 'visual-line-mode)
(add-hook 'markdown-mode-hook 'auto-fill-mode)

;;;;;;;;;;;;;;;;;;;;;;;
;; clojure (and elisp)
;;;;;;;;;;;;;;;;;;;;;;;

;; other options for sexps to think about using smart-parens
;; - wrapping / unwrapping
;; - slurp / barf
;; - transpose
;; - kill

(add-hook 'emacs-lisp-mode-hook
          (lambda () (electric-pair-local-mode)))

(add-hook 'clojure-mode-hook 'subword-mode 'electric-pair-mode)
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

(add-hook 'c++-mode-hook
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
;; Odin
;;;;;;;;;;;;;;;;;;;;;;;;

;; Mode is here https://git.sr.ht/~mgmarlow/odin-mode
;; install with M-x package-vc-install RET https://git.sr.ht/~mgmarlow/odin-mode

(use-package odin-mode
  :bind (:map odin-mode-map
              ("C-c C-c" . 'recompile)))

(add-hook 'c++-mode-hook 'subword-mode)


(with-eval-after-load 'eglot
  (add-to-list 'eglot-server-programs
               '(odin-mode . ("ols"))))

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
(load custom-file 'noerror 'nomessage)
