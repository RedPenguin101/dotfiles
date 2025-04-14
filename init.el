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
(setq auto-revert-verbose nil) ;; don't message me

;; scrolling
(pixel-scroll-precision-mode)
(setq pixel-scroll-precision-use-momentum nil)

;; line numbers display
(add-hook 'prog-mode-hook 'display-line-numbers-mode)
(setq-default display-line-numbers-type 'relative)

;; Windows and splitting
(add-to-list 'initial-frame-alist '(fullscreen . maximized))

;; Editing preferences
(setq delete-selection-mode 1)
(setq kill-do-not-save-duplicates t) ;; doesn't duplicate things in the kill ring

;; Whitespace, tabs and spaces
(setq whitespace-line-column 120)
(setq whitespace-style '(face tabs empty trailing lines-tail))
(add-hook 'prog-mode-hook 'whitespace-mode)
(add-hook 'text-mode-hook 'whitespace-mode)
(setq-default indent-tabs-mode nil)
(setq sentence-end-double-space nil)
(setq tab-width 2)

;; parens
(show-paren-mode +1)
(electric-pair-mode)

;; highlights the currently selected line
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

;; desktop save mode reloads all your windows after restart
(desktop-save-mode 1)
(setq desktop-save t)
(setq desktop-load-locked-desktop t)
(setq desktop-restore-frames t)
(setq desktop-auto-save-timeout 300)
(setq desktop-globals-to-save nil)

;; Stuff I don't have a place for currently
(setq read-answer-short t) ;; always accepts 'y' instead of 'yes'
(setq use-short-answers t)
;; Can use C-u C-SPC C-SPC C-SPC... instead of C-u C-SPC C-u C-SPC...
(setq set-mark-command-repeat-pop t)

;;;;;;;;;;;
;; dired ;;
;;;;;;;;;;;
;; look into dired-omit-mode
;; remember and write down how to do that dired append thing, where
;; subdirs are added to the buffer
;; dired-collapse

(put 'dired-find-alternate-file 'disabled nil)
(setq dired-kill-when-opening-new-dired-buffer t)

(use-package wdired
  :ensure nil
  :commands (wdired-change-to-wdired-mode)
  :config
  (setq wdired-allow-to-change-permissions t)
  (setq wdired-create-parent-directories t))

;;;;;;;;;;;;;;;;
;; Mode line
;;;;;;;;;;;;;;;;
;; makes modeline a tiny bit thicker and gets rid of border to
;; get rid of that 90's look

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

;;;;;;;;;;;;;;;;;;;;;;;;;;
;; None native packages ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;

(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

(use-package diff-hl
  :config
  (global-diff-hl-mode))

(use-package magit)
(use-package avy)

;;;;;;;;;;;;;;;;;;;;
;; Modal keybinds ;;
;;;;;;;;;;;;;;;;;;;;
;; inspired by
;; https://github.com/xahlee/xah-fly-keys/blob/master/xah-fly-keys.el
;; things to bind:
;; - sexp expressions
;; - write-file (save as)
;; - mark-sexp

(define-key input-decode-map [?\C-m] [C-m]) ;; RET
(define-key input-decode-map [?\C-i] [C-i]) ;; TAB

(load "~/.emacs.d/lisp/modal.el")

(add-hook 'prog-mode-hook 'modal-mode)
(add-hook 'text-mode-hook 'modal-mode)

(define-modal-command-keys
 '(;; general
   ("a" . execute-extended-command)
   ("t" . set-mark-command)

   ;; MOVES
   ("i" . previous-line)
   ("k" . next-line)
   ("u" . backward-char)
   ("o" . forward-char)
   ("j" . backward-word)
   ("l" . forward-word)
   ("h" . xah/beginning-of-line-or-block)
   (";" . xah/end-of-line-or-block)

   ;; SEARCH
   ("n" . isearch-forward)
   ("m" . avy-goto-char-2)

   ;; WINDOWS
   ("," . other-window)
   ("3" . split-window-right)
   ("2" . split-window-below)
   ("1" . delete-other-windows)

   ;; KILLS
   ("d" . backward-kill-word)
   ("f" . kill-word)
   ("g" . kill-sexp)
   ("s" . backward-kill-sexp)

   ;; CUA
   ("x" . xah/cut-line-or-region)
   ("c" . xah/copy-line-or-region)
   ("v" . yank)
   ("r" . undo)

   ;; EDITS
   ("q" . fill-paragraph)
   ("z" . comment-dwim)
   ("w" . xah/shrink-whitespace)
   ("e" . upcase-dwim)

   ;; FILES AND BUFFERS
   ("b" . switch-to-buffer)))

(define-modal-sexp-overrides
 '(("j" . backward-sexp)
   ("l" . forward-sexp)
   ("i" . backward-up-list)
   ("k" . down-list)))

(define-modal-leader-keys
 '(("." . universal-argument)
   ("," . negative-argument)

   ("s" . save-buffer)
   ("f" . find-file)
   ("r" . recentf-open-minibuff)
   ("d" . dired-jump)
   ("k" . kill-buffer)

   ("l" . goto-line)
   ("n" . isearch-backward)
   ("q" . query-replace)
   ("o" . occur)
   ("i" . imenu)

   ("w" . whitespace-cleanup)))

(define-modal-project-keys
 '(("a" . ag-project)
   ("f" . project-find-file)
   ("k" . project-kill-buffers)
   ("g" . magit-status)
   ("b" . project-switch-to-buffer)))

;; globals
;; (global-set-key (kbd "C-i") 'previous-line) this gets confused with TAB, so indent-for-tab-command
(global-set-key (kbd "C-k") 'next-line)
(global-set-key (kbd "C-u") 'backward-char)
(global-set-key (kbd "C-o") 'forward-char)
(global-set-key (kbd "C-j") 'backward-word)
(global-set-key (kbd "C-l") 'forward-word)

(global-set-key (kbd "C-;") 'dabbrev-expand)
(global-set-key (kbd "C-b") 'switch-to-buffer)
(global-set-key (kbd "C-+") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)

;; Keys I always hit accidentally
(global-unset-key (kbd "C-<wheel-up>")) ;; stop zooming by mistake
(global-unset-key (kbd "C-<wheel-down>"))

(global-unset-key (kbd "C-z")) ;; suspend
(global-unset-key (kbd "C-x C-z")) ;; suspend
(global-unset-key (kbd "C-x f")) ;; col fill
(global-unset-key (kbd "C-t")) ;; transpose char
(global-unset-key (kbd "M-c"))   ;; capitalize

;;;;;;;;;;;;;;
;; markdown
;;;;;;;;;;;;;;

(setq markdown-fontify-code-blocks-natively t)
(setq markdown-max-image-size '(1500 . 1500))
(add-hook 'markdown-mode-hook 'auto-fill-mode)
(add-hook 'markdown-mode-hook 'visual-line-mode)

;;;;;;;;;;;;;;;;;;;;;;;
;; clojure (and elisp)
;;;;;;;;;;;;;;;;;;;;;;;

;; other options for sexps to think about using smart-parens
;; - wrapping / unwrapping
;; - slurp / barf
;; - transpose
;; - kill

(add-hook 'emacs-lisp-mode-hook 'modal-mode-sexp)
(add-hook 'clojure-mode-hook 'subword-mode)
(add-hook 'clojure-mode-hook 'modal-mode-sexp)

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

(add-hook 'odin-mode-hook 'subword-mode)

(with-eval-after-load 'eglot
  (add-to-list 'eglot-server-programs
               '(odin-mode . ("ols"))))

;;;;;;;;;;;;;;;;;;;;;;;;
;; Common lisp and Slime
;;;;;;;;;;;;;;;;;;;;;;;;

(load (expand-file-name "~/.quicklisp/slime-helper.el"))
(setq inferior-lisp-program "sbcl")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; automatically generated config
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq custom-file (concat user-emacs-directory "custom.el"))
(load custom-file 'noerror 'nomessage)
