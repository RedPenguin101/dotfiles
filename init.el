;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Joe's Emacs init.el
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Things to do
;; - Look at Prot's `display-buffer-alist' configuration
;;   https://www.youtube.com/watch?v=1-UIzYPn38s
;;   - magit should open fullscreen
;;   - cider repl shouldn't take focus
;;   - help Major should open full screen
;; - Look at having a 'repeat' function for modal leaders, so when
;;   you SPC-<x> <x> it does the SPC-<x> command twice.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Basic editor functionality ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; This is required on older versions of emacs because (according to
;; magit error messsages) "Due to bad defaults, Emac's package manager
;; refuses to update ... build-in packages..."
(setq package-install-upgrade-built-int t)

;; Remove noise at startup
(setq inhibit-startup-message t)
(setq initial-scratch-message nil)
(setq ring-bell-function 'ignore)
(setq visible-bell 1)

;; Backups, lockfiles and trash
(setq create-lockfiles nil)
(setq make-backup-files nil)
(setq backup-inhibited t)
(setq delete-by-moving-to-trash t)

;; Autoreverts
(global-auto-revert-mode 1)
(setq global-auto-revert-non-file-buffers t) ; for dired
(setq auto-revert-verbose nil) ;; don't message me

;; line numbers display
(add-hook 'prog-mode-hook 'display-line-numbers-mode)
(setq-default display-line-numbers-type 'relative)

;; Windows and splitting
(add-to-list 'initial-frame-alist '(fullscreen . maximized))

(setq display-buffer-alist nil)

;; https://protesilaos.com/codelog/2024-02-08-emacs-window-rules-display-buffer-alist/
(add-to-list 'display-buffer-alist '("\\*Occur\\*" (display-buffer-reuse-mode-window display-buffer-below-selected)
                                     (window-height . fit-window-to-buffer) (dedicated . t) (body-function . select-window)))

(add-to-list 'display-buffer-alist '((derived-mode . magit-status-mode)
                                     (display-buffer-use-some-window)
                                     (body-function . delete-other-windows)))

(setq switch-to-buffer-in-dedicated-window 'pop)

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

;; Toggle visualization of matching parens. Matching parenthesis is
;; highlighted in ‘show-paren-style’ after ‘show-paren-delay’ seconds
;; of Emacs idle time.
(show-paren-mode +1)
;; When enabled, typing an open parenthesis automatically inserts the
;; corresponding closing parenthesis, and vice versa. (Likewise for
;; brackets, etc.). If the region is active, the parentheses
;; (brackets, etc.) are inserted around the region instead.
(electric-pair-mode)

;; highlights the currently selected line
(global-hl-line-mode +1)

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

;; When Desktop Save mode is enabled, the state of Emacs is saved from
;; one session to another. To see all the options you can set, browse
;; the ‘desktop’ customization group.
(desktop-save-mode 1)
(setq desktop-save t)
(setq desktop-load-locked-desktop t)
(setq desktop-restore-frames t)
(setq desktop-auto-save-timeout 300)
(setq desktop-globals-to-save nil)
(setq desktop-modes-not-to-save
      '(tags-table-mode special-mode Custom-mode dired-mode))

;; Stuff I don't have a place for currently
(setq read-answer-short t) ;; always accepts 'y' instead of 'yes'
(setq use-short-answers t)
;; Can use C-u C-SPC C-SPC C-SPC... instead of C-u C-SPC C-u C-SPC...
;; (or SPC-. t t t t..)
(setq set-mark-command-repeat-pop t)
(global-subword-mode 1)

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

;;;;;;;;;;;;;;;;;;;;;;;;;
;; non-native packages ;;
;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'package)

(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

(use-package diff-hl
  :config
  (diff-hl-margin-mode))

(use-package magit)
(use-package ag)

;;;;;;;;;;;;;;;;;;;;
;; Modal keybinds ;;
;;;;;;;;;;;;;;;;;;;;

(define-key input-decode-map [?\C-m] [C-m]) ;; RET
(define-key input-decode-map [?\C-i] [C-i]) ;; TAB

(load "~/.emacs.d/lisp/modal.el")

(add-hook 'prog-mode-hook 'modal-mode)
(add-hook 'text-mode-hook 'modal-mode)
(add-hook 'conf-mode-hook 'modal-mode)

;; Mostly these are just taking common commands for which the default
;; keybinds have modifiers, and removing the modifiers.

(define-modal-command-keys
 '(;; LEFT HAND
   ;; a
   ("s" . isearch-forward)          ;; C-s
   ("d" . kill-word)                ;; M-d
   ("f" . forward-char)             ;; C-s
   ("g" . set-mark-command)         ;; C-SPC

   ;; t
   ("w" . delete-other-windows)
   ("q" . prog-fill-reindent-defun) ;; M-q
   ("e" . move-end-of-line)         ;; C-e
   ("r" . isearch-backward)         ;; C-r

   ;; z
   ("x" . execute-extended-command) ;; M-x
   ("c" . switch-to-buffer)         ;; (b is taken)
   ("v" . scroll-up-command)        ;; C-v
   ("b" . backward-char)            ;; C-x b

   ;; RIGHT HAND
   ("h" . backward-sexp)
   ("j" . forward-sexp)
   ("k" . kill-sexp)                ;; C-M-k
   ("l" . recenter-top-bottom)      ;; C-l
   (";" . comment-line)             ;; C-x C-;

   ("y" . yank)                     ;; C-y
   ("u" . universal-argument)       ;; C-u
   ("i" . modal-mode--insert-mode-init)
   ("o" . other-window)             ;; C-x o
   ("p" . previous-line)            ;; C-p

   ("n" . next-line)                ;; C-n
   ("m" . back-to-indentation)      ;; M-m
   ("/" . undo)                     ;; C-/
   ("," . beginning-of-defun)
   ("." . end-of-defun)

   ;; NUMBERS

   ("-" . negative-argument)        ;; C--
   ("1" . digit-argument)
   ("2" . digit-argument)
   ("3" . digit-argument)
   ("4" . digit-argument)
   ("5" . digit-argument)
   ("6" . digit-argument)
   ("7" . digit-argument)
   ("8" . digit-argument)
   ("9" . digit-argument)
   ("0" . digit-argument)

   ))


(define-modal-leader-keys
 '(("a" . ag-project)
   ("o" . occur)                    ;; M-s o
   ("q" . query-replace)            ;; M-%
   ("f" . find-file)                ;; C-x C-f
   ("s" . save-buffer)              ;; C-x C-s
   ("k" . kill-buffer)              ;; C-x k
   ("d" . dired-jump)               ;; C-x C-d (sort of)
   ("r" . recentf-open-minibuff)
   ("g" . magit-status)             ;; C-x g
   ("w" . whitespace-cleanup)
   ("h" . highlight-phrase)

   ("," . beginning-of-buffer)      ;; M-<
   ("." . end-of-buffer)            ;; M->
   ("<backspace>" . kill-whole-line) ;; C-S-<backspace>

   ("1" . delete-other-windows)     ;; C-x 1
   ("2" . split-window-below)       ;; C-x 2
   ("3" . split-window-right)       ;; C-x 3
   ))

;; globals
(global-set-key (kbd "C-b") 'switch-to-buffer)
(global-set-key (kbd "M-o") 'other-window)
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
(global-unset-key (kbd "C-x C-x")) ;; save buffers kill terminal

;;;;;;;;;;;;;;
;; markdown
;;;;;;;;;;;;;;

(setq markdown-fontify-code-blocks-natively t)
(setq markdown-max-image-size '(1500 . 1500))
(add-hook 'markdown-mode-hook 'auto-fill-mode)

;;;;;;;;;;;;;;;;;;;;;;;
;; clojure (and elisp)
;;;;;;;;;;;;;;;;;;;;;;;

(setq clojure-toplevel-inside-comment-form t)

;; builtin cider commands:
;;   C-c C-p cider-pprint-eval-last-sexp
;;   C-c C-e cider-eval-last-sexp
;;   C-c C-c cider-eval-defun-at-point (defun = top level)
;;   C-M-x   cider-eval-defun-at-point
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
              (local-set-key (kbd "C-c C-c") 'recompile)
              (local-set-key (kbd "C-c f") 'clang-format-buffer))))

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
  :bind (:map odin-mode-map ("C-c C-c" . 'recompile)))

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
