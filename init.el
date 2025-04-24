;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Joe's Emacs init.el
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Basic editor functionality ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Remove noise at startup
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
;; (SPC-. t t t t.. in modal-mode)
(setq set-mark-command-repeat-pop t)

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
(add-hook 'conf-mode-hook 'modal-mode)

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
   ("1" . delete-other-windows)
   ("2" . split-window-below)
   ("3" . split-window-right)

   ;; KILLS
   ("d" . backward-kill-word)
   ("f" . kill-word)
   ("g" . kill-sexp)
   ("s" . backward-kill-sexp)

   ;; CUA
   ("x" . xah/cut-line-or-region)
   ("c" . xah/copy-line-or-region)
   ("v" . yank)
   ("/" . undo)

   ;; EDITS
   ("q" . fill-paragraph)
   ("z" . comment-dwim)
   ("w" . xah/shrink-whitespace)
   ("e" . upcase-dwim)

   ;; FILES AND BUFFERS
   ("b" . switch-to-buffer)))

(define-modal-leader-keys
 '(("." . universal-argument)
   ("," . negative-argument)
   ("g" . magit-status)

   ("s" . save-buffer)
   ("f" . find-file)
   ("r" . recentf-open-minibuff)
   ("d" . dired-jump)
   ("k" . kill-buffer)

   ("h" . highlight-regexp)
   ("l" . goto-line)
   ("a" . ag-project)
   ("n" . isearch-backward)
   ("q" . query-replace)
   ("o" . occur)
   ("i" . imenu)

   ("w" . whitespace-cleanup)))

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
