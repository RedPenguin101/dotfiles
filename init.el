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
;;     ag - especially ag-at-point - to search for words within a project
;;     (setq make-backup-files nil)
;;     (setq auto-save-default nil)
;; - bind dabbrev-expand to something more home-row friendly: C-' maybe
;; - rebind C-M-SPC to set-mark-command, from mark-sexp. Marking a sexp
;;   is great, but it's tempo disruptive to not have this
;;
;; Things I tried and didn't like
;;   Beacon - flashing cursor on move
;;   (setq-default show-trailing-whitespace t)
;;   lsp mode for Clojure. Bit too much
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Package installation and initialization
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar my-packages
  '(markdown-mode
    org
    ivy ivy-prescient))

(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))

(ivy-mode)
(ivy-prescient-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; the basics and one-line-wonders
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

(setq ring-bell-function 'ignore)
(blink-cursor-mode 0)
(display-battery-mode t)
(menu-bar-mode -1)

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

;;;;;;;;;;;;;;;;
;; recent mode
;;;;;;;;;;;;;;;;

(recentf-mode 1)
(setq recentf-max-menu-items 25)
(setq recentf-max-saved-items 25)
(global-set-key "\C-x\ \C-r" 'recentf-open-files)

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

;; sentence and para are defaults for C-M, but are expected to be
;; overwritten by sexp fwd/dwn etc. for lisps.

(setq sentence-end-double-space nil)

(global-set-key (kbd "C-M-l") 'forward-sentence) ;; aka page down replaces reposition-window
(global-set-key (kbd "C-M-h") 'backward-sentence) ;; replace mark-defun

(global-set-key (kbd "C-M-j") 'forward-paragraph) ;; also default-indent-new-line
(global-set-key (kbd "C-M-k") 'backward-paragraph) ;; replaces kill

(defun sexp-bindings ()
  (progn
    (local-set-key (kbd "C-M-l") 'forward-sexp)
    (local-set-key (kbd "C-M-h") 'backward-sexp)
    (local-set-key (kbd "C-M-j") 'down-list)
    (local-set-key (kbd "C-M-k") 'backward-up-list)))

(global-set-key (kbd "M-l") 'end-of-buffer) ;; replaces downcase-word
(global-set-key (kbd "M-h") 'beginning-of-buffer) ;; replaces mark-paragraph

(global-set-key (kbd "M-j") 'scroll-up-command) ;;replaces default-indent-new-line
(global-set-key (kbd "M-k") 'scroll-down-command) ;; replaces kill sentence

(global-set-key (kbd "M-o") 'other-window)

;; kill rebinds
;;       w       d
;; C   bk-wd   fw-char
;; M   copy-r  fw-word
;; CM  region  sent

(global-set-key (kbd "C-w") 'backward-kill-word) ;; replaces kill-region
(global-set-key (kbd "C-M-w") 'kill-region)
;; M-w is already kill ring save - aka copy

(global-set-key (kbd "C-M-d") 'kill-sentence) ;; replaces down-list

;; This allows retention of tempo when kill-yanking sexps
(global-set-key (kbd "C-M-y") 'yank)

;; potential things to keybind (from Yegge)
;;   comment region

;; Use this for command, in place of M-x, avoiding the meta stretch.
(global-set-key (kbd "C-x C-m") 'execute-extended-command)

;; Rebind
(global-set-key (kbd "C-;") 'recenter-top-bottom)
(global-set-key (kbd "C-'") 'dabbrev-expand)



;;;;;;;;;;;;;;
;; markdown
;;;;;;;;;;;;;;

(setq markdown-fontify-code-blocks-natively t)
(setq markdown-hide-markup t)
(setq markdown-max-image-size '(1500 . 1500))
(add-hook 'markdown-mode-hook 'visual-line-mode)
(add-hook 'markdown-mode-hook 'visual-fill-column-mode)
(add-hook 'markdown-mode-hook 'adaptive-wrap-prefix-mode)

;;;;;;;;;;;;;;
;; Org mode
;;;;;;;;;;;;;;

(setq org-hide-emphasis-markers t)
(setq org-hide-leading-stars t)
(setq org-adapt-indentation t)
(add-hook 'org-mode-hook 'visual-line-mode)
(add-hook 'org-mode-hook 'visual-fill-column-mode)
(add-hook 'org-mode-hook 'adaptive-wrap-prefix-mode)

;;;;;;;;;;;;;;;;;;;;;;;
;; clojure (and elisp)
;;;;;;;;;;;;;;;;;;;;;;;

(add-hook 'emacs-lisp-mode-hook 'sexp-bindings)
(add-hook 'emacs-lisp-mode-hook
	  (lambda () (electric-pair-local-mode)))

(add-hook 'clojure-mode-hook 'sexp-bindings)
(add-hook 'cider-mode-hook
	  (lambda () (local-set-key (kbd "C-c f") 'cider-format-defun)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; automatically generated config
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes '(material-light))
 '(custom-safe-themes
   '("f149d9986497e8877e0bd1981d1bef8c8a6d35be7d82cba193ad7e46f0989f6a" "db86c52e18460fe10e750759b9077333f9414ed456dc94473f9cf188b197bc74" "fe1c13d75398b1c8fd7fdd1241a55c286b86c3e4ce513c4292d01383de152cb7" default))
 '(display-battery-mode t)
 '(display-time-mode t)
 '(line-number-mode nil)
 '(package-selected-packages
   '(ag org which-key lsp-mode ivy-prescient ivy nov diff-hl adaptive-wrap visual-fill-column material-theme markdown-mode dracula-theme cider))
 '(safe-local-variable-values
   '((cider-clojure-cli-global-options . "-A:dev")
     (cider-preferred-build-tool . clojure-cli)))
 '(scroll-bar-mode nil)
 '(tool-bar-mode nil))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "Fira Code" :foundry "CTDB" :slant normal :weight normal :height 139 :width normal))))
 '(fixed-pitch ((t (:family "Fira Code"))))
 '(markdown-code-face ((t (:height 0.8 :family "Fira Code"))))
 '(markdown-inline-code-face ((t (:height 0.8 :family "Fira Code"))))
 '(markdown-pre-face ((t (:inherit nil :family "Fira Code"))))
 '(org-block ((t (:inherit fixed-pitch :extend t :background "#EFEBE9" :foreground "#212121" :height 0.8))))
 '(org-code ((t (:inherit fixed-pitch :background "#EFEBE9" :foreground "#212121" :height 0.8))))
 '(org-level-1 ((t (:inherit font-lock-function-name-face :extend nil :background "--" :box nil :weight bold :height 1.3))))
 '(org-level-2 ((t (:inherit font-lock-function-name-face :extend nil :background "--" :box nil :weight bold :height 1.2))))
 '(org-level-3 ((t (:inherit font-lock-function-name-face :extend nil :weight bold :height 1.1))))
 '(org-level-4 ((t (:inherit nil :extend nil :height 1.0))))
 '(org-meta-line ((t (:inherit fixed-pitch :height 0.6))))
 '(org-table ((t (:inherit fixed-pitch :background "#e0f7fa" :foreground "#1565c0"))))
 '(org-verbatim ((t (:height 0.8 :family "Fira Code"))))
 '(variable-pitch ((t (:height 1.5 :family "ETBookOT")))))
