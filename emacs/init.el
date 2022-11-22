;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; My Emacs Setup
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Basics

(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

(setq inhibit-startup-message t)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(tooltip-mode -1)
(menu-bar-mode -1)
(setq visible-bell t)

;; Initialize package sources
(require 'package)
(setq package-archives
      '(("melpa" . "https://melpa.org/packages/")
	("melpa stable" . "https://stable.melpa.org/packages/")
	("org" . "https://orgmode.org/elpa/")
        ("elpa" . "https://elpa.gnu.org/packages/")))

(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

;; use-package setup
(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

;; themes and styling
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(set-face-attribute 'default nil
		    :family "Fira Code"
		    :weight 'semi-light
		    :height 139)

(use-package doom-themes)
;; TODO: Relative line numbers
(load-theme 'doom-opera-light t)

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Miscellaneous
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Line numbers
(column-number-mode)
(global-display-line-numbers-mode t)
(defun turn-off-line-numbers () (display-line-numbers-mode 0))
(setq mode-line-percent-position nil)
;; TODO: Relative line numbers

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Package/Mode Specific Configuration
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Emacs navigation and usage help
;;;;;;;;;;;;;;;;;;;

;; Consider: helpful https://github.com/Wilfred/helpful

(use-package which-key
  :init (which-key-mode)
  :diminish
  :config (setq which-key-idle-delay 0.3))

(use-package ivy
  :diminish ivy-mode
  :bind (:map ivy-minibuffer-map
	 ("C-j" . ivy-next-line)
	 ("C-k" . ivy-previous-line)
	 :map ivy-switch-buffer-map
	 ("C-j" . ivy-next-line)
	 ("C-k" . ivy-previous-line))
  :config (ivy-mode 1))
(ivy-mode 1)

;; Consider adding ivy-rich / counsel / swiper
;; Ivy, a generic completion mechanism for Emacs.
;; Counsel, a collection of Ivy-enhanced versions of common Emacs commands.
;; Swiper, an Ivy-enhanced alternative to Isearch.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Functionality/Mode plugins
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Keybindings, Evil and General.el
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Evil
;; https://github.com/emacs-evil/evil
(use-package evil
  :config (evil-mode 1))

;; https://github.com/noctuid/general.el
;; General is an easier way to define keybindings
(use-package general)

(general-define-key
 "C-M-j"    'switch-to-prev-buffer
 "<escape>" 'keyboard-escape-quit)

(general-swap-key nil 'motion ";" ":")

;; TODO: an easier way to open files

;; Evil rebinds, use C-g to revert from insert to normal mode.
;; replicating global emacs semantics for C-g
(general-define-key
 :states 'insert
 "C-g" 'evil-normal-state)

(general-def 'motion
  "j" 'evil-next-visual-line
  "k" 'evil-previous-visual-line)

(general-create-definer my-prefix
  :states '(normal insert emacs)
  :prefix "SPC"
  :global-prefix "C-SPC")

;; Things for lisp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Other things to look at:
;; Lispy/Lispyville
;; Symex
;; Puni

(use-package paredit
  :hook emacs-lisp-mode)

;; paredit keybinds
;; M-( paredit-wrap-round (leader w)
;; C-) paredit-forward-slurp-sexp (leader s)
;; C-} paredit-forward-barf-sexp
;; C-( paredit-backward-slurp-sexp
;; C-{ paredit-backward-barf-sexp
;; C-M-f/b paredit-forward/backward (C-l/h)
;;   d/u are in-forward/out-backward (C-j/k)
;;   p/n are in-backward/out-forward
;; M-S paredit-split-s (leader s)

(general-define-key
 :keymaps 'paredit-mode-map
 :states 'motion
  "C-l" 'paredit-forward
  "C-h" 'paredit-backward
  "C-j" 'paredit-forward-down
  "C-k" 'paredit-backward-up)

(my-prefix
  :keymaps 'paredit-mode-map
  "i" 'indent-sexp
  "s" 'paredit-forward-slurp-sexp
  "d" 'paredit-forward-barf-sexp
  "w" 'paredit-wrap-round)

(my-prefix
  :keymaps 'emacs-lisp-mode-map
  "e" 'eval-last-sexp)

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(add-hook 'emacs-lisp-mode-hook 'show-paren-mode)

;; Clojure
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package clojure-mode)
(use-package cider)

(add-hook 'clojure-mode-hook 'show-paren-mode)
(add-hook 'clojure-mode-hook 'paredit-mode)
(add-hook 'clojurescript-mode-hook 'show-paren-mode)
(add-hook 'clojurescript-mode-hook 'paredit-mode)

(my-prefix
  :keymaps 'clojure-mode-map
  "e" 'cider-eval-last-sexp
  "r" 'cider-pprint-eval-last-sexp
  "b" 'cider-eval-buffer
  "i" 'cider-inspect-last-result
  "t" 'cider-test-run-project-tests)

;; Company
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package company
  :config (global-company-mode t))

;; Projectile
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package projectile
  :init (projectile-mode +1))

(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)

(use-package doom-modeline
  :init (doom-modeline-mode 1)
  :hook (after-init . (lambda ()
			(setq-default header-line-format mode-line-format)
			(setq-default mode-line-format nil) ))
  :custom ((doom-modeline-bar-width 0)
	   (doom-modeline-buffer-encoding nil)
	   (doom-modeline-height 5)
	   (doom-modeline-buffer-file-name-style 'file-name)))

;; Markdown Mode
;;;;;;;;;;;;;;;;;

(use-package markdown-mode)
(add-hook 'markdown-mode-hook 'variable-pitch-mode)
(add-hook 'markdown-mode-hook 'turn-off-line-numbers)
(add-hook 'markdown-mode-hook 'flyspell-mode)
(add-hook 'markdown-mode-hook '(lambda () (toggle-word-wrap 1)))

;; TODO: use MMM for syntax highlighting https://jblevins.org/log/mmm

(set-face-attribute 'markdown-gfm-checkbox-face nil
		    :font "Fira Code")

(set-face-attribute 'markdown-pre-face nil
		    :font "Fira Code"
		    :background "white"
		    :weight 'semi-light
		    :height 139)

(set-face-attribute 'markdown-code-face nil
		    :font "Fira Code"
		    :background "white"
		    :weight 'semi-light
		    :height 139)


;; Dashboard
;;;;;;;;;;;;;;;;;

(use-package dashboard
  :config (dashboard-setup-startup-hook))

(setq dashboard-projects-backend 'projectile)
(add-to-list 'dashboard-items '(projects . 5))
(setq dashboard-banner-logo-title nil)
(setq dashboard-startup-banner nil)
(setq dashboard-center-content t)
(setq dashboard-show-shortcuts nil)

(defun go-home ()
  (interactive)
  (switch-to-buffer "*dashboard*"))

(my-prefix
  "h" '(go-home :which-key "Go to dashboard"))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Custom set variable - don't touch
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(company ligature cider clojure-mode paredit evil general flyspell-popup projectile mmm-mode doom-modeline counsel doom-themes which-key markdown-mode rainbow-delimiters ivy use-package bind-key)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )