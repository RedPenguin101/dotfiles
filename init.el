(setq exec-path (append exec-path '("/home/joe/.nvm/versions/node/v18.14.2/bin")))
(setenv "PATH" (concat (getenv "PATH") ":/home/joe/.nvm/versions/node/v18.14.2/bin"))

;; packages
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

(defvar my-packages
  '(dracula-theme avy ivy ivy-rich prescient ivy-prescient
    ligature markdown-mode projectile
    all-the-icons doom-modeline which-key company
    visual-fill-column writeroom-mode
    super-save
    clojure-mode cider
    lsp-mode company
    smartparens
    evil)
  "A list of packages to ensure are installed at launch.")

(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))

;; evil and keybinds

(require 'evil)
(evil-mode 1)

(define-key evil-insert-state-map (kbd "C-g") 'evil-force-normal-state)

(define-key evil-normal-state-map (kbd "1") 'delete-other-windows)
(define-key evil-normal-state-map (kbd "0") 'other-window)
(define-key evil-normal-state-map (kbd "2") 'split-window-below)
(define-key evil-normal-state-map (kbd "3") 'split-window-right)

(define-key evil-normal-state-map (kbd "q") 'kill-line)
(define-key evil-normal-state-map (kbd "y") 'yank)
(define-key evil-normal-state-map (kbd "r") 'evil-replace)
(define-key evil-normal-state-map (kbd "t") 'isearch-forward)
(define-key evil-normal-state-map (kbd "w") 'evil-visual-char)
(define-key evil-normal-state-map (kbd "C-w") 'evil-visual-block)
(define-key evil-normal-state-map (kbd "W") 'evil-visual-line)

(define-key evil-normal-state-map (kbd "f") 'evil-scroll-line-to-center)
(define-key evil-normal-state-map (kbd "H") 'beginning-of-line)
(define-key evil-normal-state-map (kbd "L") 'end-of-line)
(define-key evil-normal-state-map (kbd "J") 'evil-scroll-page-down)
(define-key evil-normal-state-map (kbd "K") 'evil-scroll-page-up)

(define-key evil-normal-state-map (kbd "b") 'switch-to-buffer)

(define-key evil-normal-state-map (kbd "M-RET") 'eval-last-sexp)
(define-key evil-normal-state-map (kbd "C-M-<return>") 'eval-buffer)
(define-key evil-normal-state-map (kbd "M-q") 'kill-sexp)
(define-key evil-normal-state-map (kbd "M-k") 'backward-sexp)
(define-key evil-normal-state-map (kbd "M-h") 'backward-sexp)
(define-key evil-normal-state-map (kbd "M-j") 'forward-sexp)
(define-key evil-normal-state-map (kbd "M-l") 'forward-sexp)
(define-key evil-normal-state-map (kbd "M-q") 'kill-sexp)

(global-set-key "\C-x\ \C-r" 'recentf-open-files)

;; general / small
(scroll-bar-mode -1)
(tool-bar-mode -1)
(menu-bar-mode -1)
(setq inhibit-startup-message t)

(setq-default fill-column 80)
(electric-pair-mode 1)
(show-paren-mode 1)

(setq visible-bell t)
(setq ring-bell-function 'ignore)

(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

(super-save-mode +1)
(setq super-save-auto-save-when-idle t)
(setq auto-save-default nil)

;; markdown-mode
(require 'markdown-mode)
(add-hook 'markdown-mode-hook (lambda () (visual-line-mode t)))
(add-hook 'markdown-mode-hook (lambda () (flyspell-mode t)))
(setq markdown-fontify-code-blocks-natively t)
(add-hook 'text-mode-hook (lambda () (visual-line-mode t)))

;; modeline
(require 'doom-modeline)
(doom-modeline-mode 1)
(setq doom-modeline-buffer-encoding nil)

;; recent file mode
(recentf-mode 1)
(setq recentf-max-menu-items 25)
(setq recentf-max-saved-items 25)

;; projectile (and ivy)
(ivy-mode +1)
(projectile-mode +1)
(ivy-prescient-mode +1)

;; lsp
(setq lsp-lens-enable nil)
(setq lsp-headerline-breadcrumb-enable nil)

;; smartparens
;; https://ebzzry.com/en/emacs-pairs/
(require 'smartparens-config)

(defun smartparens-keys ()
  (progn (local-set-key (kbd "M-l") #'sp-forward-sexp)
	 (local-set-key (kbd "M-h") #'sp-backward-sexp)
	 (local-set-key (kbd "M-j") #'sp-down-sexp)
	 (local-set-key (kbd "M-k") #'sp-up-sexp)
	 (local-set-key (kbd "M-a") #'sp-beginning-of-sexp)
	 (local-set-key (kbd "M-s") #'sp-end-of-sexp)
         (local-set-key (kbd "M-9") #'sp-wrap-round)
	 (local-set-key (kbd "M-{") #'sp-wrap-curly)
	 (local-set-key (kbd "M-,") #'sp-forward-barf-sexp)
	 (local-set-key (kbd "M-.") #'sp-forward-slurp-sexp)))

;; lisp

(defun elisp-keybinds ()
  (progn (local-set-key (kbd "M-RET") #'eval-last-sexp)
	 (local-set-key (kbd "C-M-RET") #'eval-buffer)))

(add-hook 'emacs-lisp-mode-hook
	  (lambda () (local-set-key (kbd "M-RET")
				    #'eval-last-sexp)))
(add-hook 'emacs-lisp-mode-hook 'smartparens-strict-mode)

;; clojure
(cider-auto-test-mode 1)

(defun clojure-keybinds ()
  (progn (local-set-key (kbd "M-RET") #'cider-eval-sexp-at-point)
	 (local-set-key (kbd "C-M-RET") #'cider-eval-buffer) 
	 (local-set-key (kbd "M-i") #'cider-inspect-last-result)))

(add-hook 'clojure-mode-hook 'lsp)
(add-hook 'clojure-mode-hook 'smartparens-strict-mode)
(add-hook 'clojure-mode-hook #'clojure-keybinds)
(setq cider-shadow-cljs-command "shadow-cljs")

;; ligatures
(ligature-set-ligatures 'prog-mode '(":::" ":=" "!=" "!==" "----" "-->" "->" "->>"
                                     "#{" "#[" "##" "###" "####" "#(" "#?" "#_"
                                     ".." "..." ";;"
                                     "|>"
                                     "++" "+++" "==" "===" "==>" "=>" "=>>" "<="
                                     ">=" ">>" ">>>"
                                     "<|" "<|>" "<-" "<--" "<->"
                                     "<=" "<==" "<=>" "<<" "<<-" "<<<"))
(global-ligature-mode 't)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes '(dracula))
 '(custom-safe-themes
   '("fe1c13d75398b1c8fd7fdd1241a55c286b86c3e4ce513c4292d01383de152cb7" default))
 '(package-selected-packages
   '(evil lsp-mode cider super-save clojure-mode ace-flyspell writeroom-mode visual-fill-column ivy-prescient prescient ivy-rich company which-key magit treemacs-all-the-icons doom-modeline ligature smartparens ivy projectile avy markdown-mode dracula-theme))
 '(tool-bar-mode nil))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "Fira Code" :foundry "CTDB" :slant normal :weight normal :height 162 :width normal)))))
