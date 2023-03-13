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
    smartparens)
  "A list of packages to ensure are installed at launch.")

(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))

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

;; lisp
(add-hook 'emacs-lisp-mode-hook
	  (lambda () (local-set-key (kbd "M-RET")
				    #'eval-last-sexp)))
(add-hook 'emacs-lisp-mode-hook 'smartparens-strict-mode)

;; clojure
(cider-auto-test-mode 1)

(add-hook 'clojure-mode-hook 'lsp)
(add-hook 'clojure-mode-hook 'smartparens-strict-mode)
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
   '(lsp-mode cider super-save clojure-mode ace-flyspell writeroom-mode visual-fill-column ivy-prescient prescient ivy-rich company which-key magit treemacs-all-the-icons doom-modeline ligature smartparens ivy projectile avy markdown-mode dracula-theme))
 '(tool-bar-mode nil))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "Fira Code" :foundry "CTDB" :slant normal :weight normal :height 162 :width normal)))))
