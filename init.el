;; tidy up interface
(scroll-bar-mode -1)
(tool-bar-mode -1)
(menu-bar-mode -1)
(setq inhibit-startup-message t)

;; save backups to /tmp/
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

;; packages
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

(defvar my-packages
  '(dracula-theme avy ivy ligature markdown-mode projectile
    all-the-icons doom-modeline)
  "A list of packages to ensure are installed at launch.")

(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))

;; markdown-mode
(require markdown-mode)
(add-hook 'markdown-mode-hook (lambda () (visual-line-mode t)))
(add-hook 'text-mode-hook (lambda () (visual-line-mode t)))

;; modeline
(require 'doom-modeline)
(doom-modeline-mode 1)
(setq doom-modeline-buffer-encoding nil)

;; parens
(electric-pair-mode 1)
(show-paren-mode 1)

;; recent file mode
(recentf-mode 1)
(setq recentf-max-menu-items 25)
(setq recentf-max-saved-items 25)
(global-set-key "\C-x\ \C-r" 'recentf-open-files)

;; avy
(global-set-key (kbd "C-'") 'avy-goto-char-2)
(global-set-key (kbd "C-\"") 'avy-goto-line)

;; projectile (and ivy)
(ivy-mode +1)
(projectile-mode +1)
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)

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
   '(magit treemacs-all-the-icons doom-modeline ligature smartparens ivy projectile avy markdown-mode dracula-theme))
 '(tool-bar-mode nil))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "Fira Code" :foundry "CTDB" :slant normal :weight normal :height 162 :width normal)))))
