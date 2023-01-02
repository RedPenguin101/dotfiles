(scroll-bar-mode -1)
(tool-bar-mode -1)
(menu-bar-mode -1)
(setq inhibit-startup-message t)

;; packages
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

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
   '(ligature smartparens ivy projectile avy markdown-mode dracula-theme))
 '(tool-bar-mode nil))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "Fira Code" :foundry "CTDB" :slant normal :weight normal :height 162 :width normal)))))
