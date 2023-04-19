;;;;;;;;;;;;;;
;; the basics
;;;;;;;;;;;;;;

(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

(setq ring-bell-function 'ignore)
(blink-cursor-mode 0)
(display-battery-mode t)

;; fix temp file creation
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

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

;; This is to remind me of the basic navigations
;; C-h n replaces emacs news
(global-set-key (kbd "C-h n")
		(lambda ()
		  (interactive)
		  (message "    C    M    CM\nfb char word defn\nae line sent sexp")))

(global-set-key (kbd "M-o") 'other-window)
(global-set-key (kbd "C-x C-b") 'ibuffer) ;; replace list-buffer. Suggestion from Mastering Emacs. ibuffer is just better.
(global-set-key (kbd "C-w") 'backward-kill-word) ;; yegge suggestion

(global-set-key (kbd "C-M-y") 'yank) ;; retain tempo when killing sexps with C-M-K

;; potential things to keybind (from Yegge)
;;   beginning and end of buffer
;;   maybe m-x to c/m-xm
;;   comment region
;;   maybe rebind move word from M-fb to C-fb. no single char movement

;;;;;;;;;;;;;;
;; markdown
;;;;;;;;;;;;;;

(setq markdown-fontify-code-blocks-natively t)
(add-hook 'markdown-mode-hook 'visual-line-mode)

;;;;;;;;;;;;;;
;; clojure
;;;;;;;;;;;;;;

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
 '(blink-cursor-mode nil)
 '(custom-enabled-themes '(material-light))
 '(custom-safe-themes
   '("f149d9986497e8877e0bd1981d1bef8c8a6d35be7d82cba193ad7e46f0989f6a" "db86c52e18460fe10e750759b9077333f9414ed456dc94473f9cf188b197bc74" "fe1c13d75398b1c8fd7fdd1241a55c286b86c3e4ce513c4292d01383de152cb7" default))
 '(display-battery-mode t)
 '(menu-bar-mode nil)
 '(package-selected-packages
   '(adaptive-wrap visual-fill-column material-theme markdown-mode dracula-theme cider))
 '(scroll-bar-mode nil)
 '(tool-bar-mode nil))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "Fira Code" :foundry "CTDB" :slant normal :weight normal :height 139 :width normal))))
 '(markdown-code-face ((t (:foreground "#ffb86c" :family "Fira Code"))))
 '(variable-pitch ((t (:height 1.5 :family "ETBookOT")))))
