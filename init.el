;;;;;;;;;;;;;;
;; the basics
;;;;;;;;;;;;;;

(setq ring-bell-function 'ignore)

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
;;   maybe rebind move work from M-fb to C-fb. no single char movement 

;;;;;;;;;;;;;;
;; markdown
;;;;;;;;;;;;;;

(setq markdown-fontify-code-blocks-natively t)
(add-hook 'markdown-mode-hook 'visual-line-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; automatically generated config
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes '(dracula))
 '(custom-safe-themes
   '("fe1c13d75398b1c8fd7fdd1241a55c286b86c3e4ce513c4292d01383de152cb7" default))
 '(package-selected-packages '(cider clojure-mode markdown-mode dracula-theme)))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "Fira Code" :foundry "CTDB" :slant normal :weight normal :height 139 :width normal)))))
