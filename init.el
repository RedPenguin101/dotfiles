;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Joe's Emacs init.el
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Things to do or try
;;
;; - dumb-jump:a package which jumps from symbol to definition without
;;   being language specific
;; - a completion frontend. The built in is pretty terrible. Ivy, maybe.
;; - Org mode - maybe. Big commitment
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


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

;; dired-x enables `dired-do-find-marked-files` with `F`, which opens
;; all marked files in new windows
(require 'dired-x)

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

;; This is to remind me of the basic navigations C-h n replaces emacs
;; news
(global-set-key (kbd "C-h n")
		(lambda ()
		  (interactive)
		  (message "    C    M    CM\nfb char word defn\nae line sent sexp")))

(global-set-key (kbd "M-o") 'other-window)

;; replaces the default list-buffer with the more usable
;; ibuffer. Suggestion from Mastering emacs
(global-set-key (kbd "C-x C-b") 'ibuffer)

;; A Yegge suggestion to rebind backward-kill-word. The default
;; bindings of this are C/M-<backspace>/<del>. This replaces the
;; occasionally useful kill-region, which I should proabably bind to
;; something else, but haven't needed to yet.
(global-set-key (kbd "C-w") 'backward-kill-word)

;; This allows retention of tempo when kill-yanking sexps
(global-set-key (kbd "C-M-y") 'yank)

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
(add-hook 'markdown-mode-hook 'visual-fill-column-mode)
(add-hook 'markdown-mode-hook 'adaptive-wrap-prefix-mode)

;;;;;;;;;;;;;;
;; org mode - uh oh
;;;;;;;;;;;;;;

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
 '(display-time-mode t)
 '(line-number-mode nil)
 '(package-selected-packages
   '(nov diff-hl adaptive-wrap visual-fill-column material-theme markdown-mode dracula-theme cider))
 '(scroll-bar-mode nil)
 '(tool-bar-mode nil))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "Fira Code" :foundry "CTDB" :slant normal :weight normal :height 139 :width normal))))
 '(fixed-pitch ((t (:family "Fira Code"))))
 '(markdown-code-face ((t (:foreground "#ffb86c" :family "Fira Code"))))
 '(markdown-inline-code-face ((t (:foreground "#ffb86c" :family "Fira Code" :height 0.8))))
 '(org-block ((t (:inherit fixed-pitch :extend t :background "#EFEBE9" :foreground "#212121"))))
 '(org-code ((t (:inherit fixed-pitch :background "#EFEBE9" :foreground "#212121" :height 1.2))))
 '(org-level-1 ((t (:inherit font-lock-function-name-face :extend nil :background "--" :box nil :weight bold :height 1.3))))
 '(org-level-2 ((t (:inherit font-lock-function-name-face :extend nil :background "--" :box nil :weight bold :height 1.2))))
 '(org-level-3 ((t (:inherit font-lock-function-name-face :extend nil :weight bold :height 1.1))))
 '(org-level-4 ((t (:inherit nil :extend nil :height 1.0))))
 '(org-meta-line ((t (:inherit fixed-pitch :height 0.6))))
 '(org-table ((t (:inherit fixed-pitch :background "#e0f7fa" :foreground "#1565c0"))))
 '(org-verbatim ((t (:height 0.8 :family "Fira Code"))))
 '(variable-pitch ((t (:height 1.5 :family "ETBookOT")))))
