;; Keybinds

(defun move-line-up ()
  (interactive)
  (transpose-lines 1)
  (forward-line -2))

(defun move-line-down ()
  (interactive)
  (forward-line 1)
  (transpose-lines 1)
  (forward-line -1))

(use-global-map (make-sparse-keymap))

(global-set-key (kbd "M-x") 'execute-extended-command)
(global-set-key (kbd "C-g") 'keyboard-quit)

(global-set-key (kbd "DEL") 'backward-delete-char-untabify)
(global-set-key (kbd "<delete>") 'delete-forward-char)
(global-set-key (kbd "RET") 'newline)

(global-set-key (kbd "C-x 1") 'delete-other-windows)
(global-set-key (kbd "C-x 2") 'split-window-below)
(global-set-key (kbd "C-x 3") 'split-window-right)

;; set all keys to self-insert
(let ((c ?\s))
  (while (< c ?\d)
    (global-set-key (vector c) #'self-insert-command)
    (setq c (1+ c)))
  (when (eq system-type 'ms-dos)
    (setq c 128)
    (while (< c 160)
      (global-set-key (vector c) #'self-insert-command)
      (setq c (1+ c))))
  (setq c 160)
  (while (< c 256)
    (global-set-key (vector c) #'self-insert-command)
    (setq c (1+ c))))

(global-set-key (kbd "C-x C-f") 'find-file)
(global-set-key (kbd "C-x C-s") 'save-buffer)
(global-set-key (kbd "C-x C-c") 'save-buffers-kill-terminal)
(global-set-key (kbd "C-x k") 'kill-buffer)
(global-set-key (kbd "C-x b") 'switch-to-buffer)

(global-set-key (kbd "C-u") 'undo)

;; navigation

(global-set-key (kbd "C-h") 'backward-char)
(global-set-key (kbd "C-S-h") 'backward-word)
(global-set-key (kbd "C-l") 'forward-char)
(global-set-key (kbd "C-S-l") 'forward-word)
(global-set-key (kbd "C-j") 'next-line)
(global-set-key (kbd "C-S-j") 'scroll-up-command)
(global-set-key (kbd "C-k") 'previous-line)
(global-set-key (kbd "C-S-k") 'scroll-down-command)

(global-set-key (kbd "C-a") 'move-beginning-of-line)
(global-set-key (kbd "C-s") 'move-end-of-line)

(global-set-key (kbd "C-M-j") 'move-line-down)
(global-set-key (kbd "C-M-k") 'move-line-up)

;; copy/paste

(global-set-key (kbd "C-r") 'isearch-forward)

(global-set-key (kbd "C-SPC") 'set-mark-command)
(global-set-key (kbd "C-q") 'kill-line)
(global-set-key (kbd "C-w") 'kill-region)
(global-set-key (kbd "C-y") 'yank)

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
    clojure-mode cider)
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
;;(add-hook 'markdown-mode-hook (lambda () (writeroom-mode t)))
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
(global-set-key "\C-x\ \C-r" 'recentf-open-files)

;; avy
(global-set-key (kbd "C-'") 'avy-goto-char-2)
(global-set-key (kbd "C-\"") 'avy-goto-line)

;; projectile (and ivy)
(ivy-mode +1)
(projectile-mode +1)
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
(ivy-prescient-mode +1)

;; clojure
(cider-auto-test-mode 1)
(add-hook 'clojure-mode-hook
	  (lambda () (local-set-key (kbd "M-v") #'cider-eval-sexp-at-point)))

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
   '(cider super-save clojure-mode ace-flyspell writeroom-mode visual-fill-column ivy-prescient prescient ivy-rich company which-key magit treemacs-all-the-icons doom-modeline ligature smartparens ivy projectile avy markdown-mode dracula-theme))
 '(tool-bar-mode nil))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "Fira Code" :foundry "CTDB" :slant normal :weight normal :height 162 :width normal)))))
