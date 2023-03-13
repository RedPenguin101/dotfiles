(setq exec-path (append exec-path '("/home/joe/.nvm/versions/node/v18.14.2/bin")))
(setenv "PATH" (concat (getenv "PATH") ":/home/joe/.nvm/versions/node/v18.14.2/bin"))

;; Keybinds
;; CTRL
;; number-line=windows. 1:close 2/3 split, 0:switch
;; tab-line=kill: q:line w:region r:isearch y:yank u:undo
;; caps-line=navigate: a:start s:end h/j/k/l: move
;; shift-line=other: b:switch-buffer

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

(global-set-key (kbd "C-1") 'delete-other-windows)
(global-set-key (kbd "C-2") 'split-window-below)
(global-set-key (kbd "C-3") 'split-window-right)
(global-set-key (kbd "C-0") 'other-window)

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
(global-set-key (kbd "C-b") 'switch-to-buffer)

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

(global-set-key (kbd "C-<") 'beginning-of-buffer)
(global-set-key (kbd "C->") 'end-of-buffer)

(global-set-key (kbd "C-a") 'move-beginning-of-line)
(global-set-key (kbd "C-s") 'move-end-of-line)

(global-set-key (kbd "C-M-j") 'move-line-down)
(global-set-key (kbd "C-M-k") 'move-line-up)

;; sexp-nav
(global-set-key (kbd "M-k") 'backward-sexp)
(global-set-key (kbd "M-h") 'backward-sexp)
(global-set-key (kbd "M-j") 'forward-sexp)
(global-set-key (kbd "M-l") 'forward-sexp)
(global-set-key (kbd "M-q") 'kill-sexp)

;; copy/paste

(global-set-key (kbd "C-r") 'isearch-forward)

(global-set-key (kbd "C-SPC") 'set-mark-command)
(global-set-key (kbd "C-q") 'kill-line)
(global-set-key (kbd "C-w") 'kill-region)
(global-set-key (kbd "C-y") 'yank)
(global-set-key (kbd "M-y") 'yank) ;; means you don't have to switch modifier after M-q (kill sexp)

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
(global-set-key "\C-x\ \C-r" 'recentf-open-files)

;; avy
(global-set-key (kbd "C-'") 'avy-goto-char-2)
(global-set-key (kbd "C-\"") 'avy-goto-line)

;; projectile (and ivy)
(ivy-mode +1)
(projectile-mode +1)
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
(define-key projectile-mode-map (kbd "C-b") 'projectile-find-file)
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
         (local-set-key (kbd "M-(") #'sp-wrap-round)
	 (local-set-key (kbd "M-9") #'sp-wrap-round)
	 (local-set-key (kbd "M-{") #'sp-wrap-curly)
	 (local-set-key (kbd "M-,") #'sp-forward-barf-sexp)
	 (local-set-key (kbd "M-.") #'sp-forward-slurp-sexp)))

;; lisp
(add-hook 'emacs-lisp-mode-hook
	  (lambda () (local-set-key (kbd "M-RET")
				    #'eval-last-sexp)))
(add-hook 'emacs-lisp-mode-hook 'smartparens-strict-mode)
(add-hook 'emacs-lisp-mode-hook #'smartparens-keys)

;; clojure
(cider-auto-test-mode 1)

(defun clojure-keybinds ()
  (progn (local-set-key (kbd "M-RET") #'cider-eval-sexp-at-point)
	 (local-set-key (kbd "C-M-RET") #'cider-eval-buffer) 
	 (local-set-key (kbd "M-i") #'cider-inspect-last-result)))

(add-hook 'clojure-mode-hook 'lsp)
(add-hook 'clojure-mode-hook 'smartparens-strict-mode)
(add-hook 'clojure-mode-hook #'smartparens-keys)
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
   '(lsp-mode cider super-save clojure-mode ace-flyspell writeroom-mode visual-fill-column ivy-prescient prescient ivy-rich company which-key magit treemacs-all-the-icons doom-modeline ligature smartparens ivy projectile avy markdown-mode dracula-theme))
 '(tool-bar-mode nil))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "Fira Code" :foundry "CTDB" :slant normal :weight normal :height 162 :width normal)))))
