(defvar command-mode-indicator "c")
(defvar insert-mode-indicator "i")

(defvar modal-key-map     (make-sparse-keymap))
(defvar modal-command-map (make-sparse-keymap))
(defvar modal-insert-map  (make-sparse-keymap))

;; Keybinds
;;;;;;;;;;;;;;

(define-key modal-insert-map  (kbd "<escape>") #'command-mode-init)
(define-key modal-command-map (kbd "f") #'insert-mode-init)

(define-key modal-insert-map  (kbd "C-c i") (lambda () (interactive) (message "In Insert Mode")))
(define-key modal-command-map (kbd "C-c c") (lambda () (interactive) (message "In Command Mode")))

;; Command mode commands
;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar current-mode 'insert)

(define-key modal-command-map (kbd "a") 'execute-extended-command)

;; Moves

(define-key modal-command-map (kbd "k") 'previous-line)
(define-key modal-command-map (kbd "j") 'next-line)
(define-key modal-command-map (kbd "h") 'backward-word)
(define-key modal-command-map (kbd "l") 'forward-word)

(define-key modal-command-map (kbd "u") 'move-beginning-of-line)
(define-key modal-command-map (kbd "p") 'move-end-of-line)

(define-key modal-command-map (kbd "'") 'recenter-top-bottom)

;; Windows
(define-key modal-command-map (kbd "o") 'other-window)
(define-key modal-command-map (kbd "3") 'split-window-right)
(define-key modal-command-map (kbd "2") 'split-window-below)
(define-key modal-command-map (kbd "1") 'delete-other-windows)

;; Edits
(define-key modal-command-map (kbd "q") 'fill-paragraph)

;; CUA
(define-key modal-command-map (kbd "z") 'undo)
(define-key modal-command-map (kbd "x") 'kill-region)
(define-key modal-command-map (kbd "c") 'kill-ring-save)
(define-key modal-command-map (kbd "v") 'yank)

;; Activation
;;;;;;;;;;;;;;;

(defun update-key-map ()
  (set-keymap-parent modal-key-map
                     (pcase current-mode
                       ('insert modal-insert-map)
                       ('command modal-command-map))))

(defun command-mode-init ()
  (interactive)
  (setq current-mode 'command)
  (update-key-map)
  (setq mode-line-front-space command-mode-indicator)
  (force-mode-line-update)
  (message "Command mode"))

(defun insert-mode-init (&optional no-indication)
  (interactive)
  (setq current-mode 'insert)
  (update-key-map)
  (setq mode-line-front-space insert-mode-indicator)
  (force-mode-line-update)
  (message "insert mode"))

(define-minor-mode modal-mode
  "My modal mode"
  :global t
  :lighter " Modal"
  :keymap modal-key-map
  (if modal-mode
      (progn
        (add-hook 'minibuffer-setup-hook 'insert-mode-init)
        (add-hook 'minibuffer-exit-hook 'command-mode-init)
        (command-mode-init))
    (progn
      (remove-hook 'minibuffer-setup-hook 'insert-mode-init)
      (remove-hook 'minibuffer-exit-hook 'command-mode-init)
      (setq mode-line-front-space '(:eval (if (display-graphic-p) " " "-")))
      (force-mode-line-update))))

(provide 'modal-mode)

(modal-mode 1)
;;(modal-mode 0)
