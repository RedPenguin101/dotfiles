(defvar current-mode 'insert)

(defvar command-mode-indicator "c")
(defvar insert-mode-indicator "i")

(defvar modal-key-map     (make-sparse-keymap))
(defvar modal-command-map (make-sparse-keymap))
(defvar modal-insert-map  (make-sparse-keymap))

;; Keybinds
;;;;;;;;;;;;;;

(define-key modal-insert-map  (kbd "<escape>") #'command-mode-init)
(define-key modal-command-map (kbd "f") #'insert-mode-init)

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
