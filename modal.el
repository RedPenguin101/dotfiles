(defvar command-mode-indicator "c")
(defvar insert-mode-indicator "i")

(defvar my-modal-key-map     (make-sparse-keymap))
(defvar my-modal-command-map (make-sparse-keymap))
(defvar my-modal-insert-map  (make-sparse-keymap))

(define-key my-modal-insert-map  (kbd "<escape>") #'command-mode-init)
(define-key my-modal-command-map (kbd "f") #'insert-mode-init)

(define-key my-modal-insert-map  (kbd "C-c i") (lambda () (interactive) (message "In Insert Mode")))
(define-key my-modal-command-map (kbd "C-c c") (lambda () (interactive) (message "In Command Mode")))

(defvar current-mode 'insert)

(defun update-key-map ()
  (set-keymap-parent my-modal-key-map
                     (pcase current-mode
                       ('insert my-modal-insert-map)
                       ('command my-modal-command-map))))

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

(define-minor-mode my-modal-mode
  "My modal mode"
  :global t
  :lighter "modal"
  :keymap my-modal-key-map
  (insert-mode-init)
  )

(my-modal-mode 1)

;;(command-mode-init)
;;(current-active-maps)
;;(insert-mode-init)
