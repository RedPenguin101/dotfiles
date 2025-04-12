(defvar modal-key-map     (make-sparse-keymap))
(defvar modal-command-map (make-sparse-keymap))
(defvar modal-insert-map  (make-sparse-keymap))

;; Keybinds
;;;;;;;;;;;;;;

(dotimes (i 26)
  (define-key modal-command-map (char-to-string (+ ?a i)) 'ignore))

(dotimes (i 26)
  (define-key modal-command-map (char-to-string (+ ?A i)) 'ignore))

(define-key modal-insert-map  (kbd "<escape>") #'command-mode-init)
(define-key modal-insert-map  (kbd "C-SPC") #'command-mode-init)

(defun map-over-keys (keymap keys-alist)
  (mapcar
   (lambda (x) (define-key keymap (kbd (car x)) (cdr x)))
   keys-alist))

(defun define-command-keys (keys-alist)
  (map-over-keys modal-command-map keys-alist))

(define-command-keys
 '(("f"   . insert-mode-init)
   ("SPC" . modal-leader-command)))

(defun define-leader-keys (keys-alist)
  (map-over-keys (define-prefix-command 'modal-leader-command) keys-alist))

;; Activation
;;;;;;;;;;;;;;;

(defun update-key-map (mode)
  (set-keymap-parent modal-key-map
                     (pcase mode
                       ('insert modal-insert-map)
                       ('command modal-command-map))))

(defun command-mode-init ()
  (interactive)
  (update-key-map 'command)
  (setq cursor-type 'box))

(defun insert-mode-init ()
  (interactive)
  (update-key-map 'insert)
  (setq cursor-type 'bar))

(define-minor-mode modal-mode
  "My modal mode"
  :lighter " Modal"
  :keymap modal-key-map
  (if modal-mode
      (progn
        (command-mode-init))
    (progn
      (setq mode-line-front-space '(:eval (if (display-graphic-p) " " "-")))
      (force-mode-line-update))))

(provide 'modal-mode)
