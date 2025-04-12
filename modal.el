;; TODO
;; - when major mode is lispy, switch move keymaps to sexp versions
;; - space leader and keybinds

(defvar current-mode 'insert)

(defvar command-mode-indicator "c")
(defvar insert-mode-indicator "i")

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

(defun map-over-keys (keymap keys-alist)
  (mapcar
   (lambda (x) (define-key keymap (kbd (car x)) (cdr x)))
   keys-alist))

(defun define-command-keys (keys-alist)
  (map-over-keys modal-command-map keys-alist))

(define-command-keys
 '(("f" . insert-mode-init)
   ("SPC" . modal-leader-command)))

(defun define-leader-keys (keys-alist)
  (map-over-keys (define-prefix-command 'modal-leader-command) keys-alist))

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
  (setq cursor-type 'box)
  ;;(setq mode-line-front-space command-mode-indicator)
  (force-mode-line-update))

(defun insert-mode-init ()
  (interactive)
  (setq current-mode 'insert)
  (update-key-map)
  (setq cursor-type 'bar)
  ;;(setq mode-line-front-space insert-mode-indicator)
  (force-mode-line-update))

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
