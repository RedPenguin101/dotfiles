;; A simple modal / command system for emacs
;; `command-mode-init': initializes the command mode. `C-SPC' from insert mode
;; `insert-mode-init':  initializes the insert mode. `SPC-SPC' from command mode
;; `define-command-keys': creates command keybinding. Expects an alist
;; `define-leader-keys':  creates leader keybinds. The leader prefix key is `SPC'
;; `define-project-keys': creates project keybinds. The project prefix keys is `p'
;;
;; Setup and installation
;; - load the file
;; - add 'modal-mode hooks for proj-mode (and text-mode)
;; - define your command, leader and project keys

(defgroup modal-mode nil
  "A simple modal mode."
  :group 'keyboard)

(defvar modal-key-map     (make-sparse-keymap))
(defvar modal-command-map (make-sparse-keymap))
(defvar modal-insert-map  (make-sparse-keymap))
(defvar modal-command-submap-leader (define-prefix-command 'modal-leader-command))
(defvar modal-command-submap-project (define-prefix-command 'modal-leader-project))

(defvar modal-command-submap-sexp (make-sparse-keymap))
(set-keymap-parent modal-command-submap-sexp modal-command-map)

;; Keybinds
;;;;;;;;;;;;;;

(dotimes (i 26)
  (define-key modal-command-map (char-to-string (+ ?a i)) 'ignore))

(dotimes (i 26)
  (define-key modal-command-map (char-to-string (+ ?A i)) 'ignore))

(define-key modal-insert-map (kbd "C-SPC") #'command-mode-init)

(defun map-over-keys (keymap keys-alist)
  (mapcar (lambda (x) (define-key keymap (kbd (car x)) (cdr x))) keys-alist))

(defun define-command-keys (keys-alist)
  (map-over-keys modal-command-map keys-alist))

(define-command-keys
 '(("SPC" . modal-leader-command)
   ("p"   . modal-leader-project)))

(defun define-leader-keys (keys-alist)
  (map-over-keys modal-command-submap-leader keys-alist))

(define-leader-keys
 '(("SPC" . insert-mode-init)))

(defun define-project-keys (keys-alist)
  (map-over-keys modal-command-submap-project keys-alist))

(defun define-sexp-mode-overrides (keys-alist)
  (map-over-keys modal-command-submap-sexp keys-alist))

;; Activation
;;;;;;;;;;;;;;;

(defvar modal-mode-sexp-flag nil)

(defun modal-mode-sexp-override ()
  (if modal-mode
      (progn
        (setq-local modal-mode-sexp-flag t)
        (command-mode-init))))

(defun update-key-map (mode)
  (set-keymap-parent modal-key-map
                     (pcase mode
                       ('insert modal-insert-map)
                       ('command modal-command-map)
                       ('command-sexp modal-command-submap-sexp))))

(defun command-mode-init ()
  (interactive)
  (update-key-map (if modal-mode-sexp-flag 'command-sexp 'command))
  (setq cursor-type 'box))

(defun insert-mode-init ()
  (interactive)
  (update-key-map 'insert)
  (setq cursor-type 'bar))

(define-minor-mode modal-mode
  "My modal mode"
  :lighter " Modal"
  :keymap modal-key-map
  (if modal-mode (command-mode-init)))

(provide 'modal-mode)
