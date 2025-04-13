;; A simple modal / command system for emacs
;; `define-modal-command-keys': creates command keybinding. Expects an alist
;; `define-modal-leader-keys':  creates leader keybinds. The leader prefix key is `SPC'
;; `define-modal-project-keys': creates project keybinds. The project prefix keys is `p'
;; `define-modal-sexp-overrides': creates bindings that override commands in sexp mode
;;
;; Setup and installation
;; - load the file
;; - add 'modal-mode hooks for proj-mode (and text-mode)
;; - define your command, leader, project and sexp-override keys
;; - for any modes you want to use sexp bindings, add the hook `modal-mode-sexp'

(defgroup modal-mode nil
  "A simple modal mode."
  :group 'keyboard)

(defvar modal-mode--main-keymap (make-sparse-keymap))
(defvar modal-mode--insert-keymap  (make-sparse-keymap))

(defvar modal-mode--command-keymap (make-sparse-keymap))
(defvar modal-mode--command-sexp-keymap (make-sparse-keymap))
(set-keymap-parent modal-mode--command-sexp-keymap modal-mode--command-keymap)

(defvar modal-mode--command-leader-subkeymap
  (define-prefix-command 'modal-leader-command))

(defvar modal-mode--command-project-subkeymap
  (define-prefix-command 'modal-leader-project))

;; Keybinds
;;;;;;;;;;;;;;

(dotimes (i 26)
  (define-key modal-mode--command-keymap (char-to-string (+ ?a i)) 'ignore))

(dotimes (i 26)
  (define-key modal-mode--command-keymap (char-to-string (+ ?A i)) 'ignore))

(defun modal-mode--map-over-keys (keymap keys-alist)
  (mapcar (lambda (x) (define-key keymap (kbd (car x)) (cdr x))) keys-alist))

(defun define-modal-command-keys (keys-alist)
  (modal-mode--map-over-keys modal-mode--command-keymap keys-alist))

(defun define-modal-leader-keys (keys-alist)
  (modal-mode--map-over-keys modal-mode--command-leader-subkeymap keys-alist))

(defun define-modal-project-keys (keys-alist)
  (modal-mode--map-over-keys modal-mode--command-project-subkeymap keys-alist))

(defun define-modal-sexp-overrides (keys-alist)
  (modal-mode--map-over-keys modal-mode--command-sexp-keymap keys-alist))

(define-key modal-mode--insert-keymap (kbd "C-SPC") #'modal-mode--command-mode-init)

(define-modal-command-keys
 '(("SPC" . modal-leader-command)
   ("p"   . modal-leader-project)))

(define-modal-leader-keys
 '(("SPC" . modal-mode--insert-mode-init)))

;; Activation
;;;;;;;;;;;;;;;

(defvar modal-mode--sexp-flag nil)

(defun modal-mode-sexp ()
  (if modal-mode
      (progn
        (setq-local modal-mode--sexp-flag t)
        (modal-mode--command-mode-init))))

(defvar-local modal-mode--active-map nil)

(defun modal-mode--update-key-map (mode)
  "Set up the correct keymap based on current mode."
  (setq modal-mode--active-map
        (pcase mode
          ('command      modal-mode--command-keymap)
          ('insert       modal-mode--insert-keymap)
          ('command-sexp modal-mode--command-sexp-keymap)))
  (setq-local minor-mode-overriding-map-alist
              `((modal-mode . ,modal-mode--active-map))))

(defun modal-mode--command-mode-init ()
  (interactive)
  (modal-mode--update-key-map (if modal-mode--sexp-flag 'command-sexp 'command))
  (setq cursor-type 'box))

(defun modal-mode--insert-mode-init ()
  (interactive)
  (modal-mode--update-key-map 'insert)
  (setq cursor-type 'bar))

(define-minor-mode modal-mode
  "My modal mode"
  :lighter " Modal"
  :keymap modal-mode--main-keymap
  (if modal-mode (modal-mode--command-mode-init)))

(provide 'modal-mode)
