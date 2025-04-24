;; modal-mode is a minimalist emacs minor-mode which allows you to
;; reduce how much you need to use modifier keys by providing a
;; Vim-like 'command-mode' on top of the regular 'insert-mode'.
;;
;; 'insert-mode' is just your normal emacs keybinds - all your
;; existing keybinds will continue to work. Dropping into
;; 'command-mode' with `C-SPC' will surface a new keymap where you map
;; all your shortcuts. And you can drop back into insert mode with
;; `SPC-SPC'.
;;
;; To set up modal-mode, simply add some hooks and start defining keys
;;
;;   (add-hook 'prog-mode-hook 'modal-mode)
;;
;;   (define-modal-command-keys
;;     '(("a" . execute-extended-command)
;;       ("t" . set-mark-command)
;;         ... etc))
;;
;; SPC acts as a leader key, so you add further commands if required
;;
;;   (define-modal-leader-keys
;;     '(("." . universal-argument)
;;       ("," . negative-argument)
;;         ... etc))
;;
;; `SPC-SPC' (to get from command to insert mode) and `C-SPC' (to go
;; the other way) are the only keybinds modal-mode defines, everything
;; else you define yourself.
;;
;; You can tell which mode you're in by the cursor - line for insert,
;; block for command.
;;
;; My typical command-mode keybinds are at the bottom of the file.

(defgroup modal-mode nil
  "A simple modal mode."
  :group 'keyboard)

;; Keymaps
;;;;;;;;;;;

(defvar modal-mode--main-keymap (make-sparse-keymap))

;; modal-mode has a main keymap and two sub-keymaps which are switched
;; between as you move between insert and command mode.

(defvar modal-mode--insert-keymap  (make-sparse-keymap))
(defvar modal-mode--command-keymap (make-sparse-keymap))

(defvar modal-mode--command-leader-subkeymap
  (define-prefix-command 'modal-leader-command))

;; Keybinds
;;;;;;;;;;;;;;

;; Unbind all letter keys in command-mode. It's actually just the
;; letters - so a lot of punctionation, including parens and numbers
;; unless you remap them, will still work in command mode.

(dotimes (i 26)
  (define-key modal-mode--command-keymap (char-to-string (+ ?a i)) 'ignore))

(dotimes (i 26)
  (define-key modal-mode--command-keymap (char-to-string (+ ?A i)) 'ignore))

(defun modal-mode--map-over-keys (keymap keys-alist)
  (mapcar (lambda (x) (define-key keymap (kbd (car x)) (cdr x))) keys-alist))

;; These are the two function you'll use to define keys

(defun define-modal-command-keys (keys-alist)
  (modal-mode--map-over-keys modal-mode--command-keymap keys-alist))

(defun define-modal-leader-keys (keys-alist)
  (modal-mode--map-over-keys modal-mode--command-leader-subkeymap keys-alist))

(define-key modal-mode--insert-keymap (kbd "C-SPC") #'modal-mode--command-mode-init)

(define-modal-command-keys
 '(("SPC" . modal-leader-command)))

(define-modal-leader-keys
 '(("SPC" . modal-mode--insert-mode-init)))

;; Activation and mode-switching
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar-local modal-mode--active-map nil)

(defun modal-mode--update-key-map (mode)
  "Set up the correct keymap based on current mode."
  (setq modal-mode--active-map
        (pcase mode
          ('command      modal-mode--command-keymap)
          ('insert       modal-mode--insert-keymap)))
  (setq-local minor-mode-overriding-map-alist
              `((modal-mode . ,modal-mode--active-map))))

(defun modal-mode--command-mode-init ()
  (interactive)
  (modal-mode--update-key-map 'command)
  (setq cursor-type 'box))

(defun modal-mode--insert-mode-init ()
  (interactive)
  (modal-mode--update-key-map 'insert)
  (setq cursor-type 'bar))

(define-minor-mode modal-mode
  "Modal mode"
  :lighter " Modal"
  :keymap modal-mode--main-keymap
  (if modal-mode (modal-mode--command-mode-init)))

(provide 'modal-mode)

;; My modal setup
;;;;;;;;;;;;;;;;
;; (load "~/.emacs.d/lisp/modal.el")

;; (add-hook 'prog-mode-hook 'modal-mode)
;; (add-hook 'text-mode-hook 'modal-mode)
;; (add-hook 'conf-mode-hook 'modal-mode)

;; Most of these are just common modified keybinds without the
;; modifier bit.
;;
;; (define-modal-command-keys
;;  '(("j" . scroll-up-command)
;;    ("k" . scroll-down-command)

;;    ("l" . recenter-top-bottom)

;;    ("x" . execute-extended-command)
;;    ("b" . switch-to-buffer)
;;    ("/" . undo)
;;    ("i" . modal-mode--insert-mode-init)
;;    ("a" . avy-goto-char-2)
;;    ("s" . isearch-forward)
;;    ("r" . isearch-backward)

;;    ("o" . other-window)
;;    ("1" . delete-other-windows)
;;    ("2" . split-window-below)
;;    ("3" . split-window-right)))

;; (define-modal-leader-keys
;;  '(("a" . ag-project)
;;    ("o" . occur)
;;    ("f" . find-file)
;;    ("s" . save-buffer)
;;    ("d" . dired-jump)
;;    ("g" . magit-status)
;;    ("w" . whitespace-cleanup)))
