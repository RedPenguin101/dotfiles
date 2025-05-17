;; 'modal-mode' is a minimalist emacs minor-mode which provides a
;; 'command-layer' on top of the standard emacs. In any mode where
;; modal-mode is a hook - for example with.
;;
;; (add-hook 'prog-mode-hook 'modal-mode)
;;
;; You can drop into the command layer with `C-g'. From the command
;; layer you can get back into normal mode with `i'.
;;
;; In command layer the alphanumeric keys will not insert characters,
;; but will do whatever commands you set. You can set the keys with,
;; for example, with the following settings, pressing "a" while in the
;; command layer will execute the command `move-beginning-of-line'.
;;
;; (define-modal-command-keys
;;   '(("a" . move-beginning-of-line))
;;     ("d" . down-list)
;;      ...)
;;
;; In addition to the basic command layer, there are 5 sub-layers
;; intended to provide specialized functionality.
;;
;; - general (leader key: `SPC')
;; - kill (`k')
;; - search (`s')
;; - project (`SPC-p')
;; - eval (`z')
;;
;; In the following example, `k-l' will kill the whole line
;;
;; (define-modal-kill-keys
;;   '(("l" . kill-whole-line)
;;     ...)
;;
;; `modal-mode' is pretty unopinionated about what your keybinds
;; should actually be. The only keys modal-mode itself maps in the
;; command layer itself are:
;;
;; - `i': drop to normal mode
;; - the leader keys `SPC', `s', `k'
;; - universal and negative argument keys, `u' and `-' respectively
;; - The digit keys as `digit-argument'
;;
;; Every other alpha key is unmapped (ignored). Most punctation keys,
;; especially brackets, are NOT unmapped, and will insert as normal
;;
;; The only thing mapped in normal mode is `C-g' to get to the command
;; layer
;;
;; Everything else is up to you, however you can find suggested
;; keymaps at the bottom of this file.

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

(defvar modal-mode--command-project-subkeymap
  (define-prefix-command 'modal-project-leader))

(defvar modal-mode--command-search-subkeymap
  (define-prefix-command 'modal-search-leader))

(defvar modal-mode--command-eval-subkeymap
  (define-prefix-command 'modal-eval-leader))

(defvar modal-mode--command-kill-subkeymap
  (define-prefix-command 'modal-kill-leader))

;; Keybinds
;;;;;;;;;;;;;;

;; Unbind all letter keys in command-mode. It's actually just the
;; letters - so a lot of punctionation, including parens and numbers
;; unless you remap them, will still work in command mode.

(dotimes (i 26)
  (define-key modal-mode--command-keymap (char-to-string (+ ?a i)) 'ignore))

(dotimes (i 26)
  (define-key modal-mode--command-keymap (char-to-string (+ ?A i)) 'ignore))

(dotimes (i 9)
  (define-key modal-mode--command-keymap (char-to-string (+ ?0 i)) 'digit-argument))

(defun modal-mode--map-over-keys (keymap keys-alist)
  (mapcar (lambda (x) (define-key keymap (kbd (car x)) (cdr x))) keys-alist))

;; These are the two function you'll use to define keys

(defun define-modal-command-keys (keys-alist)
  (modal-mode--map-over-keys modal-mode--command-keymap keys-alist))

(defun define-modal-leader-keys (keys-alist)
  (modal-mode--map-over-keys modal-mode--command-leader-subkeymap keys-alist))

(defun define-modal-search-keys (keys-alist)
  (modal-mode--map-over-keys modal-mode--command-search-subkeymap keys-alist))

(defun define-modal-project-keys (keys-alist)
  (modal-mode--map-over-keys modal-mode--command-project-subkeymap keys-alist))

(defun define-modal-eval-keys (keys-alist)
  (modal-mode--map-over-keys modal-mode--command-eval-subkeymap keys-alist))

(defun define-modal-kill-keys (keys-alist)
  (modal-mode--map-over-keys modal-mode--command-kill-subkeymap keys-alist))

(define-key modal-mode--insert-keymap (kbd "C-G") #'modal-mode--command-mode-init)

(define-modal-command-keys
 '(("SPC" . modal-leader-command)
   ("s" . modal-search-leader)
   ("k" . modal-kill-leader)
   ("i" . modal-mode--insert-mode-init)
   ("u" . universal-argument)
   ("-" . negative-argument)
))

(define-modal-leader-keys
 '(("p" . modal-project-leader)))

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

;; A suggested keymap
;;;;;;;;;;;;;;;;;;;;;;;
;; If you're familiar with emacs default keybinds you'll notice that
;; most of these keybinds retain the default semantics, and in a lot
;; of cases just remove the modifier keys. For example `a' is
;; `move-beginning-of-line', which just replaces `C-a' in the
;; defaults.
;;
;; COMMAND-LAYER
;; a: move-beginning-of-line
;; s: <pre-mapped> search layer
;; d: down-list
;; f: forward-word
;; g: set-mark-command
;; w: delete-other-windows
;; q: prog-fill-reindent-defun (or fill-para before 30.1)
;; e: move-end-of-line
;; r: backward-up-list
;; t: up-list
;; z: <unbound>
;; x: execute-extended-command
;; c: switch-to-buffer
;; v: scroll-up-command
;; b: backward-word
;; h: backward-sexp
;; j: forward-sexp
;; k: <pre-mapped> kill layer
;; l: recenter-top-bottom
;; ;: comment-line
;; y: yank
;; u: universal-argument
;; o: other-window
;; p: previous-line
;; n: next-line
;; m: back-to-indentation
;; /: undo
;; ,: beginning-of-defun
;; .: end-of-defun
;; <: beginning-of-buffer
;; >: end-of-buffer
;; -: negative-argument
;; numbers 1-9: digit-argument
;;
;; GENERAL LEADER
;; SPC f: find-file
;; SPC s: save-buffer
;; SPC k: kill-buffer
;; SPC d: dired-jump
;; SPC r: string-rectangle
;; SPC w: whitespace-cleanup
;; SPC 1: delete-other-windows
;; SPC 2: split-window-below
;; SPC 3: split-window-right
;;
;; KILL LEADER
;; k d: kill-word
;; k <backspace>: backward-kill-word
;; k j: kill-sexp
;; k h: backward-kill-sexp
;; k k: kill-line
;; k l: kill-whole-line
;; k w: kill-region
;; k q: kill-ring-save
;; k 6: delete-indentation
;; k r: delete-rectangle
;;
;; SEARCH LEADER
;; s s: isearch-forward
;; s r: isearch-backward
;; s o: occur
;; s q: query-replace
;; s h: highlight-phrase
;;
;; PROJECT LEADER
;; SPC p f: project-find-file
;; SPC p k: project-kill-buffers
;; SPC p c: project-compile
;; SPC p d: project-dired
;; SPC p q: project-query-replace-regexp
;; SPC p b: project-switch-to-buffer
;; SPC p s: save-some-buffers
