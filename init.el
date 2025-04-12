;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Joe's Emacs init.el
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Things to do or try
;;
;; - from https://www.youtube.com/watch?v=51eSeqcaikM
;;   - save place mode
;; - from C.Meier's config https://github.com/gigasquid/emacs-config
;;     (setq make-backup-files nil)
;;     (setq auto-save-default nil)
;; - Shortcut for duplicate line? useful in C duplicate-dwim command
;; - shortcuts for commenting. Especially I would like next-sexp comment
;;   #_ for Clojure
;; - Misc stuff from Batsov https://github.com/bbatsov/emacs.d/blob/master/init.el
;; - avy - stoped using it for some reason, not sure why
;; - Solo (https://www.youtube.com/watch?v=j_2QkCcf8zE, https://github.com/LionyxML/emacs-solo/).
;;    Go through and steal the stuff
;;    (What does narrow to region do?)
;;     - savehist / save-place
;; - Check out https://github.com/adityaathalye/dotemacs/blob/master/init.el
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Basic editor functionality ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Remove most of the initial noisy stuff at startup
(setq inhibit-startup-message t)
(setq initial-scratch-message nil)
(setq ring-bell-function 'ignore)
(setq visible-bell 1)

;; File operations
(setq create-lockfiles nil)
(setq make-backup-files nil)
(setq backup-inhibited t)
(setq delete-by-moving-to-trash t)

;; Autoreverts
(global-auto-revert-mode 1)
(setq global-auto-revert-non-file-buffers t) ; for dired

;; scrolling
(setq pixel-scroll-precision-mode t)
(setq pixel-scroll-precision-use-momentum nil)
(setq scroll-conservatively 101)
(setq scroll-margin 5)

;; line numbers display
(add-hook 'prog-mode-hook 'display-line-numbers-mode)
(setq-default display-line-numbers-type 'relative)

;; Windows and splitting
(setq split-width-threshold 170)     ; So vertical splits are preferred
(setq split-height-threshold nil)
(add-to-list 'initial-frame-alist '(fullscreen . maximized))
(setq frame-resize-pixelwise t)
(setq resize-mini-windows 'grow-only)

;; Editing preferences
(setq delete-selection-mode 1)
(setq kill-do-not-save-duplicates t) ;; doesn't duplicate things in the kill ring

;; Whitespace, tabs and spaces
(setq whitespace-line-column 120)
(setq whitespace-style '(face tabs empty trailing lines-tail))
(add-hook 'prog-mode-hook 'whitespace-mode)
(setq-default indent-tabs-mode nil)
(setq sentence-end-double-space nil)
(setq tab-width 2)

;; parens
(show-paren-mode +1)
(electric-pair-mode)

;; Diffs and version control
(global-hl-line-mode +1)

;; completions
(setq completion-ignore-case t)
(setq completions-detailed t)
;; TAB first tries to indent the current line, and if the line was
;; already indented, then try to complete the thing at point.
(setq tab-always-indent 'complete)

;; FIDO/Minibuffer
;; https://www.masteringemacs.org/article/understanding-minibuffer-completion
(fido-vertical-mode)

;; Savehist
(setq savehist-additional-variables
      '(kill-ring                            ; clipboard
        register-alist                       ; macros
        mark-ring global-mark-ring           ; marks
        search-ring regexp-search-ring))     ; searches
(setq save-place-file (expand-file-name "saveplace" user-emacs-directory))
(setq save-place-limit 600)
(save-place-mode 1)
(desktop-save-mode 1)
(setq desktop-save t)
(setq desktop-load-locked-desktop t)
(setq desktop-restore-frames t)
(setq desktop-auto-save-timeout 300)
(setq desktop-globals-to-save nil)

;; On save, add an option to press d to see a diff
;; C-x s d
(add-to-list 'save-some-buffers-action-alist
             (list "d"
                   (lambda (buffer) (diff-buffer-with-file (buffer-file-name buffer)))
                   "show diff between the buffer and its file"))


;; Stuff I don't have a place for currently
(setq read-answer-short t) ;; always accepts 'y' instead of 'yes'
(setq use-short-answers t)
(setq set-mark-command-repeat-pop t) ;; So we can use C-u C-SPC C-SPC C-SPC... instead of C-u C-SPC C-u C-SPC...

;;;;;;;;;;;
;; dired ;;
;;;;;;;;;;;

(put 'dired-find-alternate-file 'disabled nil)

(use-package wdired
  :ensure nil
  :commands (wdired-change-to-wdired-mode)
  :config
  (setq wdired-allow-to-change-permissions t)
  (setq wdired-create-parent-directories t))

;;;;;;;;;;;;;;;;;;
;; Side windows ;;
;;;;;;;;;;;;;;;;;;

;; (use-package window
;;   :ensure nil
;;   :custom
;;   (display-buffer-alist
;;    '(("\\*\\(Backtrace\\|Warnings\\|Compile-Log\\|Messages\\|Bookmark List\\|Occur\\|eldoc\\)\\*"
;;       (display-buffer-in-side-window)
;;       (window-height . 0.25)
;;       (side . bottom)
;;       (slot . 0))
;;      ("\\*\\([Hh]elp\\)\\*"
;;       (display-buffer-in-side-window)
;;       (window-width . 75)
;;       (side . right)
;;       (slot . 0))
;;      ("\\*\\(Ibuffer\\)\\*"
;;       (display-buffer-in-side-window)
;;       (window-width . 100)
;;       (side . right)
;;       (slot . 1))
;;      ("\\*\\(Flymake diagnostics\\|xref\\|Completions\\)"
;;       (display-buffer-in-side-window)
;;       (window-height . 0.25)
;;       (side . bottom)
;;       (slot . 1))
;;      ("\\*\\(grep\\|find\\)\\*"
;;       (display-buffer-in-side-window)
;;       (window-height . 0.25)
;;       (side . bottom)
;;       (slot . 2)))))

;;;;;;;;;;;;;;;;
;; Mode line
;;;;;;;;;;;;;;;;
;; Minimal mode line with just the file name and the status indicator

;; (setq-default mode-line-format
;;   '("%e" " " mode-line-buffer-identification "%* "))

(let ((bg (face-attribute 'mode-line :background)))
  (set-face-attribute 'mode-line nil
                      :box (list :line-width 4 :color bg :style nil)))

(let ((bg (face-attribute 'mode-line-inactive :background)))
  (set-face-attribute 'mode-line-inactive nil
                      :box (list :line-width 4 :color bg :style nil)))

;; (possibly modified if doom-modeline is enabled further on)

;;;;;;;;;;;;;;;;
;; recent mode
;;;;;;;;;;;;;;;;

(recentf-mode 1)
(setq recentf-max-menu-items 300)
(setq recentf-max-saved-items 15)
;; I used to have this as a separate buffer which opened. But now I
;; just use a mini-buffer with FIDO, per here:
;; https://www.masteringemacs.org/article/find-files-faster-recent-files-package

(global-set-key (kbd "C-x C-r") 'recentf-open-minibuff)

(defun recentf-open-minibuff ()
  "Use `ido-completing-read' to \\[find-file] a recent file"
  (interactive)
  (if (find-file (completing-read "Find recent file: " recentf-list))
      (message "Opening file...")
    (message "Aborting")))

;;;;;;;;;;;;;;;;;
;; mac
;;;;;;;;;;;;;;;;;

;;; Use command key for meta
(when (eq system-type 'darwin)
  (setq mac-option-key-is-meta nil
        mac-command-key-is-meta t
        mac-command-modifier 'meta
        mac-option-modifier 'none)
  ;;; adding brew llvm path
  (add-to-list 'exec-path "/opt/homebrew/opt/llvm/bin")
  (setenv "PATH" (format "%s:%s" "/opt/homebrew/opt/llvm/bin" (getenv "PATH"))))

;;;;;;;;;;;;;;;;;;;;
;; Modal keybinds ;;
;;;;;;;;;;;;;;;;;;;;
;; Largely taken from
;; https://github.com/xahlee/xah-fly-keys/blob/master/xah-fly-keys.el

(load "~/.emacs.d/lisp/modal.el")
(modal-mode 1)

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

;; files
(define-key modal-command-map (kbd "b") 'switch-to-buffer)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; universal keybind changes ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Configuring keybinds in Emacs:
;; - (global-set-key KEY COMMAND)

;; C-m should not be interpreted as RET
(define-key input-decode-map [?\C-m] [C-m])

;; Moves and kill
;; |             | C-         | M-        | C-M- (code) |
;; |:------------|:-----------|:----------|:------------|
;; | a start     | line*      | buff      | defn*       |
;; | e end       | line*      | buff      | defn*       |
;; |             |            |           |             |
;; | h back      | word       | sent      | sexp        |
;; | l fwd       | word       | sent      | sexp        |
;; | j nxt       | line       | pg down   | down-in     |
;; | k prv       | line       | pg up     | up-out      |
;; |:------------|:-----------|:----------|:------------|
;; | ;           | recenter*  |           |             |
;; | SPACE       | set mark*  |           | mark sexp*  |
;; |:------------|:-----------|:----------|:------------|
;; | w kill rng  | kill rgn*  | KR save*  | KR append*  |
;; | n kill back | word       | sentence  | sexp        |
;; | m kill fwd  | word       | sentence  | sexp        |
;; | p kill line | rest line  | back line | whole line  |
;; | y yank      | yank last* | KR Cycle* |             |

(global-set-key (kbd "C-l") 'forward-word) ;; replaces recenter-top-bottom
(global-set-key (kbd "C-h") 'backward-word) ;; replaces help :(

(global-set-key (kbd "C-j") 'next-line) ;; replaces electric-newline-and-maybe-indent
(global-set-key (kbd "C-k") 'previous-line) ;; replaces kill line

(global-set-key (kbd "C-M-l") 'forward-sexp) ;; replaces reposition-window
(global-set-key (kbd "C-M-h") 'backward-sexp) ;; replace mark-defun

(global-set-key (kbd "C-M-k") 'backward-up-list) ;; also default-indent-new-line
(global-set-key (kbd "C-M-j") 'down-list) ;; replaces kill

(global-set-key (kbd "M-e") 'end-of-buffer) ;; replaces fwd sentence
(global-set-key (kbd "M-a") 'beginning-of-buffer) ;; replaces backward sentence

(global-set-key (kbd "M-l") 'forward-sentence) ;; replaces downcase-word
(global-set-key (kbd "M-h") 'backward-sentence) ;; replaces mark-paragraph

(global-set-key (kbd "M-j") 'scroll-up-command) ;; replaces default-indent-new-line
(global-set-key (kbd "M-k") 'scroll-down-command) ;; replaces kill sentence

(global-set-key (kbd "C-w") 'backward-kill-word) ;; replaces kill region
(global-set-key (kbd "C-n") 'backward-kill-word) ;; replaces next line
(global-set-key (kbd "<C-m>") 'kill-word)
(global-set-key (kbd "M-n") 'backward-kill-sentence)
(global-set-key (kbd "M-m") 'kill-sentence) ;; replaces first whitespace
(global-set-key (kbd "C-M-n") 'backward-kill-sexp) ;; replaces fwd list
(global-set-key (kbd "C-M-m") 'kill-sexp) ;; aka M-RET

(defun backward-kill-line ()
  (interactive)
  (kill-line 0))

(global-set-key (kbd "C-p") 'kill-line) ;; replaces prv line.

(global-set-key (kbd "M-p") 'backward-kill-line)
(global-set-key (kbd "C-M-p") 'kill-whole-line) ;; replaces prv line

(global-set-key (kbd "C-M-y") 'yank) ;; for maintaining tempo

;; other stuff

(global-set-key (kbd "M-o") 'other-window)
(global-set-key (kbd "C-d") 'delete-other-windows) ;; replaces transpose char

(global-set-key (kbd "C-f") 'project-find-file)
(global-set-key (kbd "C-b") 'switch-to-buffer) ; maybe project STB?

;; Use this for command, in place of M-x, avoiding the meta stretch.
(global-set-key (kbd "C-x <C-m>") 'execute-extended-command)

;; Rebind
(global-set-key (kbd "C-;") 'recenter-top-bottom)
(global-set-key (kbd "C-'") 'dabbrev-expand)
;(global-set-key (kbd "C-M-SPC") 'set-mark-command) ;for tempo

;(global-set-key (kbd "C-x v p") 'vc-pull)
;; to match C-x v P for push. + is generally pull

(global-set-key (kbd "C-+") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)

;; stop accidentally zooming
(global-set-key (kbd "C-<wheel-up>") nil)
(global-set-key (kbd "C-<wheel-down>") nil)

(global-set-key (kbd "C-c w") 'whitespace-cleanup)

(global-set-key (kbd "C-M-s") 'avy-goto-char-2) ;; replaces regex isearch

;; Keys I always hit accidentally

(global-unset-key (kbd "C-z")) ;; suspend
(global-unset-key (kbd "C-x C-z")) ;; suspend
(global-unset-key (kbd "C-x f")) ;; col fill
(global-unset-key (kbd "C-t")) ;; transpose char
(global-unset-key (kbd "M-c"))   ;; capitalize

;;;;;;;;;;;;;;;;
;; compilation
;;;;;;;;;;;;;;;;
;; Taken from emacs-solo

(use-package compile
  :ensure nil
  :custom
  (compilation-always-kill t)
  (compilation-scroll-output t)
  (ansi-color-for-compilation-mode t)
  :config
  (add-hook 'compilation-filter-hook #'ansi-color-compilation-filter))

;;;;;;;;;;;;;;;;;;;;;;;;;;
;; None native packages ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;

(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

(use-package which-key
  :config
  (which-key-mode))

(use-package diff-hl
  :config
  (global-diff-hl-mode))

;; (use-package doom-modeline
;;   :config
;;   (doom-modeline-mode 1))

;;;;;;;;;;;;;;
;; markdown
;;;;;;;;;;;;;;

(setq markdown-fontify-code-blocks-natively t)
;(setq markdown-hide-markup t)
(setq markdown-max-image-size '(1500 . 1500))
(add-hook 'markdown-mode-hook 'visual-line-mode)
(add-hook 'markdown-mode-hook 'auto-fill-mode)

;;;;;;;;;;;;;;;;;;;;;;;
;; clojure (and elisp)
;;;;;;;;;;;;;;;;;;;;;;;

;; other options for sexps to think about using smart-parens
;; - wrapping / unwrapping
;; - slurp / barf
;; - transpose
;; - kill

(add-hook 'emacs-lisp-mode-hook
          (lambda () (electric-pair-local-mode)))

(add-hook 'clojure-mode-hook 'subword-mode 'electric-pair-mode)
(add-hook 'cider-mode-hook
          (lambda () (local-set-key (kbd "C-c f") 'cider-format-defun)))

;; builtin cider commands:
;;   C-c C-p cider-pprint-eval-last-sexp
;;   C-c C-e cider-eval-last-sexp
;;   C-c C-c cider-eval-defun-at-point (defun = top level)
;;   C-c C-f cider-pprint-eval-defun-at-point

;;;;;;;;;;;;;;;;;;;;;;;
;; C
;;;;;;;;;;;;;;;;;;;;;;;

;; create a format file with
;; clang-format -style=llvm -dump-config > .clang-format
(setq clang-format-style "file")
(setq clang-format-fallback-style "llvm")

(add-hook 'c-mode-hook
          (lambda ()
            (progn
              (electric-pair-local-mode)
              (c-toggle-comment-style -1)
              (local-set-key (kbd "C-M-h") 'backward-sexp)
              (local-set-key (kbd "C-c C-c") 'recompile)
              (local-set-key (kbd "C-c f") 'clang-format-buffer)
              (local-set-key (kbd "C-d") 'delete-other-windows))))

(add-hook 'c++-mode-hook
          (lambda ()
            (progn
              (electric-pair-local-mode)
              (c-toggle-comment-style -1)
              (local-set-key (kbd "C-M-h") 'backward-sexp)
              (local-set-key (kbd "C-c C-c") 'recompile)
              (local-set-key (kbd "C-c f") 'clang-format-buffer)
              (local-set-key (kbd "C-d") 'delete-other-windows))))


; best way to hook LSP up properly is to use bear
; (https://github.com/rizsotto/Bear) to generate a
; compile_commands.json file (just run `bear -- make`) clangd will
; pick up on that automatically then just `eglot` to launch the lsp

;;;;;;;;;;;;;;;;;;;;;;;;
;; Odin
;;;;;;;;;;;;;;;;;;;;;;;;

;; Mode is here https://git.sr.ht/~mgmarlow/odin-mode
;; install with M-x package-vc-install RET https://git.sr.ht/~mgmarlow/odin-mode

(use-package odin-mode
  :bind (:map odin-mode-map
              ("C-c C-c" . 'recompile)))

(add-hook 'c++-mode-hook 'subword-mode)


(with-eval-after-load 'eglot
  (add-to-list 'eglot-server-programs
               '(odin-mode . ("ols"))))

;;;;;;;;;;;;;;;;;;;;;;;;
;; Common lisp and Slime
;;;;;;;;;;;;;;;;;;;;;;;;

(load (expand-file-name "~/.quicklisp/slime-helper.el"))
(setq inferior-lisp-program "sbcl")

;;;;;;;;;;;;;;;;;;;;;;;;
;; Ag
;;;;;;;;;;;;;;;;;;;;;;;;

(setq ag-reuse-buffers 't)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; automatically generated config
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq custom-file (concat user-emacs-directory "custom.el"))
(load custom-file 'noerror 'nomessage)
