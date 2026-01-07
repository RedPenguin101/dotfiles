;; some bits taken from here https://github.com/rexim/simpc-mode

(defgroup simple-c nil
  "Major mode for the SIMPLE-C programming language."
  :link '(url-link "https://simple-c-lang.org")
  :group 'languages)

(defconst simple-c-keywords
  '("const" "static" "extern"
	"if" "else" "while" "for" "switch" "case" "do" "goto"
    "break" "continue" "return"
    "sizeof"
	"enum" 	"union" "struct" "typedef"
    "true" "false"))

(defconst simple-c-builtins
  '("min" "max" "abs"))

(defconst simple-c-types
  '("char" "int" "long" "short" "void" "bool" "float" "double" "signed" "unsigned"
    "char16_t" "char32_t" "char8_t"
    "int8_t" "uint8_t" "int16_t" "uint16_t" "int32_t" "uint32_t" "int64_t" "uint64_t"
    "uintptr_t"
    "size_t"))

(defvar simple-c-mode-syntax-table
  (let ((table (make-syntax-table)))
    ;; Operators
    (dolist (i '(?: ?+ ?- ?* ?= ?< ?> ?& ?| ?^ ?! ??))
      (modify-syntax-entry i "." table))

    ;; Strings, chars
    (modify-syntax-entry ?\" "\"" table)
    (modify-syntax-entry ?' "\"" table)

    ;; Comments
    (modify-syntax-entry ?/ ". 124b" table)
    (modify-syntax-entry ?* ". 23n" table)
    (modify-syntax-entry ?\n "> b" table)

    table))

(defvar simple-c-font-lock-keywords
  `(
    ;; preproc
    ("^ *#[a-zA-Z]+" . font-lock-preprocessor-face)
	("# *include\\(?:_next\\)?\\s-+\\(\\(<\\|\"\\).*\\(>\\|\"\\)\\)" . (1 font-lock-string-face))

    ;; Keywords
    (,(regexp-opt simple-c-keywords 'symbols) . font-lock-keyword-face)

	;; types
    (,(regexp-opt simple-c-types 'symbols) . font-lock-type-face)
	("\\(?:enum\\|struct\\)\\s-+\\([a-zA-Z0-9_]+\\)" . font-lock-type-face)

    ;; Builtins
    (,(regexp-opt simple-c-builtins 'symbols) . font-lock-builtin-face)))

(defun simple-c-indent-line ()
  (interactive)
  (let ((indent 0)
	(indent-col 0)
	(changed))
    (save-excursion
      (back-to-indentation) ;; establish a starting point at the indendation
      (let* ((depth (car (syntax-ppss)))
	     (indent (if (eq (char-after) ?}) (- depth 1) depth)))

	;; The indent is just the block depth UNLESS the next
	;; character is a closing brace `}'. In that case it's the
	;; depth -1, which will be the same as the opening brace.

	(setq indent-col (* tab-width indent))

	;; delete everything before the current point
	(unless (= (current-column) indent-col)
	  (unless (= 0 (current-column)) (kill-line 0))
	  (indent-to indent-col)
	  (setq changed t))))

    ;; If we're 'in' the indented region, move FORWARD to the indendation
    (when  (< (current-column) indent-col)
      (message "indented: indent %d col %d" indent indent-col)
      (back-to-indentation))))

;;;###autoload
(define-derived-mode simple-c-mode
  prog-mode "SIMPLE-C"
  "Major mode for the SIMPLE-C programming language."
  :group 'simple-c
  :syntax-table simple-c-mode-syntax-table

  (setq-local font-lock-defaults '(simple-c-font-lock-keywords))

  (setq-local parse-sexp-ignore-comments t)
  (setq-local comment-start-skip "\\(//+\\|/\\*+\\)\\s *")
  (setq-local comment-start "/*")
  (setq-local comment-end "*/")

  (setq-local indent-line-function #'simple-c-indent-line)

  (setq-local electric-indent-chars
              (append "{}():;," electric-indent-chars)))

;;;###autoload

(provide 'simple-c-mode)
(add-to-list 'auto-mode-alist '("\\.c\\'" . simple-c-mode))
(add-to-list 'auto-mode-alist '("\\.h\\'" . simple-c-mode))

;;; simple-c-mode.el ends here
