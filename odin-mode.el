;; based on
;; https://git.sr.ht/~mgmarlow/odin-mode

(defgroup odin nil
  "Major mode for the Odin programming language."
  :link '(url-link "https://odin-lang.org")
  :group 'languages)

(defconst odin-keywords
  '("import" "foreign" "package"
    "where" "when" "if" "else" "for" "switch" "in" "not_in" "do" "case"
    "break" "continue" "fallthrough" "defer" "return" "proc"
    "struct" "union" "enum" "bit_field" "bit_set" "map" "dynamic"
    "auto_cast" "cast" "transmute" "distinct" "opaque"
    "using" "inline" "no_inline"
    "size_of" "align_of" "offset_of" "type_of"
    "context"))

(defconst odin-builtins
  '("len" "cap"
    "typeid_of" "type_info_of"
    "swizzle" "complex" "real" "imag" "quaternion" "conj"
    "jmag" "kmag"
    "min" "max" "abs" "clamp"
    "expand_to_tuple"

    "init_global_temporary_allocator"
    "copy" "pop" "unordered_remove" "ordered_remove" "clear" "reserve"
    "resize" "new" "new_clone" "free" "free_all" "delete" "make"
    "clear_map" "reserve_map" "delete_key" "append_elem" "append_elems"
    "append" "append_string" "clear_dynamic_array" "reserve_dynamic_array"
    "resize_dynamic_array" "incl_elem" "incl_elems" "incl_bit_set"
    "excl_elem" "excl_elems" "excl_bit_set" "incl" "excl" "card"
    "assert" "panic" "unimplemented" "unreachable"))

(defvar odin-mode-syntax-table
  (let ((table (make-syntax-table)))
    ;; Operators
    (dolist (i '(?: ?+ ?- ?* ?= ?< ?> ?& ?| ?^ ?! ??))
      (modify-syntax-entry i "." table))

    ;; Strings
    (modify-syntax-entry ?\" "\"" table)
    (modify-syntax-entry ?\\ "\\" table)

    ;; Comments
    (modify-syntax-entry ?/ ". 124b" table)
    (modify-syntax-entry ?* ". 23n" table)
    (modify-syntax-entry ?\n "> b" table)
    (modify-syntax-entry ?\^m "> b" table)

    table))

(defvar odin-font-lock-keywords
  `(
    ;; Keywords
    (,(regexp-opt odin-keywords 'symbols) . font-lock-keyword-face)

    ;; Attributes
    ("^ *@[a-zA-Z]+" . font-lock-preprocessor-face)

    ;; Builtins
    (,(regexp-opt odin-builtins 'symbols) . font-lock-builtin-face)))

(setq odin--proc-enum-or-struct "^\\(.*?\\)\s*::\s*?\\(proc\\|enum\\|struct\\).*$")

(defun odin-imenu-create-index-nested ()
  (let ((enums nil)
		(structs nil)
		(full nil))
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward odin--proc-enum-or-struct nil t)
        (let ((type (match-string-no-properties 2))
			  (name (match-string-no-properties 1))
              (pos (point-marker)))
          (push (cons name pos) (cond ((string-equal type "proc") full)
									  ((string-equal type "enum") enums)
									  ((string-equal type "struct") structs))))))
    (push (cons "ENUMS" (nreverse enums)) full)
    (push (cons "STRUCTS" (nreverse structs)) full)
	full))

(defun odin-indent-line ()
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
    (when (and changed (< (current-column) indent-col))
      ;; (message "indented: indent %d col %d" indent indent-col)
      (back-to-indentation))))

(defun odin--at-defun-start ()
  (let ((current-point (point)))
	(and (= 0 (car (syntax-ppss))) (save-excursion (back-to-indentation) (= current-point (point))))))

(defun odin-beginning-of-defun-function ()
  (let* ((depth (car (syntax-ppss)))
		 (current-point (point))
		 (already-at-start (odin--at-defun-start))
		 (found nil))
	(cond ((odin--at-defun-start)
		   (while (and (> (point) 0) (not found))
			 (previous-line)
			 (beginning-of-line)
			 (when (and (= 0 (car (syntax-ppss))) (looking-at ".*:[:=]?"))
			   (setq found t))))

		  ((= depth 0) (back-to-indentation))

		  (t (progn (backward-up-list depth)
					(back-to-indentation)) ))))

(defun odin-end-of-defun-function ()
  (back-to-indentation)

  (while (eq (char-after) ?\n) (next-line))

  (unless (odin--at-defun-start)
	(odin-beginning-of-defun-function))

  (if (looking-at odin--proc-enum-or-struct)
	  (progn
		(re-search-forward "{")
		(backward-char)
		(forward-sexp))
	(progn
	  (next-line)
	  (back-to-indentation)
	  (while (eq (char-after) ?\n) (next-line)))))

;;;###autoload
(define-derived-mode odin-mode
  prog-mode "Odin"
  "Major mode for the Odin programming language."
  :group 'odin
  :syntax-table odin-mode-syntax-table

  (setq-local font-lock-defaults '(odin-font-lock-keywords))

  (setq-local parse-sexp-ignore-comments t)
  (setq-local comment-start-skip "\\(//+\\|/\\*+\\)\\s *")
  (setq-local comment-start "/*")
  (setq-local comment-end "*/")

  (setq-local indent-line-function #'odin-indent-line)

  (setq-local electric-indent-chars
              (append "{}():;," electric-indent-chars))

  (setq-local beginning-of-defun-function #'odin-beginning-of-defun-function)
  (setq-local end-of-defun-function #'odin-end-of-defun-function)
  (setq-local imenu-create-index-function 'odin-imenu-create-index-nested))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.odin\\'" . odin-mode))

(provide 'odin-mode)

;;; odin-mode.el ends here
