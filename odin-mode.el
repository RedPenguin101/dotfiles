;; based on
;; https://git.sr.ht/~mgmarlow/odin-mode

(require 'js) ; For indentation

(defgroup odin nil
  "Major mode for the Odin programming language."
  :link '(url-link "https://odin-lang.org")
  :group 'languages)

(defconst odin-keywords
  '("import" "foreign" "package"
    "where" "when" "if" "else" "for" "switch" "in" "do" "case"
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

  (setq-local indent-line-function #'js-indent-line)

  (setq-local electric-indent-chars
              (append "{}():;," electric-indent-chars)))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.odin\\'" . odin-mode))

(provide 'odin-mode)

;;; odin-mode.el ends here
