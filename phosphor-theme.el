;;; phosphor-theme.el --- green CRT phosphor -*- lexical-binding: t; -*-

;; Phosphor: a green-screen CRT as it looked, warm green text on
;; dark green-grey glass, with everything tinted green.  Only two
;; things stand out from the text.  Literals (strings, numbers,
;; true/false and other constants) are bright phosphor, and comments
;; are dim.  Keywords, types, builtins and definitions are
;; deliberately plain text.  UI chrome uses dark tints so it never
;; outshouts the code.

(deftheme phosphor "Green CRT; bright literals, dim comments.")

(let ((bg        "#0e130e")
      (fg        "#94f455") ;; alternatives are #99f060 #9fe870.
      (literal   "#e2f7cf")
      (comment   "#7f8a7c")
      (bg-line   "#151c15")
      (bg-region "#283428")
      (bg-ui     "#1a231a")
      (border    "#283428")
      (dim       "#4a5848")
      (red       "#ff6b6b")
      (amber     "#e8c46a")
      (bg-added  "#0d2a12")
      (bg-removed "#3a1010")
      (bg-added-refine  "#1d5a26")
      (bg-removed-refine "#6a1c1c")
      (bg-match  "#2a3a4a"))
  (custom-theme-set-faces
   'phosphor

   ;; Base
   `(default ((t :background ,bg :foreground ,fg)))
   `(cursor ((t :background ,fg)))
   `(fringe ((t :background ,bg)))
   `(vertical-border ((t :foreground ,border)))
   `(window-divider ((t :foreground ,border)))
   `(shadow ((t :foreground ,comment)))
   `(region ((t :background ,bg-region :extend t)))
   `(secondary-selection ((t :background ,bg-ui :extend t)))
   `(highlight ((t :background ,bg-region)))
   `(hl-line ((t :background ,bg-line :extend t)))
   `(line-number ((t :foreground ,dim)))
   `(line-number-current-line ((t :foreground ,comment)))
   `(minibuffer-prompt ((t :weight bold)))
   `(link ((t :foreground ,literal :underline t)))
   `(link-visited ((t :foreground ,comment :underline t)))
   `(help-key-binding ((t :background ,bg-ui :foreground ,literal)))
   `(error ((t :foreground ,red :weight bold)))
   `(warning ((t :foreground ,amber :weight bold)))
   `(success ((t :foreground ,fg :weight bold)))

   ;; Mode line
   `(mode-line ((t :background ,bg-ui :foreground ,fg)))
   `(mode-line-inactive ((t :background ,bg-line :foreground ,comment)))
   `(mode-line-highlight ((t :background ,bg-region)))
   `(header-line ((t :background ,bg-ui :foreground ,fg)))

   ;; Search and parens
   `(isearch ((t :background ,literal :foreground ,bg :weight bold)))
   `(lazy-highlight ((t :background ,bg-match)))
   `(isearch-fail ((t :background ,bg-removed)))
   `(match ((t :background ,bg-match)))
   `(show-paren-match ((t :background ,bg-match :weight bold)))
   `(show-paren-mismatch ((t :background ,red :foreground ,bg)))

   ;; Syntax: literals and comments are the only colour
   `(font-lock-string-face ((t :foreground ,literal)))
   `(font-lock-number-face ((t :foreground ,literal)))
   `(font-lock-constant-face ((t :foreground ,literal)))
   `(font-lock-escape-face ((t :foreground ,literal :weight bold)))
   `(font-lock-comment-face ((t :foreground ,comment)))
   `(font-lock-doc-face ((t :foreground ,comment)))
   `(font-lock-warning-face ((t :foreground ,amber)))

   ;; Syntax: plain text
   '(font-lock-keyword-face ((t nil)))
   '(font-lock-builtin-face ((t nil)))
   '(font-lock-type-face ((t nil)))
   '(font-lock-function-name-face ((t nil)))
   '(font-lock-variable-name-face ((t nil)))
   '(font-lock-preprocessor-face ((t nil)))
   '(font-lock-negation-char-face ((t nil)))

   ;; Dired
   `(dired-directory ((t :foreground ,literal)))

   ;; Whitespace
   `(whitespace-line ((t :background ,bg-ui)))
   `(whitespace-trailing ((t :background ,bg-removed)))
   `(whitespace-empty ((t :background ,bg-line)))

   ;; Diff
   `(diff-added ((t :background ,bg-added :extend t)))
   `(diff-removed ((t :background ,bg-removed :extend t)))
   `(diff-refine-added ((t :background ,bg-added-refine)))
   `(diff-refine-removed ((t :background ,bg-removed-refine)))
   `(diff-header ((t :background ,bg-ui)))
   `(diff-file-header ((t :background ,bg-ui :weight bold)))
   `(diff-hunk-header ((t :background ,bg-line :foreground ,comment)))

   ;; Outlines and org
   '(outline-1 ((t nil)))
   '(outline-2 ((t nil)))
   '(outline-3 ((t nil)))
   '(outline-4 ((t nil)))
   '(outline-5 ((t nil)))
   '(outline-6 ((t nil)))
   '(outline-7 ((t nil)))
   '(outline-8 ((t nil)))
   `(org-block ((t :background ,bg-line :extend t)))
   `(org-code ((t :foreground ,literal)))
   `(org-verbatim ((t :foreground ,literal)))
   `(org-todo ((t :foreground ,red :weight bold)))
   `(org-done ((t :foreground ,comment :weight bold)))))

(provide-theme 'phosphor)

;;; phosphor-theme.el ends here
