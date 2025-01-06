(deftheme kod
  "Kod theme with enhanced elisp support")

(let ((class '((class color) (min-colors 89)))
      ;; Core colors
      (bg "#222")
      (fg "#eee")
      (cursor-color "#ff3319")
      (selection-bg "#2F3F52")
      ;; Syntax colors
      (comment-color "#666")
      (keyword-color "#66c8ef")
      (type-color "lightsalmon")
      (usertype-color "#CAC059")
      (classname-color "#eb7962")
      (string-fg "#9aca7e")
      (string-bg "#212A24")
      (specialchar-fg "#C0D164")
      (specialchar-bg "#2B2F26")
      (regexp-fg "#FFB14B")
      (regexp-bg "#342C22")
      (number-color "#C969B6")
      (preproc-color "#9C8B7C")
      (symbol-color "#A19DBF")
      (function-fg "#85FFDF")
      (function-bg "#1F2B31")
      (bracket-color "#ddd")
      (variable-color "#cda869")
      (constant-color "#FFB14B")
      (warning-color "#946B57")
      ;; UI colors - Added missing border-color!
      (border-color "#444"))

  (custom-theme-set-faces
   'kod
   
   ;; Basic faces
   `(default ((,class (:background ,bg :foreground ,fg))))
   `(cursor ((,class (:background ,cursor-color))))
   `(region ((,class (:background ,selection-bg))))
   `(fringe ((,class (:background ,bg))))
   `(vertical-border ((,class (:foreground ,border-color))))
   
   ;; Enhanced elisp faces
   `(font-lock-comment-face ((,class (:foreground ,comment-color :slant italic))))
   `(font-lock-comment-delimiter-face ((,class (:foreground ,comment-color))))
   `(font-lock-string-face ((,class (:foreground ,string-fg :background ,string-bg))))
   `(font-lock-doc-face ((,class (:foreground ,string-fg :slant italic))))
   
   ;; Important elisp keywords
   `(font-lock-keyword-face ((,class (:foreground ,keyword-color :weight bold))))
   `(font-lock-builtin-face ((,class (:foreground ,keyword-color))))
   `(font-lock-function-name-face ((,class (:foreground ,function-fg :background ,function-bg))))
   `(font-lock-variable-name-face ((,class (:foreground ,variable-color))))
   `(font-lock-type-face ((,class (:foreground ,type-color))))
   `(font-lock-constant-face ((,class (:foreground ,constant-color))))
   
   ;; Special elisp faces
   `(escape-glyph ((,class (:foreground ,specialchar-fg))))
   `(minibuffer-prompt ((,class (:foreground ,keyword-color :weight bold))))
   
   ;; Parenthesis matching
   `(show-paren-match ((,class (:background ,selection-bg :foreground ,fg :weight bold))))
   `(show-paren-mismatch ((,class (:background ,warning-color :foreground ,fg))))
   
   ;; Additional faces for better elisp support
   `(elisp-shorthand-font-lock-face ((,class (:inherit font-lock-keyword-face))))
   `(font-lock-warning-face ((,class (:foreground ,warning-color :weight bold))))
   
   ;; Special content markers
   `(font-lock-preprocessor-face ((,class (:foreground ,preproc-color))))
   `(font-lock-regexp-grouping-construct ((,class (:foreground ,regexp-fg :background ,regexp-bg))))
   `(font-lock-regexp-grouping-backslash ((,class (:foreground ,regexp-fg :background ,regexp-bg))))
   
   ;; Mode line
   `(mode-line ((,class (:background "#333" :foreground ,fg))))
   `(mode-line-inactive ((,class (:background "#222" :foreground ,comment-color))))))

(provide-theme 'kod)
