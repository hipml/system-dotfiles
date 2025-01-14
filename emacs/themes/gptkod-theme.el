(deftheme gptkod
  "A custom theme with dark background and vibrant colors.")

(let ((class '((class color) (min-colors 89)))
      ;; Core colors from user's CSS
      (bg "#222")
      (fg "#eee")
      (caret "#ff3319")
      (selection-bg "#2F3F52")
      (column-guide "#444")
      
      ;; Syntax colors from user's CSS
      (keyword "#66c8ef")
      (type "#ffa07a")
      (usertype "#CAC059")
      (classname "#eb7962")
      (string "#9aca7e")
      (string-bg "#212A24")
      (specialchar "#C0D164")
      (specialchar-bg "#2B2F26")
      (regexp "#FFB14B")
      (regexp-bg "#342C22")
      (comment "#666")
      (number "#C969B6")
      (preproc "#9C8B7C")
      (symbol "#A19DBF")
      (function "#85FFDF")
      (function-bg "#1F2B31")
      (cbracket "#ddd")
      (todo "#946B57")
      
      ;; UI colors
      (minibuffer-bg "#333")
      (minibuffer-active-bg "#444"))

  (custom-theme-set-faces
   'gptkod
   
   ;; Basic faces
   `(default ((,class (:background ,bg :foreground ,fg))))
   `(cursor ((,class (:background ,caret))))
   `(region ((,class (:background ,selection-bg))))
   
   ;; Syntax highlighting faces
   `(font-lock-keyword-face ((,class (:foreground ,keyword))))
   `(font-lock-type-face ((,class (:foreground ,type))))        ; Built-in types like 'int', 'char'
   `(font-lock-string-face ((,class (:foreground ,string :background ,string-bg))))
   `(font-lock-builtin-face ((,class (:foreground ,keyword))))
   `(font-lock-constant-face ((,class (:foreground ,number))))
   `(font-lock-variable-name-face ((,class (:foreground ,symbol))))
   `(font-lock-function-name-face ((,class (:foreground ,function :background ,function-bg))))
   `(font-lock-comment-face ((,class (:foreground ,comment :slant italic))))
   `(font-lock-comment-delimiter-face ((,class (:foreground ,comment :slant italic))))
   `(font-lock-preprocessor-face ((,class (:foreground ,preproc))))
   `(font-lock-regexp-grouping-backslash ((,class (:foreground ,regexp :background ,regexp-bg))))
   `(font-lock-regexp-grouping-construct ((,class (:foreground ,regexp :background ,regexp-bg))))
   `(font-lock-class-name-face ((,class (:foreground ,classname))))  ; User-defined types
   
   ;; JS2-mode specific faces
   `(js2-function-param ((,class (:foreground ,symbol))))
   `(js2-external-variable ((,class (:foreground ,symbol))))
   `(js2-function-call ((,class (:foreground ,function :background ,function-bg))))
   `(js2-object-property ((,class (:foreground ,symbol))))
   `(js2-jsdoc-tag ((,class (:foreground ,comment :slant italic))))
   `(js2-jsdoc-type ((,class (:foreground ,type))))
   `(js2-jsdoc-value ((,class (:foreground ,string :background ,string-bg))))
   `(js2-private-member ((,class (:foreground ,symbol))))

   ;; Tree-sitter faces
   (custom-set-faces
    '(tree-sitter-hl-face:function-name ((t (:foreground "#85FFDF" :background "#1F2B31"))))
    '(tree-sitter-hl-face:variable ((t (:foreground "#A19DBF"))))
    '(tree-sitter-hl-face:keyword ((t (:foreground "#66c8ef"))))
    '(tree-sitter-hl-face:type ((t (:foreground "#ffa07a"))))
    '(tree-sitter-hl-face:comment ((t (:foreground "#666" :slant italic))))
    '(tree-sitter-hl-face:string ((t (:foreground "#9aca7e" :background "#212A24")))))
   
   ;; Special characters and brackets
   `(show-paren-match ((,class (:foreground ,specialchar :background ,specialchar-bg))))
   `(paren-face ((,class (:foreground ,cbracket))))
   `(bracket-face ((,class (:foreground ,cbracket))))
   `(brace-face ((,class (:foreground ,cbracket))))
   
   ;; Minibuffer and mode line
   `(minibuffer-prompt ((,class (:background ,minibuffer-active-bg :foreground ,keyword))))
   `(mode-line ((,class (:background ,minibuffer-active-bg :foreground ,fg))))
   `(mode-line-inactive ((,class (:background ,minibuffer-bg :foreground ,comment))))
   
   ;; Additional custom faces
   `(font-lock-warning-face ((,class (:foreground ,todo :weight bold))))
   `(font-lock-doc-face ((,class (:foreground ,comment :slant italic))))
   `(font-lock-negation-char-face ((,class (:foreground ,specialchar :background ,specialchar-bg)))))

(provide-theme 'gptkod))
