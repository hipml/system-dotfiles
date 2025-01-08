(deftheme kud "A dark theme inspired by the Kod editor.")
(let ((class '((class color) (min-colors 89)))
      (fg "#eeeeee")
      (bg "#222222")
      (cursor "#ff3319")
      (selection "#2F3F52")
      (comment "#666666")
      (keyword "#66c8ef")
      (type "#ffa07a")
      (usertype "#9c8b7c")
      (classname "#eb7962")
      (string "#9aca7e")
      (string-bg "#212A24")
      (specialchar "#C0D164")
      (specialchar-bg "#2B2F26")
      (regexp "#FFB14B")
      (regexp-bg "#342C22")
      (number "#C969B6")
      (preproc "#cac059")
      (symbol "#A19DBF")
      (function "#85FFDF")
      (function-bg "#1F2B31")
      (bracket "#dddddd")
      (variable "#cda869"))
  (custom-theme-set-faces
   'kud
   ;; Basic faces
   `(default ((,class (:foreground ,fg :background ,bg))))
   `(cursor ((,class (:background ,cursor))))
   `(region ((,class (:background ,selection))))
   
   ;; Standard syntax faces
   `(font-lock-comment-face ((,class (:foreground ,comment :slant italic))))
   `(font-lock-keyword-face ((,class (:foreground ,keyword))))
   `(font-lock-type-face ((,class (:foreground ,type))))
   `(font-lock-builtin-face ((,class (:foreground ,usertype))))
   `(font-lock-function-name-face ((,class (:foreground ,function))))
   `(font-lock-string-face ((,class (:foreground ,string :background ,string-bg))))
   `(font-lock-constant-face ((,class (:foreground ,symbol))))
   `(font-lock-number-face ((,class (:foreground ,number))))
   `(font-lock-variable-name-face ((,class (:foreground ,variable))))
   
   ;; JavaScript specific faces
   `(js2-function-param ((,class (:foreground ,variable))))
   `(js2-external-variable ((,class (:foreground ,variable))))
   `(js2-instance-member ((,class (:foreground ,variable))))
   `(js2-private-member ((,class (:foreground ,variable))))
   `(js2-jsdoc-type ((,class (:foreground ,type))))
   `(js2-jsdoc-tag ((,class (:foreground ,preproc))))
   `(js2-jsdoc-value ((,class (:foreground ,string))))
   `(js2-object-property ((,class (:foreground ,variable))))
   `(js2-object-property-access ((,class (:foreground ,fg))))
   `(js2-magic-paren ((,class (:foreground ,bracket))))
   `(js2-function-call ((,class (:foreground ,function))))
   
   ;; Tree-sitter faces for modern JavaScript highlighting
   `(tree-sitter-hl-face:method.call ((,class (:foreground ,function))))
   `(tree-sitter-hl-face:property ((,class (:foreground ,variable))))
   `(tree-sitter-hl-face:number ((,class (:foreground ,number))))
   `(tree-sitter-hl-face:operator ((,class (:foreground ,keyword))))
   `(tree-sitter-hl-face:constant ((,class (:foreground ,symbol))))
   `(tree-sitter-hl-face:string ((,class (:foreground ,string :background ,string-bg))))
   
   ;; Keep your existing Python faces
   `(python-attribute-face ((,class (:foreground ,variable))))
   `(python-variable-name-face ((,class (:foreground ,variable))))
   `(python-attribute ((,class (:foreground ,variable))))
   `(python-instance-attribute ((,class (:foreground ,variable))))
   `(python-type-face ((,class (:foreground ,type))))
   `(python-decorator ((,class (:foreground ,preproc))))
   `(python-number ((,class (:foreground ,number))))))

;;;###autoload
(when load-file-name
  (add-to-list 'custom-theme-load-path
               (file-name-directory load-file-name)))
(provide-theme 'kud)
