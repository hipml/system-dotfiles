(deftheme gptkod
  "A custom theme with dark background and vibrant colors.")

(let ((class '((class color) (min-colors 89)))
      (bg "#222")
      (fg "#eee")
      (caret "#ff3319")
      (selection-bg "#2F3F52")
      (column-guide "#444")
      (minibuffer-fg "#666")
      (minibuffer-bg "#333")      ;; Inactive minibuffer background (darker)
      (minibuffer-active-bg "#444")  ;; Active minibuffer background (lighter)
      (minibuffer-border "#444")
      (keyword "#66c8ef")
      (type "#FFA07A")  ;; light salmon
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
      (todo "#946B57"))

  ;; Set default background and foreground
  (custom-theme-set-faces
   'gptkod
   `(default ((,class (:background ,bg :foreground ,fg))))
   `(cursor ((,class (:foreground ,caret))))
   `(region ((,class (:background ,selection-bg))))
   `(hl-line ((,class (:background ,selection-bg))))
   `(column-number ((,class (:foreground ,column-guide))))
   
   ;; Minibuffer styling - active vs inactive states
   `(minibuffer-prompt ((,class (:background ,minibuffer-active-bg :foreground ,keyword))))
   `(minibuffer-line ((,class (:background ,minibuffer-active-bg))))
   `(mode-line ((,class (:background ,minibuffer-active-bg 
                        :foreground ,fg 
                        :box (:line-width -1 :color ,minibuffer-border)))))
   `(mode-line-inactive ((,class (:background ,minibuffer-bg 
                                :foreground ,minibuffer-fg 
                                :box (:line-width -1 :color ,minibuffer-border)))))

   ;; Font lock faces
   `(font-lock-keyword-face ((,class (:foreground ,keyword))))
   `(font-lock-type-face ((,class (:foreground ,type))))
   `(font-lock-variable-name-face ((,class (:foreground ,symbol))))
   `(font-lock-function-name-face ((,class (:foreground ,function :background ,function-bg))))
   `(font-lock-string-face ((,class (:foreground ,string :background ,string-bg))))
   `(font-lock-comment-face ((,class (:foreground ,comment))))
   `(font-lock-constant-face ((,class (:foreground ,number))))
   `(font-lock-preprocessor-face ((,class (:foreground ,preproc))))
   `(font-lock-doc-face ((,class (:foreground ,comment))))
   `(font-lock-regexp-grouping-backslash ((,class (:foreground ,regexp))))
   `(font-lock-regexp-grouping-construct ((,class (:foreground ,regexp))))

   ;; Other faces
   `(show-paren-match ((,class (:foreground ,function :background ,function-bg))))
   `(show-paren-mismatch ((,class (:foreground ,specialchar :background ,specialchar-bg))))
   `(font-lock-builtin-face ((,class (:foreground ,keyword))))
   `(font-lock-negation-char-face ((,class (:foreground ,specialchar))))
   `(font-lock-type-face ((,class (:foreground ,type))))

   ;; Custom faces
   `(font-lock-comment-delimiter-face ((,class (:foreground ,comment))))
   `(font-lock-type-face ((,class (:foreground ,usertype))))
   `(font-lock-class-name-face ((,class (:foreground ,classname))))
   `(font-lock-preprocessor-face ((,class (:foreground ,preproc))))
   `(font-lock-number-face ((,class (:foreground ,number))))
   `(font-lock-function-name-face ((,class (:foreground ,function))))

   ;; Brackets and other structural syntax highlighting
   `(show-paren-match-face ((,class (:background ,function-bg :foreground ,function))))
   `(show-paren-mismatch-face ((,class (:background ,specialchar-bg :foreground ,specialchar))))
   
   ;; TODO comments
   `(font-lock-warning-face ((,class (:foreground ,todo :weight bold))))))

(provide-theme 'gptkod)
