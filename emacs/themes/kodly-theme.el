(deftheme kodly
  "A theme based off of Kod, a defunct OS X editor")

(let ((class '((class color) (min-colors 89)))
      (bg "#222")
      (fg "#eee")
      (caret "#ff3319")
      (selection-bg "#2F3F52")
      (column-guide "#444")
      
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
      
      (minibuffer-bg "#333")
      (minibuffer-active-bg "#444"))

  (custom-theme-set-faces
   'kodly
   
   ;; Basic faces
   `(default ((,class (:background ,bg :foreground ,fg))))
   `(cursor ((,class (:background ,caret))))
   `(region ((,class (:background ,selection-bg))))
   
   ;; Font-lock faces
   `(font-lock-builtin-face ((,class (:foreground ,classname))))
   `(font-lock-comment-delimiter-face ((,class (:foreground ,comment :slant italic))))
   `(font-lock-comment-face ((,class (:foreground ,comment :slant italic))))
   `(font-lock-constant-face ((,class (:foreground ,specialchar :background ,specialchar-bg))))
   `(font-lock-doc-face ((,class (:foreground ,comment :slant italic))))
   `(font-lock-function-name-face ((,class (:foreground ,function :background ,function-bg))))
   `(font-lock-keyword-face ((,class (:foreground ,keyword))))
   `(font-lock-negation-char-face ((,class (:foreground ,keyword))))
   `(font-lock-preprocessor-face ((,class (:foreground ,preproc))))
   `(font-lock-string-face ((,class (:foreground ,string :background ,string-bg))))
   `(font-lock-variable-name-face ((,class (:foreground ,symbol))))
   `(font-lock-variable-use-face ((,class (:foreground ,preproc))))
   `(font-lock-warning-face ((,class (:foreground ,todo :weight bold))))
   `(font-lock-number-face ((,class (:foreground ,number))))
   `(font-lock-type-face ((,class (:foreground ,type))))

   ;; Treesit faces
   `(treesit-font-lock-bracket-face ((,class (:foreground ,cbracket))))
   `(treesit-font-lock-builtin-face ((,class (:foreground ,keyword))))
   `(treesit-font-lock-comment-face ((,class (:foreground ,comment :slant italic))))
   `(treesit-font-lock-constant-face ((,class (:foreground ,specialchar :background ,specialchar-bg))))
   `(treesit-font-lock-constructor-face ((,class (:foreground ,classname))))
   `(treesit-font-lock-delimiter-face ((,class (:foreground ,cbracket))))
   `(treesit-font-lock-escape-face ((,class (:foreground ,specialchar))))
   `(treesit-font-lock-function-call-face ((,class (:foreground ,function))))
   `(treesit-font-lock-function-name-face ((,class (:foreground ,function :background ,function-bg))))
   `(treesit-font-lock-keyword-face ((,class (:foreground ,keyword))))
   `(treesit-font-lock-literal-face ((,class (:foreground ,usertype))))
   `(treesit-font-lock-number-face ((,class (:foreground ,number))))
   `(treesit-font-lock-operator-face ((,class (:foreground ,keyword))))
   `(treesit-font-lock-property-face ((,class (:foreground ,classname))))
   `(treesit-font-lock-punctuation-face ((,class (:foreground ,cbracket))))
   `(treesit-font-lock-string-face ((,class (:foreground ,string :background ,string-bg))))
   `(treesit-font-lock-type-face ((,class (:foreground ,type))))
   `(treesit-font-lock-variable-name-face ((,class (:foreground ,symbol))))
   
   
   ;; Additional syntax faces
   `(escape-glyph ((,class (:foreground ,specialchar))))
   `(homoglyph ((,class (:foreground ,specialchar))))
   `(match ((,class (:background ,selection-bg :foreground ,fg))))
   
   ;; Parenthesis matching
   `(show-paren-match ((,class (:foreground ,keyword :weight bold))))
   `(show-paren-mismatch ((,class (:background ,caret :foreground ,fg))))

   ;; Line highlighting
   `(hl-line ((,class (:background "#333"))))
   `(highlight ((,class (:background ,selection-bg))))
   
   ;; Mode line
   `(mode-line ((,class (:background ,minibuffer-active-bg :foreground ,fg))))
   `(mode-line-inactive ((,class (:background ,minibuffer-bg :foreground ,comment))))
   
   ;; Company (completion) faces
   `(company-tooltip ((,class (:background ,minibuffer-bg :foreground ,fg))))
   `(company-tooltip-selection ((,class (:background ,selection-bg))))
   `(company-tooltip-common ((,class (:foreground ,keyword))))
   `(company-tooltip-annotation ((,class (:foreground ,type))))

   ;; Treemacs
   `(treemacs-root-face ((,class (:foreground ,type :weight bold))))
   `(treemacs-file-face ((,class (:foreground ,fg))))
   `(treemacs-directory-face ((,class (:foreground ,function))))
   `(treemacs-git-modified-face ((,class (:foreground ,classname))))
   `(treemacs-git-added-face ((,class (:foreground ,string))))
   `(treemacs-git-conflict-face ((,class (:foreground ,caret))))
   `(treemacs-git-untracked-face ((,class (:foreground ,comment))))
   `(treemacs-term-node-face ((,class (:foreground ,function))))
   `(treemacs-tags-face ((,class (:foreground ,symbol))))

   ;; Buffer border colors
   `(fringe ((,class (:background ,minibuffer-bg))))  
   `(vertical-border ((,class (:background ,minibuffer-active-bg :foreground ,minibuffer-active-bg))))  
   `(window-divider ((,class (:foreground ,minibuffer-active-bg))))  
   `(window-divider-first-pixel ((,class (:foreground ,minibuffer-active-bg))))
   `(window-divider-last-pixel ((,class (:foreground ,minibuffer-active-bg))))

   ;; Line numbers
   `(fringe ((,class (:background ,minibuffer-active-bg)))) ;; Border color #444
   `(line-number ((,class (:background ,minibuffer-bg :foreground ,comment)))) ;; Background #333
   `(line-number-current-line ((,class (:background ,minibuffer-bg :foreground ,fg :weight bold)))) 

   ;; Minibuffer
   `(minibuffer-prompt ((,class (:foreground ,keyword :weight bold)))))

  (custom-theme-set-variables
   'kodly
   `(ansi-color-names-vector
     [,bg ,regexp ,string ,usertype ,keyword ,number ,type ,fg])))

;;;###autoload
(when load-file-name
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'kodly)
