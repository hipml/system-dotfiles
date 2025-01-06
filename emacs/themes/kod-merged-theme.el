(deftheme kod-merged
  "Comprehensive Kod theme merging Emacs and CSS definitions")

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
      ;; UI colors
      (column-guide "#444")
      (metaruler-fg "#666")
      (metaruler-bg "#333")
      (border-color "#444")
      ;; Special content
      (date-color "#F09C9F")
      (time-color "#A78AB0")
      (name-color "#8CB194")
      (fixed-fg "#bbeecc")
      (fixed-bg "#2c2c2c")
      (argument-color "#9194BB")
      (math-color "orange")
      (bibtex-color "#8D86EE")
      ;; Diff colors
      (diff-old-fg "#f7bfb6")
      (diff-old-bg "#42201C")
      (diff-new-fg "#cff7bf")
      (diff-new-bg "#13340C")
      (diff-change-fg "#FFFFFF")
      (diff-change-bg "#3D96DE")
      ;; Web-specific
      (selector-color "#cda869")
      (property-color "#c4af75")
      (value-color "#f9ed97"))

  (custom-theme-set-faces
   'kod-merged
   ;; Basic faces
   `(default ((,class (:background ,bg :foreground ,fg))))
   `(cursor ((,class (:background ,cursor-color))))
   `(region ((,class (:background ,selection-bg))))
   `(fringe ((,class (:background ,bg))))
   `(vertical-border ((,class (:foreground ,border-color))))
   
   ;; Syntax highlighting
   `(font-lock-comment-face ((,class (:foreground ,comment-color :slant italic))))
   `(font-lock-keyword-face ((,class (:foreground ,keyword-color))))
   `(font-lock-type-face ((,class (:foreground ,type-color))))
   `(font-lock-string-face ((,class (:foreground ,string-fg :background ,string-bg))))
   `(font-lock-constant-face ((,class (:foreground ,constant-color))))
   `(font-lock-variable-name-face ((,class (:foreground ,variable-color))))
   `(font-lock-function-name-face ((,class (:foreground ,function-fg :background ,function-bg))))
   `(font-lock-builtin-face ((,class (:foreground ,keyword-color))))
   `(font-lock-preprocessor-face ((,class (:foreground ,preproc-color))))
   
   ;; Additional syntax faces
   `(font-lock-regexp-grouping-construct ((,class (:foreground ,regexp-fg :background ,regexp-bg))))
   `(font-lock-regexp-grouping-backslash ((,class (:foreground ,regexp-fg :background ,regexp-bg))))
   
   ;; Special content
   `(font-lock-doc-face ((,class (:foreground ,time-color))))
   `(font-lock-warning-face ((,class (:foreground ,warning-color :weight bold))))
   
   ;; UI Elements
   `(mode-line ((,class (:background ,metaruler-bg :foreground ,metaruler-fg :box nil))))
   `(mode-line-inactive ((,class (:background ,fixed-bg :foreground ,comment-color :box nil))))
   
   ;; Diffs and version control
   `(diff-added ((,class (:foreground ,diff-new-fg :background ,diff-new-bg))))
   `(diff-removed ((,class (:foreground ,diff-old-fg :background ,diff-old-bg))))
   `(diff-changed ((,class (:foreground ,diff-change-fg :background ,diff-change-bg))))
   
   ;; Additional faces for web development
   `(css-selector ((,class (:foreground ,selector-color))))
   `(css-property ((,class (:foreground ,property-color))))
   
   ;; Links and special text
   `(link ((,class (:foreground "#77B5FF" :underline t))))
   `(fixed-pitch ((,class (:foreground ,fixed-fg :background ,fixed-bg))))
   `(variable-pitch ((,class (:family "M+ 1m"))))
   
   ;; Math and technical content
   `(font-latex-math-face ((,class (:foreground ,math-color))))
   `(font-latex-sedate-face ((,class (:foreground ,bibtex-color))))
   
   ;; Headers
   `(markdown-header-face ((,class (:foreground ,type-color :weight bold))))
   `(markdown-header-face-1 ((,class (:foreground ,type-color :weight bold :height 1.4))))
   `(markdown-header-face-2 ((,class (:foreground ,type-color :weight bold :height 1.3))))
   `(markdown-header-face-3 ((,class (:foreground ,type-color :weight bold :height 1.2))))))

(provide-theme 'kod-merged)
