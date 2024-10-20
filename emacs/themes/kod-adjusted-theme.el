(deftheme kod-adjusted
  "Kod to Emacs, maybe")

(let ((class '((class color) (min-colors 89)))
      (bg "#222")
      (fg "#eee")
      (cursor-color "#ff3319")
      (selection-bg "#2F3F52")
      (comment-color "#666")
      (keyword-color "#66c8ef")
      (type-color "lightsalmon")
      (string-fg "#9aca7e")
      (string-bg "#212A24")
      (number-color "#C969B6")
      (function-fg "#85FFDF")
      (function-bg "#1F2B31")
      (variable-color "#cda869")
      (constant-color "#FFB14B")
      (operator-color "#A19DBF")
      (warning-color "#946B57")
      (docstring-color "#666")
      (highlight-line "#333")
      (hl-keyword "#66c8ef"))

  (custom-theme-set-faces
   'kod-adjusted

   ;; Default settings
   `(default ((,class (:background ,bg :foreground ,fg))))
   `(cursor ((,class (:background ,cursor-color))))
   `(region ((,class (:background ,selection-bg))))
   `(highlight ((,class (:background ,highlight-line))))
   `(fringe ((,class (:background ,bg))))

   ;; Syntax highlighting for Python
   `(font-lock-comment-face ((,class (:foreground ,comment-color :slant italic))))
   `(font-lock-doc-face ((,class (:foreground ,docstring-color :slant italic))))
   `(font-lock-keyword-face ((,class (:foreground ,keyword-color))))
   `(font-lock-type-face ((,class (:foreground ,type-color))))
   `(font-lock-string-face ((,class (:foreground ,string-fg :background ,string-bg))))
   `(font-lock-constant-face ((,class (:foreground ,constant-color))))
   `(font-lock-variable-name-face ((,class (:foreground ,variable-color))))
   `(font-lock-function-name-face ((,class (:foreground ,function-fg :background ,function-bg))))
   `(font-lock-builtin-face ((,class (:foreground ,hl-keyword))))
   `(font-lock-operator-face ((,class (:foreground ,operator-color))))
   `(font-lock-number-face ((,class (:foreground ,number-color))))

   ;; UI Elements
   `(mode-line ((,class (:background "#333" :foreground "#666" :box nil))))
   `(mode-line-inactive ((,class (:background "#2c2c2c" :foreground "#444" :box nil))))
   `(vertical-border ((,class (:foreground "#444"))))

   ;; Warnings and errors
   `(warning ((,class (:foreground ,warning-color :weight bold))))

   ;; Links
   `(link ((,class (:foreground "#77B5FF" :underline t))))

   ;; Diff and git-related highlighting
   `(diff-added ((,class (:foreground "#cff7bf" :background "#13340C"))))
   `(diff-removed ((,class (:foreground "#f7bfb6" :background "#42201C"))))
   `(diff-changed ((,class (:foreground "#FFFFFF" :background "#3D96DE"))))))

(provide-theme 'kod-adjusted)
