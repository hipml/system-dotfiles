;; Kodl theme based on provided color scheme
(deftheme kodl "A dark theme with precise color matching")

(custom-theme-set-faces
 'kodl
 
 ;; Basic faces
 '(default ((t (:foreground "#eee" :background "#222"))))
 '(cursor ((t (:background "#ff3319"))))
 '(region ((t (:background "#2F3F52"))))
 '(fringe ((t (:background "#333"))))
 '(line-number ((t (:foreground "#666" :background "#333"))))
 '(line-number-current-line ((t (:foreground "#ddd" :background "#333"))))
 
 ;; Syntax highlighting
 '(font-lock-keyword-face ((t (:foreground "#66c8ef"))))
 '(font-lock-type-face ((t (:foreground "lightsalmon"))))
 '(font-lock-builtin-face ((t (:foreground "#CAC059"))))  ; usertype
 '(font-lock-function-name-face ((t (:foreground "#85FFDF" :background "#1F2B31"))))
 '(font-lock-string-face ((t (:foreground "#9aca7e" :background "#212A24"))))
 '(font-lock-comment-face ((t (:foreground "#666" :slant italic))))
 '(font-lock-constant-face ((t (:foreground "#C969B6"))))  ; numbers
 '(font-lock-preprocessor-face ((t (:foreground "#9C8B7C"))))
 '(font-lock-variable-name-face ((t (:foreground "#cda869"))))
 '(font-lock-warning-face ((t (:foreground "#946B57" :weight bold))))  ; todo

 ;; Elisp-specific faces
 '(font-lock-symbol-face ((t (:foreground "#A19DBF"))))  ; Added symbol face with the symbol color

 ;; Method calls and arguments (python-specific)
 '(python-mode-method-call ((t (:foreground "#85FFDF"))))  ; for method calls like .from_pretrained
 '(python-mode-keyword-arg ((t (:foreground "#9194BB"))))  ; for named parameters like token=

 ;; Special characters and regexp
 '(escape-glyph ((t (:foreground "#C0D164" :background "#2B2F26"))))  ; specialchar
 '(regexp-grouping-construct ((t (:foreground "#FFB14B" :background "#342C22"))))
 
 ;; Brackets and symbols
 '(show-paren-match ((t (:foreground "#ddd" :weight bold))))  ; cbracket
 '(show-paren-mismatch ((t (:foreground "#ff3319" :weight bold))))
 
 ;; Mode-specific faces
 ;; Markdown/Org headers
 '(markdown-header-face ((t (:foreground "lightsalmon"))))
 '(org-level-1 ((t (:foreground "lightsalmon"))))
 
 ;; Diffs
 '(diff-added ((t (:foreground "#cff7bf" :background "#13340C"))))   ; newfile
 '(diff-removed ((t (:foreground "#f7bfb6" :background "#42201C")))) ; oldfile
 '(diff-hunk-header ((t (:foreground "white" :background "#3D96DE")))) ; difflines
 
 ;; Web-mode specific
 '(web-mode-html-tag-face ((t (:foreground "#cda869"))))        ; selector
 '(web-mode-html-attr-name-face ((t (:foreground "#c4af75"))))  ; property
 '(web-mode-html-attr-value-face ((t (:foreground "#f9ed97")))) ; value
 
 ;; Additional faces
 '(link ((t (:foreground "#77B5FF" :underline t))))             ; url
 '(highlight ((t (:background "#2F3F52"))))                     ; selection
 '(fixed-pitch ((t (:foreground "#bbeecc" :background "#2c2c2c")))) ; fixed
 '(variable-pitch ((t (:foreground "#cda869"))))               ; variable
)

;; Theme variables
(custom-theme-set-variables
 'kodl
 '(frame-background-mode 'dark))

;; Additional customizations for Python mode
(with-eval-after-load 'python
  (font-lock-add-keywords
   'python-mode
   '(("\\.[a-zA-Z_][a-zA-Z0-9_]*" 0 'python-mode-method-call t)  ; Method calls
     ("\\([a-zA-Z_][a-zA-Z0-9_]*\\)=" 1 'python-mode-keyword-arg t))))  ; Named parameters

;; Additional customizations for Elisp mode - ensure symbols are properly highlighted
(with-eval-after-load 'elisp-mode
  (font-lock-add-keywords
   'emacs-lisp-mode
   '(("'\\([a-zA-Z][a-zA-Z0-9-]*\\)" 1 'font-lock-symbol-face t))))  ; Quoted symbols

(provide-theme 'kodl)
