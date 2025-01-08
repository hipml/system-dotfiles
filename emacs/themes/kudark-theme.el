(deftheme kudark
  "Dark theme based on provided CSS colors")

(defcustom kudark-background
  "#222"
  "Background color for kudark theme.")

(custom-theme-set-faces
 'kudark
 `(default ((t (:inherit nil :stipple nil :background ,kudark-background :foreground "#eee"
                         :inverse-video nil :box nil :strike-through nil :overline nil
                         :underline nil :slant normal :weight normal
                         :width normal :foundry nil))))
 '(cursor ((t (:foreground "#222" :background "#ff3319"))))
 '(error ((t (:inherit 'default :underline (:style wave :color "red")))))
 '(highlight ((t (:background "#2F3F52"))))
 '(region ((t (:background "#2F3F52"))))
 '(secondary-selection ((t (:background "#2F3F52"))))
 '(fringe ((t (:background nil :foreground "#666"))))
 '(vertical-border ((t (:foreground "#444"))))
 
 ;; Font lock faces
 '(font-lock-builtin-face ((t (:foreground "#66c8ef"))))
 '(font-lock-comment-face ((t (:foreground "#666" :slant italic))))
 '(font-lock-comment-delimiter-face ((t (:foreground "#666" :slant italic))))
 '(font-lock-constant-face ((t (:foreground "#C969B6"))))
 '(font-lock-doc-face ((t (:foreground "#666" :slant italic))))
 '(font-lock-function-name-face ((t (:foreground "#85FFDF" :background "#1F2B31"))))
 '(font-lock-keyword-face ((t (:foreground "#66c8ef"))))
 '(font-lock-negation-char-face ((t (:foreground "#eee"))))
 '(font-lock-preprocessor-face ((t (:foreground "#9C8B7C"))))
 '(font-lock-string-face ((t (:foreground "#9aca7e" :background "#212A24"))))
 '(font-lock-type-face ((t (:foreground "lightsalmon"))))
 '(font-lock-variable-name-face ((t (:foreground "#eee"))))
 '(font-lock-warning-face ((t (:foreground "orange" :weight bold))))

 ;; Mode line faces
 '(mode-line ((t (:background "#333" :foreground "#eee" :box nil))))
 '(mode-line-inactive ((t (:background "#222" :foreground "#666" :box nil))))
 '(mode-line-buffer-id ((t (:weight bold))))
 '(mode-line-emphasis ((t (:weight bold))))

 ;; Search
 '(isearch ((t (:background "#2F3F52" :foreground "#ff3319"))))
 '(lazy-highlight ((t (:background "#2F3F52"))))
 '(match ((t (:background "#2F3F52"))))

 ;; Company mode
 '(company-tooltip ((t (:background "#333" :foreground "#eee"))))
 '(company-scrollbar-bg ((t (:background "#222"))))
 '(company-scrollbar-fg ((t (:background "#666"))))
 '(company-tooltip-selection ((t (:background "#2F3F52"))))
 '(company-tooltip-common ((t (:foreground "#66c8ef" :weight bold))))
 
 ;; Line numbers
 '(line-number ((t (:foreground "#666" :background "#222"))))
 '(line-number-current-line ((t (:foreground "#eee" :background "#333"))))

 ;; Treemacs specific faces
 '(treemacs-root-face ((t (:foreground "#eee" :weight bold))))
 '(treemacs-directory-face ((t (:foreground "#eee"))))
 '(treemacs-file-face ((t (:foreground "#eee"))))
 '(treemacs-git-modified-face ((t (:foreground "#A19DBF"))))
 '(treemacs-git-added-face ((t (:foreground "#9aca7e"))))
 '(treemacs-git-deleted-face ((t (:foreground "#ff3319")))))

;;;###autoload
(when (and (boundp 'custom-theme-load-path) load-file-name)
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'kudark)
