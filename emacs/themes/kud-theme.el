;; kud-theme.el --- A dark theme inspired by Kod editor

;;; Code:
(deftheme kud
  "A dark theme inspired by the Kod editor.")

(require 'font-latex)
(declare-function font-latex-add-keywords nil)

(let ((class '((class color) (min-colors 89)))
      (fg "#eeeeee")
      (bg "#222222")
      (cursor "#ff3319")
      (selection "#2F3F52")
      (comment "#666666")
      (keyword "#66c8ef")
      (type "lightsalmon")
      (usertype "#CAC059")
      (classname "#eb7962")
      (string "#9aca7e")
      (string-bg "#212A24")
      (specialchar "#C0D164")
      (specialchar-bg "#2B2F26")
      (regexp "#FFB14B")
      (regexp-bg "#342C22")
      (number "#C969B6")
      (preproc "#9C8B7C")
      (symbol "#A19DBF")
      (function "#85FFDF")
      (function-bg "#1F2B31")
      (bracket "#dddddd")
      (variable "#cda869")
      (date "#F09C9F")
      (time "#A78AB0")
      (name "#8CB194")
      (url "#77B5FF")
      (oldfile "#f7bfb6")
      (oldfile-bg "#42201C")
      (newfile "#cff7bf")
      (newfile-bg "#13340C")
      (difflines-bg "#3D96DE"))

  (custom-theme-set-faces
   'kud
   `(default ((,class (:foreground ,fg :background ,bg))))
   `(cursor ((,class (:background ,cursor))))
   `(region ((,class (:background ,selection))))
   `(font-lock-comment-face ((,class (:foreground ,comment :slant italic))))
   `(font-lock-keyword-face ((,class (:foreground ,keyword))))
   `(font-lock-type-face ((,class (:foreground ,type))))
   `(font-lock-builtin-face ((,class (:foreground ,usertype))))
   `(font-lock-function-name-face ((,class (:foreground ,function :background ,function-bg))))
   `(font-lock-string-face ((,class (:foreground ,string :background ,string-bg))))
   `(font-lock-constant-face ((,class (:foreground ,symbol))))
   `(font-lock-number-face ((,class (:foreground ,number))))
   `(font-lock-property-name-face ((,class (:foreground ,symbol))))
   `(font-lock-preprocessor-face ((,class (:foreground ,preproc))))
   `(font-lock-variable-name-face ((,class (:foreground ,variable))))
   
   ;; Additional syntax elements
   `(font-lock-regexp-grouping-construct ((,class (:foreground ,regexp :background ,regexp-bg))))
   `(font-lock-regexp-grouping-backslash ((,class (:foreground ,specialchar :background ,specialchar-bg))))
   
   ;; Diff specific
   `(diff-added ((,class (:foreground ,newfile :background ,newfile-bg))))
   `(diff-removed ((,class (:foreground ,oldfile :background ,oldfile-bg))))
   `(diff-changed ((,class (:background ,difflines-bg))))
   
   ;; URLs and special text
   `(link ((,class (:foreground ,url :underline t))))
   `(link-visited ((,class (:foreground ,url :underline t))))
   
   ;; Mode line
   `(mode-line ((,class (:foreground ,fg :background "#333333"))))
   `(mode-line-inactive ((,class (:foreground ,comment :background "#2a2a2a"))))
   
   ;; Search
   `(isearch ((,class (:foreground ,bg :background ,keyword))))
   `(lazy-highlight ((,class (:foreground ,bg :background ,regexp))))
   
   ;; LaTeX faces
   `(font-latex-sectioning-1-face ((,class (:foreground "lightsalmon"))))
   `(font-latex-sectioning-2-face ((,class (:foreground "lightsalmon"))))
   `(font-latex-sectioning-3-face ((,class (:foreground "lightsalmon"))))
   `(font-latex-sectioning-4-face ((,class (:foreground "lightsalmon"))))
   `(font-latex-sectioning-5-face ((,class (:foreground "lightsalmon"))))
   `(font-latex-bold-face ((,class (:foreground "#ffffff" :bold t))))
   `(font-latex-italic-face ((,class (:underline t :slant italic))))
   `(font-latex-math-face ((,class (:foreground "orange"))))
   `(font-latex-verbatim-face ((,class (:foreground "#bbeecc" :background "#2c2c2c"))))
   `(font-latex-argument-face ((,class (:foreground "#9194BB"))))
   `(font-latex-bibtex-face ((,class (:foreground "#8D86EE"))))))

;;;###autoload
(when load-file-name
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'kud)
;;; kud-theme.el ends here
