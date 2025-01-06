;; kud-theme.el --- A dark theme inspired by Kod editor
;;; Code:
(deftheme kud "A dark theme inspired by the Kod editor.")

(let ((class '((class color) (min-colors 89)))
      (fg "#eeeeee")
      (bg "#222222")
      (cursor "#ff3319")
      (selection "#2F3F52")
      (comment "#666666")
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
      (number "#C969B6")
      (preproc "#9C8B7C")
      (symbol "#A19DBF")
      (function "#85FFDF")
      (function-bg "#1F2B31")
      (bracket "#dddddd")
      (variable "#cda869"))

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
   `(font-lock-variable-name-face ((,class (:foreground ,variable))))
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
               (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'kud)
;;; kud-theme.el ends here
