(require 'package)
(setq package-enable-at-startup nil)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("gnu" . "https://elpa.gnu.org/packages/")))

(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

;; short prompts
(setq use-short-answers t)

;; default font
(set-face-attribute 'default nil :family "Monospace" :height 100)

;; default size
(add-to-list 'default-frame-alist '(width . 120)) 
(add-to-list 'default-frame-alist '(height . 25))

;; enable line numbers
(global-display-line-numbers-mode t)

;; custom themes
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")

;; load my theme
(load-theme 'kod-merged t)

;; set line wrapping
(setq-default truncate-lines t)

;; tab width
(setq-default tab-width 4)

;; org mode 
(use-package org
  :config
  (setq org-log-done 'time)
  (setq org-agenda-files '("~/Documents/org/agenda.org"))
  (setq org-hide-leading-stars t)
  (setq org-startup-indented t) ; indent headlines

  (global-set-key (kbd "C-c l") #'org-store-link)
  (global-set-key (kbd "C-c a") #'org-agenda)
  (global-set-key (kbd "C-c c") #'org-capture))

;; set default Org-mode file extension
(setq org-file-apps '((auto-mode . emacs)
                      ("\\.org\\'" . emacs)))

;; set default Org-mode agenda view
(setq org-agenda-default-view 'agenda)

;; set default Org-mode capture template
(setq org-capture-templates
      '(("t" "Todo" entry (file+headline "~/Documents/org/agenda.org" "Tasks")
         "* TODO %?\n  %i\n  %a")
        ("n" "Note" entry (file+headline "~/Documents/org/agenda.org" "Notes")
         "* %? :NOTE:\n  %i\n  %a")))

;; python
(use-package python 
  :ensure nil
  :hook (python-mode . (lambda ()
                         (setq indent-tabs-mode nil)
                         (setq python-indent 4))))

(add-hook 'python-mode-hook
		  (lambda ()
			(front-lock-mode 1)))

(add-to-list 'auto-mode-alist '("\\.py$" . python-mode))

(use-package elpy
  :init 
  (elpy-enable))

(use-package ein
  :after (python)
  :config
  (setq ein:use-auto-completion t))

(use-package magit
  :ensure t)

(use-package evil
  :ensure t
  :init
  (setq evil-want-C-u-scroll t)
  (setq evil-want-integration t)

  :config
  (evil-mode 0))

(use-package tree-sitter
  :ensure t
  :config
  (global-tree-sitter-mode))

(use-package tree-sitter-langs
  :ensure t
  :after tree-sitter
  :config
  (add-hook 'python-mode-hook #'tree-sitter-hl-mode)
  (add-hook 'org-mode-hook #'tree-sitter-hl-mode))

;; global
(global-tree-sitter-mode)

;; major mode for ocaml
(use-package tuareg
  :ensure t
  :mode (("\\.ocamlinit\\'" . tuareg-mode)))

(use-package merlin
  :ensure t
  :config
  (add-hook 'tuareg-mode-hook #'merlin-mode)
  (add-hook 'merlin-mode-hook #'company-mode)
  ;; we're using flycheck instead
  (setq merlin-error-after-save nil))

(use-package merlin-eldoc
  :ensure t
  :hook ((tuareg-mode) . merlin-eldoc-setup))

(use-package flycheck-ocaml
  :ensure t
  :config
  (flycheck-ocaml-setup))

(use-package pdf-tools
  :ensure t
  :config
  (pdf-tools-install)
  (add-hook 'pdf-view-mode-hook
			(lambda ()
              (display-line-numbers-mode -1)))) 

;; AUCTeX configuration
(use-package auctex
  :ensure t
  :mode ("\\.tex\\'" . LaTeX-mode)
  :hook ((LaTeX-mode . reftex-mode)            ;; Enable RefTeX with LaTeX
         (LaTeX-mode . pdf-tools-install)      ;; Install PDF Tools in LaTeX mode
		 (LaTeX-mode . visual-line-mode))      ;; word wrap for LaTeX
  :custom
  (TeX-view-program-selection '((output-pdf "PDF Tools")))
  (reftex-default-bibliography '("bibliography.bib"))
  (reftex-plug-into-AUCTeX t)
  (TeX-command-default "LaTeX")
  (TeX-auto-save t)
  (TeX-parse-self t)
  (TeX-PDF-mode t)
  (TeX-master nil)  ;; Ask which file is the master if not set
  :config
  (add-hook 'TeX-after-compilation-finished-functions
            #'TeX-revert-document-buffer)
  ;; (add-hook 'LaTeX-mode-hook
  ;;		(lambda () (add-hook 'after-save-hook
  ;;							 #'TeX-command-master nil t)))
  )

;; Markdown mode configuration
(use-package markdown-mode
  :ensure t
  :mode ("\\.md\\'" . markdown-mode)
  :config
  (add-hook 'markdown-mode-hook 'visual-line-mode)) ;; word wrap for markdown

(use-package projectile
  :ensure t
  :demand t
  :config
  (projectile-mode +1)
  (setq projectile-file-explorer 'treemacs)
  (setq projectile-enable-caching t)
  ;; (setq projectile-switch-project-action 'treemacs)
  (setq projectile-project-root-files '(".git" ".hg" "package.json" "build.gradle"))
  ;;(setq projectile-indexing-method 'native)
  ;;:hook
  ;;('find-file-hook . (lambda ()
	;;				   (when (not (projectile-project-p))
	;;					 (projectile-discover-project (buffer-file-name)))))
  ;;('projectile-after-switch-project-hook 'treemacs)
  :bind-keymap
  ("C-c p" . projectile-command-map))

(use-package treemacs
  :ensure t
  :after projectile
  :config
  (treemacs-resize-icons 16)
  (treemacs-follow-mode t)
  (treemacs-filewatch-mode t)
  (treemacs-fringe-indicator-mode t)
  ;; (setq treemacs-switch-project-action 'treemacs-add-and-switch)
  )

(use-package treemacs-projectile
  :after (treemacs projectile)
  :ensure t)

(treemacs-start-on-boot)

(defun update-treemacs-project ()
  "update treemacs when opening a new file"
  (let ((project-root (projectile-project-root)))
	(when project-root
	  (message "Adding project to Treemacs: %s" project-root)
	  (treemacs-add-project-to-workspace project-root))))

(add-hook 'find-file-hook #'update-treemacs-project)

;;(defun projectile-switch-to-project-when-opening-file ()
;;  (when (projectile-project-p)
;;	(projectile-switch-project-by-name (projectile-project-name))
;;	(treemacs-select-window)
;;	(treemacs-add-and-display-current-project)
;;	(other-window 1))) ; switch to the newly opened file's buffer

;;(add-hook 'find-file-hook 'projectile-switch-to-project-when-opening-file)

;;(defun treemacs-refresh-when-switching-projects ()
 ;; (treemacs-refresh))

;;(add-hook 'projectile-switch-project-hook 'treemacs-refresh-when-switching-projects)

;; fix mouse
(setq mouse-drag-copy-region nil)
(setq mouse-scroll-accepts-movement nil)


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("cd3a935a8ffa314b540e05877c97fc4651f62300f9f89d6e9e7ca822a4d591f2" "c0fe46c2c91bda132c98f1f882a83ee263335a3c934d10f0db96c7dbccb7c8a0" "536622b90022666ba1ed1de27535fc79a8a2d0d03c8e7dd4a66872cb225e3bd9" "c30f1ac361bc0025b677e82de3b4a454f77b3abb6542278650e471dd80a6e36a" "9f96a5e589c9e5bfb299ea372ef82ae636f1a0b88b01bc3263d64cb0bfac4de4" "52526fdb0eafd76fdc1963a87a30bd38f70673407646ae13b72561b503dc6f69" "a4c78d5d55160c9a719a36724dba8e428958470dd7952ab0b7b715efd006f6f4" "8bf1e0be927767ae05d4035ee68f54998b112d548494676ec8d1d1b77e43c808" "1d8ed1460acd9d6352b46379ca6463e14b560ce659fb07ac1e808e19834ba798" default))
 '(package-selected-packages
   '(treemacs-projectile treemacs auctex-cluttex auctex-cont-latexmk auctex-latexmk auctex-label-numbers markdown-mode pdf-tools auctex flycheck-ocaml merlin-eldoc merlin tuareg tree-sitter-langs tree-sitter treesitter magit evil elpy ein)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
