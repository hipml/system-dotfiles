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
  :custom
  (org-log-done 'time)
  (org-agenda-files '("~/code/org/agenda.org"))
  (org-agenda-default-view 'agenda)
  (org-hide-leading-stars t)
  (org-startup-indented t)
  (org-file-apps '((auto-mode . emacs)
                   ("\\.org\\'" . emacs)))
  (org-capture-templates
   '(("t" "Todo" entry (file+headline "~/code/org/agenda.org" "Tasks")
      "* TODO %?\n  %i\n  %a")
     ("n" "Note" entry (file+headline "~/code/org/agenda.org" "Notes")
      "* %? :NOTE:\n  %i\n  %a")))

  :config
  (global-set-key (kbd "C-c l") #'org-store-link)
  (global-set-key (kbd "C-c a") #'org-agenda)
  (global-set-key (kbd "C-c c") #'org-capture)

  (defun org-move-done-to-end ()
    "Move DONE tasks to end of list."
    (org-map-entries
     (lambda ()
       (when (member (org-get-todo-state) '("DONE"))
         (org-move-subtree-down)))))

  (add-hook 'org-after-todo-state-change-hook 'org-move-done-to-end))

;; python
(use-package python 
  :mode ("\\.py\\'" . python-mode)
  :custom
  (python-indent 4)
  (python-shell-interpreter "python3")
  (indent-tabs-mode nil)
  :hook (python-mode . (lambda ()
						 (tree-sitter-hl-mode)
                         (font-lock-mode))))

(use-package jupyter
  :after python
  :config
  ;; Set up runtime directory
  (setq jupyter-runtime-directory "/home/lita/.local/share/jupyter/runtime")
  (setq jupyter-executable "/usr/bin/jupyter")
  
  ;; Enable jupyter-repl mode in python buffers
  (add-hook 'python-mode-hook #'jupyter-repl-interaction-mode)
  
  ;; Configure REPL display
  (setq jupyter-repl-prompt-margin-width 8)
  (setq jupyter-repl-echo-eval-p t
        jupyter-repl-allow-RET-when-busy t)
  
  ;; Enable inline image display
  (setq jupyter-repl-display-buffer-action '(display-buffer-below-selected))
  
  ;; Configure default image size (adjust as needed)
  (setq jupyter-image-default-width 800
        jupyter-image-default-height 600)
  
  ;; Add convenient keybindings
  :bind (:map jupyter-repl-interaction-mode-map
              ("C-c C-c" . jupyter-eval-line-or-region)
              ("C-c C-r" . jupyter-eval-region)
              ("C-c C-b" . jupyter-eval-buffer)
              ("C-c C-k" . jupyter-repl-interrupt-kernel)
              ("C-c j r" . jupyter-run-repl)
              ("C-c j R" . jupyter-repl-restart-kernel)))
  
;; Add async for asynchronous process execution
(use-package async
  :ensure t)

(defun my/notebook-export-to-pdf (notebook-file)
  "Export a Jupyter notebook to PDF using nbconvert."
  (interactive "fNotebook file: ")
  (let ((default-directory (file-name-directory notebook-file)))
    (async-start
     `(lambda ()
        ,(async-inject-variables "\\`\\(notebook-file\\|default-directory\\)\\'")
        (shell-command-to-string
         (format "cd %s && jupyter nbconvert --to pdf --template classic %s"
                 default-directory
                 (file-name-nondirectory notebook-file))))
     (lambda (result)
       (if (string-match "error\\|Error\\|ERROR" result)
           (message "Export failed: %s" result)
         (message "Successfully exported to PDF! Check %s"
                  (concat (file-name-sans-extension notebook-file) ".pdf")))))))
 
(defun my/notebook-export-with-template (notebook-file template)
  "Export a Jupyter notebook to PDF using a specific template."
  (interactive
   (list
    (read-file-name "Notebook file: ")
    (completing-read "Template: "
                    '("classic" "article" "report" "basic")
                    nil t "article")))
  (async-start
   `(lambda ()
      ,(async-inject-variables "\\`\\(notebook-file\\|template\\)\\'")
      (shell-command-to-string
       (format "jupyter nbconvert --to pdf --template %s %s"
               template notebook-file)))
   (lambda (result)
     (if (string-match "error" result)
         (message "Export failed: %s" result)
       (message "Successfully exported to PDF with %s template!" template)))))

;; Function to check and install required dependencies
(defun my/check-pdf-export-dependencies ()
  "Check if required PDF export dependencies are installed."
  (interactive)
  (let ((missing-deps '()))
    (unless (executable-find "jupyter")
      (push "jupyter" missing-deps))
    (unless (executable-find "pdflatex")
      (push "texlive" missing-deps))
    (unless (executable-find "pandoc")
      (push "pandoc" missing-deps))
    
    (if missing-deps
        (message "Missing dependencies for PDF export: %s. Install them with your package manager."
                 (string-join missing-deps ", "))
      (message "All PDF export dependencies are installed!"))))

;; Add to existing keybindings
(global-set-key (kbd "C-c j p") 'my/notebook-export-to-pdf)
(global-set-key (kbd "C-c j t") 'my/notebook-export-with-template)
(global-set-key (kbd "C-c j d") 'my/check-pdf-export-dependencies)

(use-package company
  :config
  (global-company-mode)
  (setq company-idle-delay 0.3))

(use-package elpy
  :after (python)
  (elpy-enable))


(use-package magit
  :ensure t)

(use-package evil
  :ensure t
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding t)
  (setq evil-want-C-u-scroll t)
  :config
  (evil-mode 1)
  (evil-set-initial-state 'treemacs-mode 'emacs)
  :bind
  (:map evil-normal-state-map ("C-y" . 'yank)
   :map evil-visual-state-map ("C-y" . 'yank)
   :map evil-insert-state-map ("C-y" . 'yank)))

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
  :hook ((LaTeX-mode . reftex-mode)
         (LaTeX-mode . pdf-tools-install)
		 (LaTeX-mode . visual-line-mode))
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
  :init
  (setq projectile-project-search-path '("~/code/"))
  :config
  (projectile-mode +1)
  ;; (setq projectile-file-explorer 'treemacs)
  (setq projectile-discover-projects-in-directory (expand-file-name "~/code"))
  (setq projectile-auto-discover t)
  (setq projectile-enable-caching t)
  ;; (setq projectile-auto-update-cache t)
  :bind-keymap
  ("C-x p" . projectile-command-map))

(use-package treemacs
  :ensure t
  :after projectile
  :config
  (treemacs-resize-icons 16)
  (treemacs-follow-mode t)
  (treemacs-filewatch-mode t)
  (treemacs-fringe-indicator-mode t)
  (treemacs-git-mode 'deferred)
  (treemacs-fringe-indicator-mode 'always)
  :bind
  (:map global-map
		("M-0" . treemacs-select-window)
		("C-x t 1" . treemacs-delete-other-windows)
		("C-x t t" . treemacs)
		("C-x t d" . treemacs-select-directory))
  :hook
  (projectile-after-switch-project-hook . treemacs-add-project-to-workspace)
  (projectile-after-switch-project-hook . treemacs-refresh))

(use-package treemacs-projectile
  :after (treemacs projectile)
  :ensure t)

(use-package treemacs-magit
  :after (treemacs magit))

(use-package treemacs-evil
  :after (treemacs evil)
  :ensure t)

(treemacs-start-on-boot)

;; fix mouse
(setq mouse-drag-copy-region nil)
(setq mouse-scroll-accepts-movement nil)

(use-package vterm
  :ensure t)



(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("cd3a935a8ffa314b540e05877c97fc4651f62300f9f89d6e9e7ca822a4d591f2" "c0fe46c2c91bda132c98f1f882a83ee263335a3c934d10f0db96c7dbccb7c8a0" "536622b90022666ba1ed1de27535fc79a8a2d0d03c8e7dd4a66872cb225e3bd9" "c30f1ac361bc0025b677e82de3b4a454f77b3abb6542278650e471dd80a6e36a" "9f96a5e589c9e5bfb299ea372ef82ae636f1a0b88b01bc3263d64cb0bfac4de4" "52526fdb0eafd76fdc1963a87a30bd38f70673407646ae13b72561b503dc6f69" "a4c78d5d55160c9a719a36724dba8e428958470dd7952ab0b7b715efd006f6f4" "8bf1e0be927767ae05d4035ee68f54998b112d548494676ec8d1d1b77e43c808" "1d8ed1460acd9d6352b46379ca6463e14b560ce659fb07ac1e808e19834ba798" default))
 '(package-selected-packages
   '(async jupyter vterm treemacs-magit treemacs-projectile treemacs auctex-cluttex auctex-cont-latexmk auctex-latexmk auctex-label-numbers markdown-mode pdf-tools auctex flycheck-ocaml merlin-eldoc merlin tuareg tree-sitter-langs tree-sitter treesitter magit evil elpy ein)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
