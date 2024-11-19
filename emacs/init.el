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
(load-theme 'kod t)

;; set line wrapping
(setq-default truncate-lines t)

;; tab width
(setq-default tab-width 4)

;; use shift to move between buffers
(windmove-default-keybindings)

;; removing menu bar
;; (menu-bar-mode -1)
(tool-bar-mode -1)

;; change default buffer to Scratch
(setq initial-buffer-choice t)

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
  (python-indent-offset 4)
  (python-shell-interpreter "python3")
  (indent-tabs-mode nil)
  :hook ((python-mode . tree-sitter-mode)
         (python-mode . tree-sitter-hl-mode)))

(use-package ein
  :ensure t
  :config
  (setq ein:output-area-inlined-images t)
  (setq jupyter-kernel-connections '((python . "twelve")))
  (setq ein:jupyter-default-notebook-directory "~/code/"))

(use-package async
  :ensure t)
 
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


(add-hook 'emacs-lisp-mode-hook
          (lambda ()
            (setq font-lock-keywords-case-fold-search nil)
            (font-lock-add-keywords 
             nil
             '(("(\\(lambda\\)\\>" (1 font-lock-keyword-face))
               ("(\\(defun\\|defvar\\|defcustom\\|defface\\|defgroup\\)\\s-+\\(\\sw+\\)"
                (1 font-lock-keyword-face)
                (2 font-lock-function-name-face))))))

;; tree sitter for pretty syntax highlighting 
(global-font-lock-mode t)
(setq font-lock-maximum-decoration t)
(use-package tree-sitter
  :ensure t
  :config
  (global-tree-sitter-mode)
  (add-hook 'emacs-lisp-mode-hook #'tree-sitter-hl-mode)
  (add-hook 'python-mode-hook #'tree-sitter-hl-mode))

(use-package tree-sitter-langs
  :ensure t
  :after tree-sitter
  :config
  (add-hook 'python-mode-hook #'tree-sitter-hl-mode)
  (add-hook 'org-mode-hook #'tree-sitter-hl-mode))

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
  (TeX-engnie 'xetex)
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
  (setq projectile-project-root-files-functions
        '(projectile-root-local
          projectile-root-top-down
          projectile-root-bottom-up
          projectile-root-top-down-recurring))
  (setq projectile-project-root-files-bottom-up
        (append '(".git")
                projectile-project-root-files-bottom-up))
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
  (add-hook 'emacs-startup-hook #'treemacs)
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


;; fix mouse
(setq mouse-drag-copy-region nil)
(setq mouse-scroll-accepts-movement nil)

(use-package vterm
  :ensure t)

(defun my/projectile-vterm ()
  "open a vterm for the current Projectile project in the background."
  (interactive) ;; callable yeaaaa
  ;; check if we are in a Projectile project
  (when (projectile-project-p)
      (let* ((project-name (projectile-project-name))
             (vterm-buffer-name (format "*%s-vterm*" project-name))
             ;; ensure default-directory is set to the project root
             (default-directory (projectile-project-root))
             (frame-width (frame-width))
             (window-width (window-width))
             (width-ratio (/ (float window-width) frame-width)))

        ;; see if buf already exists
        (unless (get-buffer vterm-buffer-name)
          (save-window-excursion
            (vterm vterm-buffer-name))

          ;; start conda for my thesis...
          (when (string-equal project-name "thesis")
            (with-current-buffer vterm-buffer-name
              (vterm-send-string "conda activate twelve")
              (vterm-send-return))))

        ;; split based on current window width
        (if (< width-ratio 0.5)
            (split-window-below)
          (split-window-right))

        ;; open up the terminal 
        (other-window 1)
        (switch-to-buffer vterm-buffer-name)
        (message "Created vterm session: %s" vterm-buffer-name))))

(global-set-key (kbd "C-c p v") #'my/projectile-vterm)
(add-hook 'projectile-after-switch-project-hook #'my/projectile-vterm)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("134308c17ad386da20ac5302283f85b20993b929e3a75f4531c7238fde15e067" "9f96a5e589c9e5bfb299ea372ef82ae636f1a0b88b01bc3263d64cb0bfac4de4" "cd3a935a8ffa314b540e05877c97fc4651f62300f9f89d6e9e7ca822a4d591f2" default))
 '(package-selected-packages
   '(treemacs-all-the-icons ein vterm tuareg treemacs-projectile treemacs-magit treemacs-evil tree-sitter-langs request polymode pdf-tools merlin-eldoc markdown-mode jupyter flycheck-ocaml elpy deferred auctex async anaphora)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
