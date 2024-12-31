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
(set-face-attribute 'default nil :family "Monospace" :height 110)

;; remove minibuffer scroll bars
(set-window-scroll-bars (minibuffer-window) nil nil)

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
(menu-bar-mode -1)
(tool-bar-mode -1)

;; change default buffer to Scratch
(setq initial-buffer-choice t)

;; disable new frames
(setq pop-up-frames nil)

;; org mode 
(use-package org
  :custom
  (org-log-done 'time)
  (org-agenda-files '("~/code/org/agenda.org"))
  (org-agenda-default-view 'agenda)
  (org-hide-leading-stars t)
  (org-startup-indented t)
  (org-file-apps '((auto-mode . find-file)
                   ("\\.org\\'" . find-file)
                   (directory . find-file)))
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

(use-package org-roam
  :ensure t
  :custom
  (org-roam-directory (file-truename "~/Dropbox/org"))
  (org-roam-file-extensions '("org"))
  (org-roam-completion-everywhere t)
  (org-roam-capture-templates
   '(("d" "default" plain
      "%?"
      :target (file+head "%<%Y%m%d%H%M%S>-${slug}.org"
                        "#+title: ${title}\n")
      :unnarrowed t)))
  :bind (("C-c n l" . org-roam-buffer-toggle)
         ("C-c n f" . org-roam-node-find)
         ("C-c n i" . org-roam-node-insert)
         ("C-c n c" . org-roam-capture))
  :config
  (org-roam-db-autosync-mode))

;; enable word wrap for org and org-roam mode files
(add-hook 'org-mode-hook 'visual-line-mode)
(add-hook 'org-roam-mode-hook 'visual-line-mode)

;; python
(use-package python 
  :mode ("\\.py\\'" . python-mode)
  :custom
  (python-indent-offset 4)
  (python-shell-interpreter "python3")
  (indent-tabs-mode nil)
  :hook ((python-mode . tree-sitter-mode)
         (python-mode . tree-sitter-hl-mode)))

(use-package async
  :ensure t)
 
(use-package company
  :ensure t
  :config
  (global-company-mode)
  (setq company-idle-delay 0.2))

(use-package company-jedi
  :ensure t
  :config
  (add-to-list 'company-backends 'company-jedi))

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
  (:map evil-normal-state-map
        ("C-y" . 'yank))
  (:map evil-visual-state-map
        ("C-y" . 'yank))
  (:map evil-insert-state-map
        ("C-y" . 'yank)))

(use-package evil-org
  :after org
  :hook (org-mode . evil-org-mode))

(use-package evil-tex
  :ensure t
  :hook (LaTeX-mode . evil-tex-mode))

;; makes tab play nicely with org-mode
(setq evil-want-C-i-jump nil) ;; prevents TAB from being bound to evil-jump-forward

(evil-define-key 'normal org-mode-map
  (kbd "M-h") 'org-metaleft
  (kbd "M-j") 'org-metadown
  (kbd "M-k") 'org-metaup
  (kbd "M-l") 'org-metaright)

(evil-define-key 'normal org-mode-map
  (kbd "TAB") 'org-cycle
  (kbd "<tab>") 'org-cycle
  (kbd "S-TAB") 'org-shifttab
  (kbd "<S-tab>") 'org-shifttab)

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
         (LaTeX-mode . prettify-symbols-mode)
		 (LaTeX-mode . visual-line-mode))
  :custom
  (TeX-engine 'xetex)
  (TeX-PDF-mode t)
  (TeX-auto-save t)
  (TeX-parse-self t)
  (TeX-master nil)
  (reftex-default-bibliography '("bibliography.bib"))
  (reftex-plug-into-AUCTeX t)
  (TeX-view-program-selection '((output-pdf "PDF Tools")))
  (TeX-view-program-list '(("PDF Tools" "TeX-pdf-tools-sync-view")))
  ;; (LaTeX-command "latex -shell-escape")
  ;; -pvc for continuous compilation on save
  :config
  (with-eval-after-load 'tex
    ;; add command to the list
    (add-to-list 'TeX-command-list
                 '("LaTeXMk" "latexmk -pdf %s" TeX-run-TeX nil t
                   :help "Run LatexMk"))
    ;; make it default
    (TeX-command-default "LaTeXMk")))

(defun hipml/latex-pdf-view (process)
  "Open the PDF after successful latex compilation... hopefully"
  (unless (TeX-process-get-variable TeX-command-next 'running)
    (let ((pdf-file (concat (TeX-master-file) ".pdf")))
      (if (file-exists-p pdf-file)
          (progn
            (TeX-revert-document-buffer process)
            (TeX-view))
        (message "PDF file not found: %s" pdf-file)))))

(add-hook 'TeX-after-compilation-finished-functions #'hipml/latex-pdf-view)

(use-package xenops
  :ensure t
  :hook ((org-mode . xenops-mode)
         (LaTeX-mode . xenops-mode))
  :config
  (setq xenops-math-image-scale-factor 1.7
        xenops-reveal-on-entry t))

(add-hook 'latex-mode-hook 'flyspell-mode)
(add-hook 'tex-mode-hook 'flyspell-mode)
(setq ispell-program-name "aspell")

;; add latex packages for math support
;;  https://michaelneuper.com/posts/how-i-use-org-roam-to-takes-notes-for-cs/
(with-eval-after-load 'org
  (add-to-list 'org-latex-packages-alist '("" "amsmath" t))
  (add-to-list 'org-latex-packages-alist '("" "amssymb" t))
  (add-to-list 'org-latex-packages-alist '("" "mathtools" t))
  (add-to-list 'org-latex-packages-alist '("" "mathrsfs" t)))

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
  (setq projectile-file-explorer 'treemacs)
  (setq projectile-auto-discover t)
  (setq projectile-enable-caching t)
  (setq projectile-auto-update-cache t)
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
  :after (projectile magit evil)
  :init
  (add-hook 'after-make-frame-functions
            (lambda (frame)
              (select-frame frame)
              (treemacs)))
  :config
  (treemacs-resize-icons 16)
  (treemacs-follow-mode t)
  (treemacs-filewatch-mode t)
  (treemacs-fringe-indicator-mode t)
  (treemacs-git-mode 'deferred)
  (treemacs-hide-gitignored-files-mode t)
  (treemacs-git-commit-diff-mode t)
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
  :ensure t
  :after (treemacs))

(use-package treemacs-magit
  :ensure t
  :after (treemacs))

(use-package treemacs-evil
  :ensure t
  :after (treemacs))

(use-package treemacs-icons-dired
  :ensure t
  :hook (dired-mode . treemacs-icons-dired-enable-once))

;; fix mouse
(setq mouse-drag-copy-region nil)
(setq mouse-scroll-accepts-movement nil)

(use-package vterm
  :ensure t)

(add-hook 'server-after-make-frame-hook
          (lambda ()
            (let ((frame (car (frame-list))))
              (select-frame frame)
              (raise-frame frame))))

(use-package gdscript-mode
  :mode "\\.gd\\'" 
  :ensure t)

(use-package conda
  :config
  (conda-env-autoactivate-mode t))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("b02fd42d0881f9e1e5106c5d2bcf6793163208585066ad148d9ba8c29993c720" "dbb62acef475676dab89ccf15a914806df2028d52a1d458e8305ae1bd9d2ae24" "536622b90022666ba1ed1de27535fc79a8a2d0d03c8e7dd4a66872cb225e3bd9" "5f4ed5b64eb9fbb3fe4b39493b16409c48cde3da0fde9ac4a56fe4277cc3e2ac" "9448ac7767727bb8947c5b689acc74c190db465dbe78bddf404a8bc3be38457a" "fb97b7404431120bb8c85d2ffbfe9629c181ef78d93e83a866677f359fc840dc" "468eb9a6c7a8f0d5e94e82dfb24472d945f813d7168b5d7860cbae852941fc00" "d474ec389bbb890e4a5aab3c444a746e8be3392588e22841e51f0564997a005d" "76185c24b2e39a42f238e8c8740f0e12c66df0309dc721c99b0ec52d59ad81cc" "925d6006c807abac5c8161c497249d15478fe1ad7a42e73d84b80b31f0b17c12" "a046f87a68ff2dedd4b994814f14b55e4f24da317f50adea3563c2921cdc4ac6" "134308c17ad386da20ac5302283f85b20993b929e3a75f4531c7238fde15e067" "9f96a5e589c9e5bfb299ea372ef82ae636f1a0b88b01bc3263d64cb0bfac4de4" "cd3a935a8ffa314b540e05877c97fc4651f62300f9f89d6e9e7ca822a4d591f2" default))
 '(org-agenda-files nil nil nil "Customized with use-package org")
 '(package-selected-packages
   '(conda lsp-pyright lsp-ui lsp-mode evil-org gdscript-mode godoctor yaml-mode evil-tex treemacs-icons-dired treemacs-all-the-icons ein vterm tuareg treemacs-projectile treemacs-magit treemacs-evil tree-sitter-langs request polymode pdf-tools merlin-eldoc markdown-mode jupyter flycheck-ocaml elpy deferred auctex async anaphora))
 '(safe-local-variable-values '((conda-project-env-name . "ai_env"))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
