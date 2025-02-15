(setq custom-file (locate-user-emacs-file "custom.el"))
(load custom-file :no-error-if-file-is-missing)

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

(add-to-list 'display-buffer-alist
             '("\\`\\*\\(Warnings\\|Compile-Log\\)\\*\\'"
               (display-buffer-no-window)
               (allow-no-window . t)))


;; short prompts
(setq use-short-answers t)

;; default font
(set-face-attribute 'default nil :family "Monospace" :height 105)

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
(load-theme 'gptkod t)

;; set line wrapping
(setq-default truncate-lines t)

;; tab width
(setq-default tab-width 4)

;; removing menu bar and scroll bar
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

(set-frame-parameter nil 'internal-border-width 0)
(setq window-divider-default-places 'all) ; or 'all for all sides
(setq window-divider-default-right-width 2)
(window-divider-mode 1)

;; change default buffer to Scratch
(setq initial-buffer-choice t)

;; disable new frames
(setq pop-up-frames nil)

;; fix mouse
(setq mouse-drag-copy-region nil)
(setq mouse-scroll-accepts-movement nil)

;; emoji font? C-x 8 RET
(set-fontset-font t 'symbol "Noto Color Emoji")


;; tree sitter for pretty syntax highlighting 
(require 'treesit)
(global-font-lock-mode t)
(setq font-lock-maximum-decoration t)
(setq treesit-font-lock-level 4)
(setq treesit-extra-load-path '("~/.emacs.d/tree-sitter/"))

(use-package treesit-auto
  :custom
  (treesit-auto-install 'prompt)
  :config
  (tresit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode))

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
  (org-roam-completion-system 'default)
  :bind (("C-c n l" . org-roam-buffer-toggle)
         ("C-c n f" . org-roam-node-find)
         ("C-c n i" . org-roam-node-insert)
         ("C-c n c" . org-roam-capture))
  :config
  (org-roam-db-autosync-mode)
  
  (add-hook 'org-capture-after-finalize-hook (lambda ()
            (when-let* ((buf (org-capture-get :buffer))
                       (win (get-buffer-window buf)))
              (when (window-live-p win)
                (delete-window win)))))
  
  ;; make sure evil doesn't interfere with org-capture
  (evil-set-initial-state 'org-capture-mode 'insert)
  
  (evil-define-key '(normal) org-mode-map
    (kbd "RET") #'org-open-at-point))


;; enable word wrap for org and org-roam mode files
(add-hook 'org-mode-hook 'visual-line-mode)
(add-hook 'org-roam-mode-hook 'visual-line-mode)

;; python
(use-package python 
  :mode ("\\.py\\'" . python-ts-mode)
  :custom
  (python-indent-offset 4)
  (python-shell-interpreter "python3")
  (indent-tabs-mode nil)
  :bind (:map python-ts-mode-map
              ("C-<return>" . python-shell-send-buffer)))


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
  :hook (org-mode . evil-org-mode)
  :config
  (add-hook 'evil-org-mode-hook
            (lambda ()
              (evil-org-set-key-theme '(navigation insert textobjects additional))))
  (evil-define-key 'insert evil-org-mode-map
    (kbd "RET") 'org-return-and-maybe-indent)
  (evil-define-key 'normal org-mode-map
    (kbd "M-RET") (lambda ()
                    (interactive)
                    (end-of-line)
                    (org-meta-return))))

;; (evil-define-key 'insert evil-org-mode-map
;;   (kbd "RET" 'org-return)

(use-package evil-tex
  :ensure t
  :hook (LaTeX-mode . evil-tex-mode))

;; makes tab play nicely with org-mode
(setq evil-want-C-i-jump nil) ;; prevents TAB from being bound to evil-jump-forward

;; (windmove-default-keybindings)
;; evil-style binds with Meta as leader
(define-key evil-normal-state-map (kbd "M-h") 'windmove-left)
(define-key evil-normal-state-map (kbd "M-j") 'windmove-down)
(define-key evil-normal-state-map (kbd "M-k") 'windmove-up)
(define-key evil-normal-state-map (kbd "M-l") 'windmove-right)

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

(use-package undo-fu
  :ensure t
  :config
  (define-key evil-normal-state-map "u" 'undo-fu-only-undo)
  (define-key evil-normal-state-map "\C-r" 'undo-fu-only-redo))

(setq evil-undo-system 'undo-fu)

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
  (setq xenops-math-image-scale-factor 2
        xenops-reveal-on-entry t))

(with-eval-after-load 'xenops
  (advice-add 'org-meta-return :after
              (lambda (&rest _)
                (when xenops-mode
                  (xenops-render)))))

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

(add-hook 'treemacs-mode-hook (lambda () (display-line-numbers-mode -1)))

(use-package vterm
  :ensure t)

(use-package gdscript-mode
  :mode "\\.gd\\'" 
  :ensure t)

(use-package conda
  :config
  (conda-env-initialize-interactive-shells)
  (conda-env-initialize-eshell))

;; General indentation settings
(setq-default indent-tabs-mode nil)  ; Use spaces instead of tabs
(setq-default tab-width 2)           ; Set default tab width to 2 spaces

;; Web mode for HTML, CSS, and JS
(use-package web-mode
  :mode (("\\.html?\\'" . web-mode)
         ("\\.css\\'" . web-mode)
         ("\\.tsx?\\'" . web-mode))
  :config
  (setq web-mode-markup-indent-offset 2)    ; HTML indentation
  (setq web-mode-css-indent-offset 2)       ; CSS indentation
  (setq web-mode-code-indent-offset 2)      ; JS indentation
  (setq web-mode-script-padding 2)          ; Padding in <script> tags
  (setq web-mode-style-padding 2)           ; Padding in <style> tags
  (setq web-mode-enable-auto-pairing t)     ; Auto-pairing of brackets
  (setq web-mode-enable-css-colorization t) ; CSS color highlighting
  (setq web-mode-enable-current-element-highlight t)) ; Highlight current element

(use-package js
  :ensure nil
  :mode (("\\.js\\'" . js-ts-mode)
         ("\\.jsx\\'" . js-ts-mode))
  :config
  (require 'treesit)
  (when (treesit-available-p)
    (add-to-list 'auto-mode-alist '("\\.js\\'" . js-ts-mode))))

;; TypeScript support
(use-package typescript-mode
  :mode "\\.ts\\'"
  :config
  (setq typescript-indent-level 2))

(use-package imenu-list
  :bind (("C-'" . imenu-list-smart-toggle))
  :config
  (setq imenu-list-position 'right
        imenu-list-size 35))

(use-package vertico
  :ensure t
  :hook (after-init . vertico-mode))

(use-package orderless
  :custom
  (completion-styles '(orderless basic))
  (completion-category-defaults nil)
  (completion-category-overrides nil))

(use-package marginalia
  :ensure t
  :hook (after-init . marginalia-mode))

(use-package savehist
  :ensure nil
  :hook (after-init . savehist-mode))

(use-package elisp-mode
  :bind (:map emacs-lisp-mode-map
              ("C-<return>" . eval-buffer)))

;; (add-to-list 'load-path "~/code/aitocomplete/")
;; (require 'company-llm)
;; (company-llm-setup)
;; (add-hook 'prog-mode-hook 'llm-autocomplete-mode)
;; (add-hook 'completion-at-point-functions #'llm-autocomplete-complete-at-point)
