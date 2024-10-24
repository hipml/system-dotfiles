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
(load-theme 'kod-adjusted t)

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
    :init
    (setq evil-want-C-u-scroll t)
    (setq evil-want-integration t)

    :config
    (evil-mode 1))
