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

;; enable line numbers
(global-display-line-numbers-mode t)

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

(use-package org
    :config
    (setq org-log-done 'time)
    (setq org-agenda-files '("~/Documents/org/agenda.org"))
    (setq org-hide-leading-stars t)
    (setq org-startup-indented t) ; indent headlines
)

(use-package magit
    :ensure t)

(use-package evil
    :init
    (setq evil-want-C-u-scroll t)
    (setq evil-want-integration t)

    :config
    (evil-mode 1)
)

(set-face-attribute 'default nil :height 120)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages '(ein elpy)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
