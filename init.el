;;
;; my emacs init file
;; created 20.01.2023
;;

;; packages
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

;; use-package
;; install and configure
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

;; magit
(use-package magit
  :init
  (setq magit-commit-show-diff nil))

;; input method
(setq default-input-method "russian-computer")

;; org-mode activation
;; https://orgmode.org/manual/Activation.html
(global-set-key (kbd "C-c l") #'org-store-link)
(global-set-key (kbd "C-c a") #'org-agenda)
(global-set-key (kbd "C-c c") #'org-capture)

;; org-mode customize
(setq org-todo-keywords '((sequence "TODO(t)" "WAITING(w)" "PLANNED(p)" "LATER(l)" "DELEGATED(g@)" "|" "DONE(d!)" "CANCELLED(c@)")))
(setq org-hide-leading-stars t)
(setq org-log-done 'time)
(setq org-log-into-drawer t)
(setq org-log-reschedule 'time)
(setq org-refile-targets '((org-agenda-files :maxlevel . 3)))
(setq org-refile-use-outline-path 'file)

;; org-mode pathes
(setq org-directory "~/org/")
(setq org-agenda-files (list org-directory))

;; org-mode capture templates
(setq org-capture-templates '())
(add-to-list 'org-capture-templates '("i" "Inbox" entry (file "~/org/inbox.org") "* TODO %?\n:PROPERTIES:\n:CREATED:  %U\n:END:" :empty-lines 1))

;; custom settings file
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(ignore-errors (load custom-file))
