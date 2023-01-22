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

;; run on windows predicate
(defvar run-on-win-p (eq system-type 'windows-nt))

;; monokai-theme
(use-package monokai-theme
  :config
  (load-theme 'monokai t))

;; magit
(use-package magit
  :init
  (setq magit-commit-show-diff nil))

;; start maximized
(add-to-list 'default-frame-alist '(fullscreen . maximized))

;; hide menubar, scrollbar and toolbar
(menu-bar-mode -1)
(scroll-bar-mode -1)
(tool-bar-mode -1)

;; disable sounds
(setq ring-bell-function 'ignore)

;; hide dialog box
(setq use-dialog-box nil)

;; customize cursor
(setq-default cursor-type 'bar)
(if (fboundp 'blink-cursor-mode) (blink-cursor-mode 0))

(when run-on-win-p
  (add-to-list 'default-frame-alist '(font . "DejaVu Sans Mono-10" ))
  (set-face-attribute 'default t :font "DejaVu Sans Mono-10" ))

;; l8n
(setq default-input-method "russian-computer")

(setq calendar-week-start-day 1)

(setq calendar-week-start-day 1
      calendar-day-name-array ["Воскресенье" "Понедельник" "Вторник" "Среда"
			       "Четверг" "Пятница" "Суббота"]
      calendar-day-header-array ["Вс" "Пн" "Вт" "Ср" "Чт" "Пт" "Сб"]
      calendar-day-abbrev-array ["Вск" "Пнд" "Втр" "Сре" "Чтв" "Птн" "Суб"]
      calendar-month-name-array ["Январь" "Февраль" "Март" "Апрель" "Май"
				 "Июнь" "Июль" "Август" "Сентябрь"
				 "Октябрь" "Ноябрь" "Декабрь"]
      calendar-month-abbrev-array ["Янв" "Фев" "Мар" "Апр" "Май" "Июн" "Июл" "Авг" "Сен" "Окт" "Ноя" "Дек"])

;; ibuffer
(use-package ibuffer
  :bind ([remap list-buffers] . ibuffer))

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
(add-to-list 'org-capture-templates '("e" "Event" entry	(file "~/org/inbox.org") "* PLANNED %? %(org-set-tags \"event\")\nSCHEDULED: %^T\n:PROPERTIES:\n:CREATED:  %U\n:END:" :empty-lines 1))
(add-to-list 'org-capture-templates '("t" "Today" entry	(file "~/org/inbox.org") "* TODO %?\nSCHEDULED: %t\n:PROPERTIES:\n:CREATED:  %U\n:END:" :empty-lines 1))
(add-to-list 'org-capture-templates '("a" "Timer" entry (file "~/org/inbox.org") "* TODO %?\n:PROPERTIES:\n:CREATED:  %U\n:END:" :empty-lines 1 :clock-in t :clock-keep t))
(add-to-list 'org-capture-templates '("z" "Dstrb" entry	(file "~/org/inbox.org") "* DONE %?\nCLOSED: %U\n:PROPERTIES:\n:CREATED:  %U\n:END:" :empty-lines 1 :clock-in t :clock-resume t))

;; org-superstar mode
(use-package org-superstar
  :hook (org-mode . (lambda () (org-superstar-mode 1))))

;; elfeed
(use-package elfeed
  :bind ("C-x w" . elfeed)
  :init
  (setq elfeed-search-title-max-width 100)
  (when run-on-win-p
    (setq elfeed-use-curl nil))
  (defun elfeed-search-format-date (date)
    (format-time-string "%Y-%m-%d %H:%M" (seconds-to-time date))))

(use-package elfeed-org
  :init
  (setq rmh-elfeed-org-files '("~/org/elfeed.org"))
  :config
  (elfeed-org))

;; packages auto update
(use-package auto-package-update
  :config
  (auto-package-update-maybe))

;; rclone orc-cat sync

(defvar rclone-path "" "path to rclone binary")
(defvar rclone-log-path "" "path to sync log file")
(defvar rclone-filter-path "" "path to rclone filter path")
(defvar rclone-local-path "" "path to local directory")
(defvar rclone-remote-path "" "path to remote directory")
(defvar rclone-remote-backup "" "path to remote backup directory")
(defvar rclone-local-backup "" "path to local backup directory")

(defun rclone-org-upload ()
  "upload org-files to remote via rclone"
  (interactive)
    (shell-command (format "%s sync --filter-from %s --log-file %s --log-level INFO --backup-dir %s %s %s"
	rclone-path
	rclone-filter-path
	rclone-log-path
	(concat rclone-remote-backup "remote\\" (format-time-string "%Y%m%d_%H%M%S" (current-time)))
	rclone-local-path
	rclone-remote-path))
    (message "Upload complete"))

(defun rclone-org-download ()
  "download org-files from remote via rclone"
  (interactive)
    (shell-command (format "%s sync --filter-from %s --log-file %s --log-level INFO --backup-dir %s %s %s"
	rclone-path
	rclone-filter-path
	rclone-log-path
	(concat rclone-local-backup (format-time-string "%Y%m%d_%H%M%S" (current-time)))
	rclone-remote-path
	rclone-local-path))
    (message "Download complete"))

(defun rclone-backup-upload ()
  "upload backup files to cloud storage"
  (interactive)
    (elfeed-db-save)
    (shell-command (format "%s move --log-file %s --log-level INFO --delete-empty-src-dirs %s %s"
	rclone-path
	rclone-log-path
	rclone-local-backup
	(concat rclone-remote-backup (system-name) "\\")
	))
    (message "Backup move complete"))

(defun save-sync-and-quit ()
  (interactive)
  (when (y-or-n-p "Save, sync and exit emacs?")
    (when (org-clocking-buffer)
      (org-clock-out))
    (save-some-buffers t)
    (rclone-org-upload)
    (kill-emacs)))

(global-set-key (kbd "C-c q") 'save-sync-and-quit)
;; custom settings file
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(ignore-errors (load custom-file))
