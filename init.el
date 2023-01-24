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

;; local functions
(defvar run-on-win-p (eq system-type 'windows-nt))
(defvar load-mastodon nil)

;; load secret file
(ignore-errors (load (expand-file-name "secret.el" user-emacs-directory)))

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

;; emojify
(use-package emojify
  :hook (after-init . global-emojify-mode))

;; discover
(use-package discover)

;; ibuffer
(use-package ibuffer
  :bind ([remap list-buffers] . ibuffer))

;; files backup directory
(let ((backup-dir (format "%sbackups" (file-name-directory user-init-file))))
  (add-to-list 'backup-directory-alist `("." . ,backup-dir)))

(setq version-control t)
(setq delete-old-versions t)
(setq kept-new-versions 4)
(setq kept-old-versions 4)

;; revert files on disk change
(global-auto-revert-mode t)

;; do not create lock-files
(setq create-lockfiles nil)

;; simple dialogs
(fset 'yes-or-no-p 'y-or-n-p)

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

;; all the icons
(use-package all-the-icons
  :if (display-graphic-p))

;; dashboard
(use-package dashboard
  :config
  (setq dashboard-set-heading-icons t)
  (setq dashboard-set-file-icons t)
  (setq dashboard-items '((recents  . 5)
                        (bookmarks . 5)
                        (registers . 5)))
  (setq dashboard-startup-banner 'logo)
  (setq dashboard-center-content t)

  ;; add navigator
  (setq dashboard-set-navigator t)
  ;; Format: "(icon title help action face prefix suffix)"

  (setq dashboard-navigator-buttons
	`(;; elfeed line
	  ((,(all-the-icons-faicon "rss" :height 1.1 :v-adjust 0.0)
            "Elfeed"
            ""
            (lambda (&rest _) (elfeed))))
	  ;; mastodon line
	  ((,(all-the-icons-faicon "trash" :height 1.1 :v-adjust 0.0)
            "Mastodon"
            ""
            (lambda (&rest _) (mastodon))))))

  (dashboard-setup-startup-hook))

;; elfeed
(use-package elfeed
  :bind ("C-x w" . elfeed)
  :config
  (when run-on-win-p
    (setq elfeed-use-curl nil))
  (defun elfeed-search-format-date (date)
    (format-time-string "%Y-%m-%d %H:%M" (seconds-to-time date)))
  (setq elfeed-search-title-max-width 100)
  ;; run secret elfeed config
  (when (fboundp 'elfeed-afterconf)
    (elfeed-afterconf)))

(use-package elfeed-org
  :init
  (setq rmh-elfeed-org-files '("~/org/elfeed.org"))
  :config
  (elfeed-org))

;; packages auto update
(use-package auto-package-update
  :config
  (auto-package-update-maybe))

;; json-mode
(use-package json-mode)

;; markdown-mode
(use-package markdown-mode
  :mode ("README\\.md\\'" . gfm-mode))

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

;; hydra
(use-package hydra
  :defer 2
  :bind ("<f9>" . hydra-clock/body)
  :config
  (defun org-clock-in-last-with-prefix-arg ()
    (interactive)
    (setq current-prefix-arg '(4)) ; C-u
    (call-interactively 'org-clock-in-last))
  (defhydra hydra-clock (:color blue)
    "
    ^
    ^Clock^             ^Do^
    ^─────^─────────────^──^─────────
    _q_ quit            _c_ cancel
    ^^                  _d_ display
    ^^                  _e_ effort
    ^^                  _i_ in
    ^^                  _j_ jump
    ^^                  _o_ out
    ^^                  _r_ report
    ^^                  _l_ continue last
    ^^                  ^^
    "
    ("q" nil)
    ("c" org-clock-cancel :color pink)
    ("d" org-clock-display)
    ("e" org-clock-modify-effort-estimate)
    ("i" org-clock-in)
    ("j" org-clock-goto)
    ("o" org-clock-out)
    ("r" org-clock-report)
    ("l" org-clock-in-last-with-prefix-arg)))


;; mastodon
(use-package mastodon
  :if load-mastodon
  :config
  (mastodon-discover)
  (mastodon-toot--enable-custom-emoji)
  (setq mastodon-tl--show-avatars t))

;; windows-rus shell encoding
(defun windows-shell-encoding-config ()
  (defadvice shell (after my-shell-advice)
    (set-process-coding-system (get-buffer-process (current-buffer)) 'cp1251 'cp1251))
  (ad-activate 'shell))

;; run secret function late configuration
(when (fboundp 'secret-afterconf)
 (secret-afterconf))

;; custom settings file
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(ignore-errors (load custom-file))
