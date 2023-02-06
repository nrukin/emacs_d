;; monokai-theme
(use-package monokai-theme
  :config
  (load-theme 'monokai t))

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

;; font settings
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

;; simple dialogs
(fset 'yes-or-no-p 'y-or-n-p)

;; nyan-mode
(use-package nyan-mode
  :config
  (nyan-mode t))

(use-package which-key
  :config
  (which-key-mode))

;; emojify
(use-package emojify)

;; discover
(use-package discover)

;; all the icons
(use-package all-the-icons
  :if (display-graphic-p))

(use-package all-the-icons-dired
  :requires all-the-icons
  :hook (dired-mode . all-the-icons-dired-mode))

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

;; packages auto update
(use-package auto-package-update
  :config
  (auto-package-update-maybe))
