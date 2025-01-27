(use-package emacs
  :init
  (menu-bar-mode -1)
  (scroll-bar-mode -1)
  (tool-bar-mode -1)

  (setq initial-buffer-choice t)

  (setq ring-bell-function 'ignore)
  (setq use-dialog-box nil)
  (setq use-file-dialog nil)
  (fset 'yes-or-no-p 'y-or-n-p)

  (setq-default cursor-type 'bar)
  (if (fboundp 'blink-cursor-mode) (blink-cursor-mode 0))

  (setq default-input-method "russian-computer")

  (setq calendar-day-name-array
	["Воскресенье" "Понедельник" "Вторник"
	 "Среда" "Четверг" "Пятница" "Суббота"])
  (setq calendar-day-header-array
	["Вс" "Пн" "Вт"
	 "Ср" "Чт" "Пт" "Сб"])
  (setq calendar-day-abbrev-array
	["Вск" "Пнд" "Втр"
	 "Сре" "Чтв" "Птн" "Суб"])
  (setq calendar-month-name-array
	["Январь" "Февраль" "Март"
	 "Апрель" "Май" "Июнь"
	 "Июль" "Август" "Сентябрь"
	 "Октябрь" "Ноябрь" "Декабрь"])
  (setq calendar-month-abbrev-array
	["Янв" "Фев" "Мар"
	 "Апр" "Май" "Июн"
	 "Июл" "Авг" "Сен"
	 "Окт" "Ноя" "Дек"])
  
  (setq calendar-week-start-day 1)

  (let ((backup-dir (format "%sbackups" (file-name-directory user-init-file))))
    (add-to-list 'backup-directory-alist `("." . ,backup-dir)))
  (setq version-control t)
  (setq delete-old-versions t)
  (setq kept-new-versions 4)
  (setq kept-old-versions 4)
  (global-auto-revert-mode t)
  (setq create-lockfiles nil)

  (add-to-list 'display-buffer-alist
               '("\\`\\*\\(Warnings\\|Compile-Log\\)\\*\\'"
		 (display-buffer-no-window)
		 (allow-no-window . t))))

(use-package ibuffer
  :bind ([remap list-buffers] . ibuffer))

(use-package which-key
  :ensure t
  :config
  (which-key-mode))

(use-package free-keys
  :ensure t)

(use-package ace-window
  :ensure t
  :bind (("M-o" . ace-window)))

(use-package delsel
  :ensure nil
  :hook (after-init . delete-selection-mode))

(use-package savehist
  :ensure nil
  :hook (after-init . savehist-mode))

;; Разрешить C-c n n
;; Фокусировка на выделении
(put 'narrow-to-region 'disabled nil)
