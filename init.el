;; Создан 2024-06-19
;; Вторая версия init.el файла
;; упрощенная, вся инициализация в одном файле

;; Общая процедура: загрузка файла по имени
(defun me/load-config-file (file)
  "Загружает файл по имени"
  (let ((config-file (expand-file-name (format "%s.el" file) user-emacs-directory)))
    (when (file-exists-p config-file)
      (load-file config-file))))

;; Загружаем локальный init файл
(me/load-config-file ".secret/init")

;; Настройка пакетов, подключение melpa
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

;; Общие настройки emacs
(use-package emacs
  :init
  (menu-bar-mode -1)
  (scroll-bar-mode -1)
  (tool-bar-mode -1)

  (setq initial-buffer-choice t)

  (setq ring-bell-function 'ignore)
  (setq use-dialog-box nil)
  (fset 'yes-or-no-p 'y-or-n-p)

  (setq-default cursor-type 'bar)
  (if (fboundp 'blink-cursor-mode) (blink-cursor-mode 0))

  (add-to-list 'default-frame-alist
	       '(font . "Fira Code-12"))  

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
  (setq create-lockfiles nil))

;; лигатуры для fira code
(use-package ligature
  :ensure t
  :config

  ;; Enable the www ligature in every possible major mode
  (ligature-set-ligatures 't '("www"))

  ;; Enable ligatures in programming modes                                                           
  (ligature-set-ligatures 'prog-mode '("www" "**" "***" "**/" "*>" "*/" "\\\\" "\\\\\\" "{-" "::"
                                       ":::" ":=" "!!" "!=" "!==" "-}" "----" "-->" "->" "->>"
                                       "-<" "-<<" "-~" "#{" "#[" "##" "###" "####" "#(" "#?" "#_"
                                       "#_(" ".-" ".=" ".." "..<" "..." "?=" "??" ";;" "/*" "/**"
                                       "/=" "/==" "/>" "//" "///" "&&" "||" "||=" "|=" "|>" "^=" "$>"
                                       "++" "+++" "+>" "=:=" "==" "===" "==>" "=>" "=>>" "<="
                                       "=<<" "=/=" ">-" ">=" ">=>" ">>" ">>-" ">>=" ">>>" "<*"
                                       "<*>" "<|" "<|>" "<$" "<$>" "<!--" "<-" "<--" "<->" "<+"
                                       "<+>" "<=" "<==" "<=>" "<=<" "<>" "<<" "<<-" "<<=" "<<<"
                                       "<~" "<~~" "</" "</>" "~@" "~-" "~>" "~~" "~~>" "%%"))

  (global-ligature-mode 't))

;; ibuffer для диалога выбора буфера
(use-package ibuffer
  :bind ([remap list-buffers] . ibuffer))

;; org-mode
(use-package org
  :preface (defun my/org-set-created()
	     (interactive)
	     (org-set-property
	      "CREATED"
	      (format-time-string
	       "[%Y-%m-%d %a %H:%M]"
	       (seconds-to-time (current-time)))))
  (defun my/org-inbox-file-name()
    (file-name-concat org-directory  "inbox.org"))
  (setq org-capture-templates '())
  :bind (("C-c l" . org-store-link)
	 ("C-c a" . org-agenda)
	 ("C-c c" . org-capture)
	 :map org-mode-map
	 ("<f6>" . my/org-set-created))
  :config (setq org-todo-keywords '((sequence "TODO(t)" "WAITING(w)" "PLANNED(p)" "LATER(l)" "DELEGATED(g@)" "|" "DONE(d!)" "CANCELLED(c@)")))
  (setq org-log-done 'time)
  (setq org-log-into-drawer t)
  (setq org-log-reschedule 'time)
  (setq org-hide-leading-stars t)
  (setq org-startup-folded 'content)
  (setq org-adapt-indentation nil)
  (setq org-bookmark-names-plist nil)
  (add-to-list 'org-modules 'org-habit t)
  (setq org-habit-graph-column 55)
  (setq org-export-with-sub-superscripts '{})
  (add-to-list 'org-structure-template-alist '("g" . "src go") t)
  (add-to-list 'org-structure-template-alist '("z" . "src emacs-lisp") t)
  (add-to-list 'org-file-apps '("\\.xlsx?\\'" . default))
  (add-to-list 'org-file-apps '("\\.docx?\\'" . default))
  (add-to-list 'org-file-apps '("\\.ods\\'" . default))
  (add-to-list 'org-file-apps '("\\.org_archive\\'" . emacs))
  (add-to-list 'org-export-backends 'md)
  (setq org-refile-targets '((org-agenda-files :maxlevel . 3)))
  (setq org-refile-use-outline-path 'file)
  (setq org-agenda-files (list org-directory))
  ;; Приоритет от A до E
  (setq org-priority-default 67)
  (setq org-priority-lowest 69)
  ;; org-babel
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((shell . t)))
  ;; org-capture-templates
  (add-to-list 'org-capture-templates
	       '("i" "Inbox"
		 entry (file my/org-inbox-file-name)
		 "* TODO %?%(my/org-set-created)"
		 :empty-lines 1))
  (add-to-list 'org-capture-templates
	       '("a" "Timer"
		 entry (file my/org-inbox-file-name)
		 "* TODO %?%(my/org-set-created)"
		 :empty-lines 1 :clock-in t :clock-keep t))
  (add-to-list 'org-capture-templates
	       '("e" "Event"
		 entry (file my/org-inbox-file-name)
		 "* PLANNED %?%(my/org-set-created)\nSCHEDULED: %^T"
		 :empty-lines 1))
  (add-to-list 'org-capture-templates
	       '("z" "Interrupt"
		 entry (file my/org-inbox-file-name)
		 "* %?%(my/org-set-created)"
		 :empty-lines 1 :clock-in t :clock-resume t)))

;; magit
(use-package magit
  :ensure t)

;; Тема оформления
(use-package monokai-theme
  :ensure t
  :config
  (load-theme 'monokai t))

(use-package which-key
  :ensure t
  :config
  (which-key-mode))

;; Программирование
(use-package go-mode
  :ensure t
  :init
  (setq gofmt-command "goimports")
  :hook ((before-save . gofmt-before-save)
         (go-mode . (lambda () (local-set-key [f5] 'project-compile)))))


;; музыка в emms
(use-package emms
  :ensure t
  :config
  (require 'emms-setup)
  (emms-all)
  (setq emms-player-list '(emms-player-mpv)
	emms-info-functions '(emms-info-native))
  (customize-set-variable 'emms-player-mpv-update-metadata t)
  (setq emms-streams-file "~/.emacs.d/.secret/streams.emms")

  (defun my/emms-track-description (track)
    (if (eq (emms-track-type track) 'url)
	(let ((rdo (emms-track-get track 'radio))
	      (infttl (emms-track-get track 'info-title)))
	  (if (and rdo infttl)	    
	      (format "%-50s: %s" rdo infttl)
	    (if rdo rdo
	      (emms-track-simple-description track))))
      (emms-track-simple-description track)))

  (setq emms-track-description-function 'my/emms-track-description))

;; Загрузка локального конфиг-файла
(me/load-config-file ".secret/config")

;; Отдельный файл для хранения всех пользовательских настроек
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(ignore-errors (load custom-file))
