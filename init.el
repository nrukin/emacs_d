;; файл создан 28.07.2023

;; пакеты
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)
(setq use-package-always-ensure t)

;; определяем запуск под windows
(defvar run-on-win-p (eq system-type 'windows-nt))

;; общие настройки
(setq default-input-method "russian-computer")

;; открывать scratch при запуске
(setq initial-buffer-choice t)

;; оформление
(use-package dracula-theme
  :config
  (load-theme 'dracula t))

;; скрыть меню, скролбары и тулбары
(menu-bar-mode -1)
(scroll-bar-mode -1)
(tool-bar-mode -1)

;; Шрифт
(add-to-list 'default-frame-alist
             '(font . "Fira Code-10"))

;; Отключить звуки
(setq ring-bell-function 'ignore)

;; Скрыть диалоги
(setq use-dialog-box nil)

;; Настройка курсора
(setq-default cursor-type 'bar)
(if (fboundp 'blink-cursor-mode) (blink-cursor-mode 0))

;; настройка календаря
(setq calendar-week-start-day 1)
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

;; magit
(use-package magit
  :init
  (setq magit-commit-show-diff nil))

;; Название команд по сочетаниям
(use-package which-key
  :config
  (which-key-mode))

;; elfeed
(setq me/load-elfeed-p nil)
(let ((elfeed-cnf-file (expand-file-name ".elfeed.el" user-emacs-directory)))
  (when (file-exists-p elfeed-cnf-file)
    (load-file elfeed-cnf-file)
    (setq me/load-elfeed-p t)))

(use-package elfeed
  :if me/load-elfeed-p
  :bind ("C-x w" . elfeed)
  :config
  (when run-on-win-p
    (setq elfeed-use-curl nil))
  (defun elfeed-search-format-date (date)
    (format-time-string "%Y-%m-%d %H:%M" (seconds-to-time date)))
  (setq elfeed-search-title-max-width 100))

(use-package org
  :preface
  (defun my/org-set-created()
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
  :config
  (setq org-todo-keywords '((sequence "TODO(t)" "WAITING(w)" "PLANNED(p)" "LATER(l)" "DELEGATED(g@)" "|" "DONE(d!)" "CANCELLED(c@)")))
  (setq org-log-done 'time)
  (setq org-log-into-drawer t)
  (setq org-log-reschedule 'time)
  (setq org-hide-leading-stars t)
  (setq org-startup-folded 'content)
  (setq org-adapt-indentation nil)
  (setq org-export-with-sub-superscripts '{})
  (add-to-list 'org-structure-template-alist '("g" . "src go") t)
  (add-to-list 'org-structure-template-alist '("z" . "src emacs-lisp") t)
  (add-to-list 'org-file-apps '("\\.xlsx?\\'" . default))
  (add-to-list 'org-file-apps '("\\.ods\\'" . default))
  (add-to-list 'org-file-apps '("\\.org_archive\\'" . emacs))
  (add-to-list 'org-export-backends 'md)
  (setq org-refile-targets '((org-agenda-files :maxlevel . 3)))
  (setq org-refile-use-outline-path 'file)
  (setq org-directory "~/org/")
  (setq org-agenda-files (list org-directory))
  ;; org-capture-templates
  (add-to-list 'org-capture-templates
	       '("i" "Inbox"
		 entry (file my/org-inbox-file-name)
		 "* TODO %? %(my/org-set-created)"
		 :empty-lines 1)))

;; Красивые иконки для хедеров
(use-package org-superstar
  :requires org
  :after (org)
  :hook (org-mode . (lambda () (org-superstar-mode 1))))

;; использовать id для внутренних ссылок
(use-package org-id
  :requires org
  :after (org)
  :ensure nil
  :init
  (setq org-id-link-to-org-use-id 'create-if-interactive-and-no-custom-id))

;; делаем HTTP-запросы прямо в org
(use-package verb
  :requires org
  :after (org)
  :config
  (with-eval-after-load 'org
    (define-key org-mode-map (kbd "C-c C-r") verb-command-map)))

;; hydra
(use-package hydra
  :defer 2
  :bind (("<f9>" . hydra-clock/body))
  :preface
  (defun my/org-clock-in-last-with-prefix-arg ()
    (interactive)
    (setq current-prefix-arg '(4)) ; C-u
    (call-interactively 'org-clock-in-last))
  :config
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
    ("l" my/org-clock-in-last-with-prefix-arg)))

  ;; отдельный файл для настроек, выполняемых через меню настроек
  (setq custom-file (expand-file-name "custom.el" user-emacs-directory))
  (ignore-errors (load custom-file))
