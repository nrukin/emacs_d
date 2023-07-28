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

;; org-mode
(require 'org)

;; активация org-mode
(global-set-key (kbd "C-c l") #'org-store-link)
(global-set-key (kbd "C-c a") #'org-agenda)
(global-set-key (kbd "C-c c") #'org-capture)

;; статусы
(setq org-todo-keywords '((sequence "TODO(t)" "WAITING(w)" "PLANNED(p)" "LATER(l)" "DELEGATED(g@)" "|" "DONE(d!)" "CANCELLED(c@)")))

;; файлы agenda
(setq org-agenda-files "~/.emacs.d/.agenda_files")

;; общие настройки
(setq org-log-done 'time)
(setq org-log-into-drawer t)
(setq org-log-reschedule 'time)
(setq org-refile-targets '((org-agenda-files :maxlevel . 3)))
(setq org-refile-use-outline-path 'file)

;; функция установки свойства created в текущее время
(defun my/org-set-created()
  (interactive)
  (org-set-property
   "CREATED"
   (format-time-string
    "[%Y-%m-%d %a %H:%M]"
    (seconds-to-time (current-time)))))
(define-key org-mode-map (kbd "<f6>") 'my/org-set-created)

;; magit
(use-package magit
  :init
  (setq magit-commit-show-diff nil))

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
