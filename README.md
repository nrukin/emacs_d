# Мой файл конфигурации emacs

## Настройка
Команда для установки необходимых шрифтов
```
M-x all-the-icons-install-fonts
```
## Сочетания клавиш
+ `C-x w` - Запуск Elfeed
+ `C-x y` - Запуск Mastodon
+ `F9` - Команды учета рабочего времени

## Сохранение открытой статьи elfeed в org-mode
`C-c c f` - Сохраняет открытую страницу elfeed в org-файле `inbox.org`

## Примеры файлов
### secret.el
Уникальные приватные настройки: пути до файлов, каталогов, приложений, логины, пароли.

Исполняется в начале чтения init.el  
Дополнительно, в конце инициализации вызывается `secret-afterconf`

```elisp
;; rclone-org pathes
(setq rclone-path "rclone")
(setq rclone-log-path "~/org_sync/logs/org.log")
(setq rclone-filter-path "~/org_sync/filters/org")
(setq rclone-local-path "~/org/")
(setq rclone-remote-path "db:org")
(setq rclone-remote-backup "db:backup/org/")
(setq rclone-local-backup "~/org_sync/backup")

;; markdown
(setq markdown-command "multimarkdown")

;; mastodon username / instance
(setq mastodon-active-user "username")
(setq mastodon-instance-url "https://example.com")

;; run function at end of file
(defun secret-afterconf ()
  "late secret configuration"
  (interactive)
  ;; windows-rus shell encoding
  ;;run function on windows machine with shell encoding problems
  (windows-shell-encoding-config))

```
### elfeed.org
Файл перечня подписок elfeed в формате org.  
Также вызывается через `org-babel-load-file` для установки цветов тегов.
```
* Feeds                                       :elfeed:
** [[https://example1.com/full.rss][ex1]]     :ex1:
** [[https://example2.com/full.rss][ex2]]     :ex2:
* tags-coloring
=M-x list-colors-display=
** ex1
#+begin_src elisp
  (defface elfeed-ex1-tag
    '((t :foreground "PaleGreen"))
    "color for ex1 tag in elfeed")

  (push '(ex1 elfeed-ex1-tag)
	elfeed-search-face-alist)
#+end_src
** ex2
#+begin_src elisp
  (defface elfeed-ex2-tag
    '((t :foreground "#78a2b7"))
    "color for ex2 tag in elfeed")

  (push '(ex2 elfeed-ex2-tag)
	elfeed-search-face-alist)
#+end_src

```
### `~/org_sync/filters/org`

```
- *~
- .#*
- .~*#
- #*#
- .git/*
```
