;;;; Создан 2024-06-19
;;;; Вторая версия init.el файла
;;;; упрощенная, вся инициализация в массиве отдельных el файлов

;;;; Общие файлы располагаются в каталоге config
;;;; Локальные файлы - в каталоге .secret
;;;; Отдельный файл custom.el для настроек через меню

(defun me/load-config-file (file)
  "Загружает файл по имени"
  (let ((config-file (expand-file-name (format "%s.el" file) user-emacs-directory)))
    (when (file-exists-p config-file)
      (load-file config-file))))

(setq
 init-files
 '("config/windows-shell-encoding" ".secret/init" "config/packages"
   "config/emacs" "config/elfeed" "config/org" "config/discover" "config/emoji"
   "config/emms" "config/theme" "config/magit""config/prog" "config/mastodon" "config/plz"
   "config/icons" "config/dashboard" "config/hydra" ".secret/config"))

(dolist
    (init-file init-files)
  (me/load-config-file init-file))

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(ignore-errors (load custom-file))
