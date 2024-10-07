;;; Настройка пакетов, подключение melpa
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)
(setq package-install-upgrade-built-in t)

