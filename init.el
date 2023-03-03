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
(defvar load-elfeed nil)
(defvar load-rclone-org nil)

;; load secret file
(ignore-errors (load (expand-file-name "secret.el" user-emacs-directory)))

;; windows-rus shell encoding
(defun windows-shell-encoding-config ()
  (defadvice shell (after my-shell-advice)
    (set-process-coding-system (get-buffer-process (current-buffer)) 'cp1251 'cp1251))
  (ad-activate 'shell))

(defun load-modular-config-files (filelist)
  (dolist (file filelist)
    (let ((modular-config-file (expand-file-name (format "%s.el" file) user-emacs-directory)))
      (when (file-exists-p modular-config-file)
	(load-file modular-config-file)))))

(load-modular-config-files '("glob"
			     "prog"
			     "org"
			     "magit"
			     "dashboard"
			     "hydra"))

;; load mastodon only if need to
(when load-mastodon
  (ignore-errors (load (expand-file-name "mastodon.el" user-emacs-directory))))

;; load elfeed only if need to
(when load-elfeed
  (ignore-errors (load (expand-file-name "elfeed.el" user-emacs-directory))))

load-rclone-org
;; load elfeed only if need to
(when load-rclone-org
  (ignore-errors (load (expand-file-name "rclone-org.el" user-emacs-directory))))

;; run secret function late configuration
(when (fboundp 'secret-afterconf)
  (secret-afterconf))

;; custom settings file
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(ignore-errors (load custom-file))
