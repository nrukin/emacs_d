;; (defvar use-dark-theme nil "Использовать темную тему")

;; (use-package modus-themes
;;   :ensure t
;;   :config
;;   (if use-dark-theme
;;       (load-theme 'modus-vivendi t)
;;     (load-theme 'modus-operandi t)))

(use-package zenburn-theme
  :ensure t
  :config
  (load-theme 'zenburn t))
