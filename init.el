;; файл создан 28.07.2023

(require 'org)
(org-babel-load-file (expand-file-name "config.org" user-emacs-directory))
;; (let ((file-name-handler-alist nil))
;;   ;; If config is pre-compiled, then load that
;;   (if (file-exists-p (expand-file-name "readme.elc" user-emacs-directory))
;;       (load-file (expand-file-name "readme.elc" user-emacs-directory))
;;     ;; Otherwise use org-babel to tangle and load the configuration
;;     (require 'org)
;;     (org-babel-load-file (expand-file-name "readme.org" user-emacs-directory))))
