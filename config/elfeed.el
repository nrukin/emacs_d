(defvar load-elfeed nil "Загружать elfeed")

(use-package elfeed
  :ensure t
  :if load-elfeed
  :bind ("C-x w" . elfeed)
  (:map elfeed-show-mode-map ("w" . elfeed-eww-show))
  :config (setq elfeed-search-title-max-width 100)

  (defun elfeed-search-format-date (date)
    (format-time-string "%Y-%m-%d %H:%M" (seconds-to-time date)))

  (defun elfeed-eww-show ()
    (interactive)
    (when (elfeed-entry-p elfeed-show-entry)
      (let ((link (elfeed-entry-link elfeed-show-entry)))
	(eww-browse-url link)))))

(use-package elfeed-org
  :ensure t
  :if load-elfeed  
  :config (elfeed-org)
  (setq rmh-elfeed-org-files (list "~/.emacs.d/.secret/elfeed.org")))
