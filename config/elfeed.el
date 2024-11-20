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

(use-package elfeed-protocol
  :after (elfeed)
  :ensure t  
  :config
  (setq elfeed-protocol-enabled-protocols '(fever))
  (elfeed-protocol-enable))

