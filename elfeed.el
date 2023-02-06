;; elfeed
(use-package elfeed
  :bind ("C-x w" . elfeed)
  :config
  (when run-on-win-p
    (setq elfeed-use-curl nil))
  (defun elfeed-search-format-date (date)
    (format-time-string "%Y-%m-%d %H:%M" (seconds-to-time date)))
  (setq elfeed-search-title-max-width 100)
  (defun elfeed-org-capture-template ()
    (if elfeed-show-entry (let ((link (elfeed-entry-link elfeed-show-entry))
				(title (elfeed-entry-title elfeed-show-entry))
				(tags (elfeed-entry-tags elfeed-show-entry)))
			    (format "* TODO %s %s%%(org-set-tags \"elfeed\")\n:PROPERTIES:\n:CREATED: %%U\n:END:\n%s\n%%?" title tags link)) "* %?"))

  (add-to-list 'org-capture-templates '("f" "Elfeed" entry
					(file "~/org/inbox.org")
					(function elfeed-org-capture-template) :empty-lines 1  :immediate-finish t)))
;; elfeed-org
(use-package elfeed-org
  :init
  (setq rmh-elfeed-org-files '("~/org/elfeed.org"))
  :config
  (elfeed-org)
  (ignore-errors (org-babel-load-file "~/org/elfeed.org")))
