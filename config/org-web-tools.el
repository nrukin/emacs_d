(use-package org-web-tools
  :ensure t
  :after (org elfeed)
  :config
  (defun grab-current-elfeed-entry-as-org ()
    (if elfeed-show-entry (let ((link (elfeed-entry-link elfeed-show-entry))
				(title (elfeed-entry-title elfeed-show-entry)))
			    (org-web-tools--url-as-readable-org link))))

  (add-to-list 'org-capture-templates '("n" "Elfeed-Capture" entry
					(file my/org-inbox-file-name)
					(function grab-current-elfeed-entry-as-org) :empty-lines 1 :immediate-finish t)))
