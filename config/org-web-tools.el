(use-package org-web-tools
  :ensure t
  :after (org elfeed)
  :config
  (defun my/get-org-from-url (url)
	 (-let* ((dom (plz 'get url :as #'org-web-tools--sanitized-dom))
	  ((title . readable) (org-web-tools--eww-readable dom))
	  (title (org-web-tools--cleanup-title (or title "")))
	  (converted (org-web-tools--html-to-org-with-pandoc readable))
	  (timestamp (format-time-string (org-time-stamp-format 'with-time 'inactive))))
    (with-temp-buffer
      (org-mode)
      (insert converted)
      (goto-char (point-min))
      (insert "* " title "\n")
      (org-set-property "CREATED" timestamp)
      (org-set-property "URL" url)
      (buffer-string))))
  
  (defun grab-current-elfeed-entry-as-org ()
    (if elfeed-show-entry (let ((link (elfeed-entry-link elfeed-show-entry)))
			    (my/get-org-from-url link))))

  (add-to-list 'org-capture-templates '("n" "Elfeed-Capture" entry
					(file my/org-inbox-file-name)
					(function grab-current-elfeed-entry-as-org) :empty-lines 1 :immediate-finish t)))
