;; dashboard
(use-package dashboard
  :config
  (setq dashboard-set-heading-icons t)
  (setq dashboard-set-file-icons t)
  (setq dashboard-projects-backend 'project-el)
  (setq dashboard-items '((recents  . 5)
                          (bookmarks . 5)
                          (registers . 5)
			  (projects . 5)))
  (setq dashboard-startup-banner 'logo)
  (setq dashboard-center-content t)

  ;; add navigator
  (setq dashboard-set-navigator t)

  ;; Format: "(icon title help action face prefix suffix)"
  (setq dashboard-navigator-buttons
	`(
	  ;; org-agenda
	  ((,(all-the-icons-faicon "calendar" :height 1.1 :v-adjust 0.0)
            "Agenda"
            ""
            (lambda (&rest _) (org-agenda-list))))))

  (when load-mastodon
    (add-to-list 'dashboard-navigator-buttons
		 `((,(all-the-icons-faicon "comments-o" :height 1.1 :v-adjust 0.0)
		    "Mastodon"
		    ""
		    (lambda (&rest _) (mastodon)))) t))

  (when load-elfeed
    (add-to-list 'dashboard-navigator-buttons
		 `((,(all-the-icons-faicon "rss" :height 1.1 :v-adjust 0.0)
		    "Elfeed"
		    ""
		    (lambda (&rest _) (elfeed)))) t))

  (when load-rclone-org
    (add-to-list 'dashboard-navigator-buttons
		 `((,(all-the-icons-faicon "download" :height 1.1 :v-adjust 0.0)
		    "ROD"
		    "rclone-org-download"
		    (lambda (&rest _) (rclone-org-download)))
		   (,(all-the-icons-faicon "upload" :height 1.1 :v-adjust 0.0)
		    "ROU"
		    "rclone-org-upload"
		    (lambda (&rest _) (rclone-org-upload)))) t))

  (dashboard-setup-startup-hook))
