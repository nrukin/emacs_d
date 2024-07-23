(use-package dashboard
  :ensure t
  :config
  (setq dashboard-set-heading-icons t)
  (setq dashboard-startup-banner 2)
  (setq dashboard-set-file-icons t)
  (setq dashboard-projects-backend 'project-el)
  (setq dashboard-items '((recents   . 5)
                          (bookmarks . 5)
                          (projects  . 5)
                          (agenda    . 5)
                          (registers . 5)))
  (setq dashboard-center-content t)
  
  (setq dashboard-startupify-list '(dashboard-insert-banner
                                    dashboard-insert-newline
                                    dashboard-insert-banner-title
                                    dashboard-insert-newline
                                    dashboard-insert-navigator
                                    dashboard-insert-newline
                                    dashboard-insert-init-info
                                    dashboard-insert-items
                                    dashboard-insert-newline
                                    dashboard-insert-footer))

  (setq dashboard-navigator-buttons
	`(
	  ;; org-agenda
	  ((,(all-the-icons-faicon "calendar" :height 1.1 :v-adjust 0.0)
	    "Agenda"
	    ""
	    (lambda (&rest _) (org-agenda-list))))))

  (add-to-list 'dashboard-navigator-buttons
	       `((,(all-the-icons-faicon "music" :height 1.1 :v-adjust 0.0)
		  "Radio"
		  ""
		  (lambda (&rest _) (emms-streams)))) t)

  (when load-mastodon
    (add-to-list 'dashboard-navigator-buttons
		 `((,(all-the-icons-faicon "comments-o" :height 1.1 :v-adjust 0.0)
		    "Mastodon"
		    ""
		    (lambda (&rest _) (mastodon)))) t))
  
  (dashboard-setup-startup-hook))
