(use-package dashboard
  :ensure t
  :config
  (setq dashboard-set-heading-icons t)
  (setq dashboard-set-file-icons t)
  (setq dashboard-startup-banner 2)
  (setq dashboard-projects-backend 'project-el)
  (setq dashboard-items '((recents   . 5)
                          (bookmarks . 5)
                          (projects  . 5)
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
	  ((, (get-dashboard-faicon "#" "calendar")
	    "Agenda"
	    ""
	    (lambda (&rest _) (org-agenda-list))))))

  (when load-emms
    (add-to-list 'dashboard-navigator-buttons
		 `((,(get-dashboard-faicon "%" "music")
		    "Radio"
		    ""
		    (lambda (&rest _) (emms-streams)))) t))

  (when load-elfeed
    (add-to-list 'dashboard-navigator-buttons
		 `((,(get-dashboard-faicon "$" "rss")
		    "Elfeed"
		    ""
		    (lambda (&rest _) (elfeed)))) t))

  (when load-mastodon
    (add-to-list 'dashboard-navigator-buttons
		 `((,(get-dashboard-faicon "@" "comments-o")
		    "Mastodon"
		    ""
		    (lambda (&rest _) (mastodon)))) t))

  (setq initial-buffer-choice (lambda () (get-buffer-create dashboard-buffer-name)))
  
  (dashboard-setup-startup-hook))
