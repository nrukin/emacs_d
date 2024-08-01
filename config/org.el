(defvar org-directory "~/org" "Каталог файлов org-mode")

(use-package org
  :preface (defun my/org-set-created()
	     (interactive)
	     (org-set-property
	      "CREATED"
	      (format-time-string
	       "[%Y-%m-%d %a %H:%M]"
	       (seconds-to-time (current-time)))))
  (defun my/org-inbox-file-name()
    (file-name-concat org-directory  "inbox.org"))
  (setq org-capture-templates '())
  :bind (("C-c l" . org-store-link)
	 ("C-c a" . org-agenda)
	 ("C-c c" . org-capture)
	 :map org-mode-map
	 ("<f6>" . my/org-set-created))
  :config
  (setq org-todo-keywords '((sequence "TODO(t)" "PROGRESS(s)" "WAITING(w)" "PLANNED(p)" "LATER(l)" "DELEGATED(g@)" "|" "DONE(d!)" "CANCELLED(c@)")))
  (setq org-log-done 'time)
  (setq org-log-into-drawer t)
  (setq org-log-reschedule 'time)
  (setq org-hide-leading-stars t)
  (setq org-startup-folded 'content)
  (setq org-adapt-indentation nil)
  (setq org-bookmark-names-plist nil)
  (add-to-list 'org-modules 'org-habit t)
  (setq org-habit-graph-column 55)
  (setq org-export-with-sub-superscripts '{})
  (add-to-list 'org-structure-template-alist '("g" . "src go") t)
  (add-to-list 'org-structure-template-alist '("z" . "src emacs-lisp") t)
  (add-to-list 'org-file-apps '("\\.xlsx?\\'" . default))
  (add-to-list 'org-file-apps '("\\.docx?\\'" . default))
  (add-to-list 'org-file-apps '("\\.ods\\'" . default))
  (add-to-list 'org-file-apps '("\\.org_archive\\'" . emacs))
  (add-to-list 'org-export-backends 'md)
  (setq org-refile-targets '((org-agenda-files :maxlevel . 3)))
  (setq org-refile-use-outline-path 'file)
  (setq org-agenda-files (list org-directory))
  (setq org-priority-default 67)
  (setq org-priority-lowest 69)
  (setq org-clock-out-remove-zero-time-clocks t)
  (setq org-clock-report-include-clocking-task t)
  (setq org-global-properties
	'(("Effort_ALL" . "0 0:10 0:30 1:00 2:00 3:00 4:00 5:00 6:00 7:00")))
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((shell . t)))

  ;; https://nrukin.github.io/jira-prop-hotkey.html
  (defun org-set-jira(jira-value)
    "set jira property at current heading"
    (interactive (list (read-from-minibuffer "Jira? " (org-entry-get nil "jira"))))
    (org-set-property "jira" jira-value))

  (define-key org-mode-map (kbd "<f7>") 'org-set-jira)
  
  (add-to-list 'org-capture-templates
	       '("i" "Inbox"
		 entry (file my/org-inbox-file-name)
		 "* TODO %?%(my/org-set-created)"
		 :empty-lines 1))
  (add-to-list 'org-capture-templates
	       '("a" "Timer"
		 entry (file my/org-inbox-file-name)
		 "* TODO %?%(my/org-set-created)"
		 :empty-lines 1 :clock-in t :clock-keep t))
  (add-to-list 'org-capture-templates
	       '("e" "Event"
		 entry (file my/org-inbox-file-name)
		 "* PLANNED %?%(my/org-set-created)\nSCHEDULED: %^T"
		 :empty-lines 1))
  (add-to-list 'org-capture-templates
	       '("z" "Interrupt"
		 entry (file my/org-inbox-file-name)
		 "* %?%(my/org-set-created)"
		 :empty-lines 1 :clock-in t :clock-resume t)))

(use-package org-id
  :requires org
  :after (org)
  :ensure nil
  :init
  (setq org-id-link-to-org-use-id 'create-if-interactive-and-no-custom-id))

(use-package toc-org
  :after org
  :ensure t
  :hook (org-mode . toc-org-mode))

(use-package org-superstar
  :after org
  :ensure t
  :hook (org-mode . (lambda () (org-superstar-mode 1))))

(defun my/ediff-org-prepare ()
  (when (eq major-mode 'org-mode)
    (org-show-all)))

(add-hook 'ediff-prepare-buffer-hook #'my/ediff-org-prepare)
