(require 'org)

;; org-mode activation
;; https://orgmode.org/manual/Activation.html
(global-set-key (kbd "C-c l") #'org-store-link)
(global-set-key (kbd "C-c a") #'org-agenda)
(global-set-key (kbd "C-c c") #'org-capture)

;; org-mode customize
(setq org-todo-keywords '((sequence "TODO(t)" "WAITING(w)" "PLANNED(p)" "LATER(l)" "DELEGATED(g@)" "|" "DONE(d!)" "CANCELLED(c@)")))
(setq org-hide-leading-stars t)
(setq org-log-done 'time)
(setq org-log-into-drawer t)
(setq org-log-reschedule 'time)
(setq org-refile-targets '((org-agenda-files :maxlevel . 3)))
(setq org-refile-use-outline-path 'file)
(setq org-startup-folded 'content)
(setq org-adapt-indentation nil)
(setq org-export-with-sub-superscripts '{})
(add-to-list 'org-structure-template-alist '("g" . "src go"))
(add-to-list 'org-structure-template-alist '("z" . "src emacs-lisp"))
(add-to-list 'org-file-apps '("\\.xlsx?\\'" . default))
(add-to-list 'org-export-backends 'md)

;; priorities
(setq org-priority-default 67)
  (setq org-priority-lowest 69)
  (setq org-priority-highest 65)
  (setq org-priority-faces
   '((69 . "thistle1")
     (68 . "plum1")
     (67 . "LightPink1")
     (66 . "salmon1")
     (65 . "firebrick1")))  

;; org-mode pathes
(setq org-directory "~/org/")
(setq org-agenda-files (list org-directory))

;; org-superstar mode
(use-package org-superstar
  :hook (org-mode . (lambda () (org-superstar-mode 1))))

;; org-contacts
(use-package  org-contacts
    :config
    (setq org-contacts-files '("~/org/contacts.org")))

;; org-mode capture templates
(setq org-capture-templates '())
(add-to-list 'org-capture-templates
	     '("i" "Inbox" entry (file "~/org/inbox.org") "* TODO %?\n:PROPERTIES:\n:CREATED:  %U\n:END:" :empty-lines 1))
(add-to-list 'org-capture-templates
	     '("e" "Event" entry
	       (file "~/org/inbox.org")
	       "* PLANNED %? %(org-set-tags \"event\")\nSCHEDULED: %^T\n:PROPERTIES:\n:CREATED:  %U\n:END:" :empty-lines 1))
(add-to-list 'org-capture-templates
	     '("t" "Today" entry
	       (file "~/org/inbox.org")
	       "* TODO %?\nSCHEDULED: %t\n:PROPERTIES:\n:CREATED:  %U\n:END:" :empty-lines 1))
(add-to-list 'org-capture-templates
	     '("a" "Timer" entry
	       (file "~/org/inbox.org")
	       "* TODO %?\n:PROPERTIES:\n:CREATED:  %U\n:END:" :empty-lines 1 :clock-in t :clock-keep t))
(add-to-list 'org-capture-templates
	     '("z" "Dstrb" entry
	       (file "~/org/inbox.org")
	       "* DONE %?\nCLOSED: %U\n:PROPERTIES:\n:CREATED:  %U\n:END:" :empty-lines 1 :clock-in t :clock-resume t))
(add-to-list 'org-capture-templates
	     '("c" "Cntct" entry
	       (file "~/org/contacts.org")
	       "* %(org-contacts-template-name)\n:PROPERTIES:\n:EMAIL:  %(org-contacts-template-email)\n:END:" :empty-lines 1))

(add-to-list 'org-capture-templates
	     '("n" "Note" entry
	       (file "~/org/notes.org")
	       "* %? %(org-set-property \"CREATED\" \"%U\")" :empty-lines 1))


;; org-habits
(add-to-list 'org-modules 'org-habit t)
(setq org-habit-graph-column 56)

;; use ID-s for links
(use-package org-id
  :ensure nil
  :init
  (setq org-id-link-to-org-use-id 'create-if-interactive-and-no-custom-id))

(use-package verb
  :after (org)
  :config
  (with-eval-after-load 'org
    (define-key org-mode-map (kbd "C-c C-r") verb-command-map)))
