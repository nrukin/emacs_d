(add-hook 'prog-mode-hook (lambda()
			    (toggle-truncate-lines 1)
			    (display-line-numbers-mode 1)
			    (hs-minor-mode)))
(electric-pair-mode 1)
(which-function-mode 1)
(setq compilation-ask-about-save nil)
(setq compilation-scroll-output t)

(use-package rainbow-delimiters
  :ensure t
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package go-mode
  :ensure t
  :demand t
  :init
  (setq gofmt-command "goimports")
  :hook ((before-save . gofmt-before-save)
         (go-mode . (lambda () (local-set-key [f5] 'project-compile)))))

(use-package python
  :demand t)

(use-package go-dlv
  :ensure t)

(use-package gotest
  :ensure t)

(use-package go-playground
  :ensure t)

(use-package company
  :ensure t
  :hook (prog-mode . company-mode))

(use-package yasnippet
  :ensure t
  :config (yas-global-mode 1))

(use-package yasnippet-snippets
  :ensure t
  :after yasnippet)

(use-package eglot
  :ensure nil
  :hook (go-mode . eglot-ensure)
  (python-mode . eglot-ensure)
  (csharp-mode . eglot-ensure)
  (c++-mode . eglot-ensure)
  :after (yasnippet company))

(use-package plantuml-mode
  :mode ".puml"
  :ensure t
  :config (add-to-list
	   'org-src-lang-modes '("plantuml" . plantuml)))

(use-package yaml-mode
  :mode ".yml"
  :ensure t)

(use-package imenu-list
  :ensure t
  :bind ("C-'" . imenu-list-smart-toggle))

(use-package treemacs
  :defer t
  :ensure t
  :bind ("C-x !" . treemacs-select-window))
