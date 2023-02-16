;; prog mode config:
;; - display line numbers
;; - truncate lines
(add-hook 'prog-mode-hook (lambda()
			    (toggle-truncate-lines 1)
			    (display-line-numbers-mode 1)))

;; electric-pair
(electric-pair-mode 1)

;; rainbow-delimeters
(use-package rainbow-delimiters
  :hook prog-mode)

;; go
(use-package go-mode
  :init
  (setq gofmt-command "goimports")
  :hook ((before-save . gofmt-before-save)
	 (go-mode . (lambda () (local-set-key [f5] 'project-compile)))))

(use-package go-dlv)
(use-package gotest)

;; yasnippet
(use-package yasnippet
  :config (yas-global-mode 1))

;; eglot
(use-package eglot
  :hook (go-mode . eglot-ensure))

;; company
(use-package company
  :hook prog-mode)

;; compiling
(setq compilation-ask-about-save nil)

;; local variables used in compiling
(add-to-list 'safe-local-variable-values '(compilation-scroll-output . t))
(add-to-list 'safe-local-variable-values '(compilation-read-command))
(add-to-list 'safe-local-variable-values '(compile-command . "go run ."))
(add-to-list 'safe-local-variable-values '(compile-command . "./run.sh"))

;; json-mode
(use-package json-mode)

;; auto-hotkey
(use-package ahk-mode)

;; markdown-mode
(use-package markdown-mode
  :mode ("README\\.md\\'" . gfm-mode))
