(use-package go-mode
  :ensure t
  :init
  (setq gofmt-command "goimports")
  :hook ((before-save . gofmt-before-save)
         (go-mode . (lambda () (local-set-key [f5] 'project-compile)))))
