(use-package emojify
  :ensure t
  :hook (after-init . global-emojify-mode)
  :config (emojify-set-emoji-styles '(github unicode)))
