(use-package all-the-icons
  :ensure t)

  (use-package all-the-icons-dired
    :requires all-the-icons
    :ensure t
    :hook (dired-mode . all-the-icons-dired-mode))
