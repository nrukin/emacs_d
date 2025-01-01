(defvar load-mastodon nil "Загружать mastodon")

  (use-package mastodon
    :if load-mastodon
    :bind ("C-x y" . mastodon)
    :ensure t
    :config
    (mastodon-discover)
    (mastodon-toot--enable-custom-emoji)
    (setq mastodon-tl--show-avatars t)
    (setq mastodon-use-emojify t)
    (setq mastodon-toot--enable-custom-instance-emoji t))
