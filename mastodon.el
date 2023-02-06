;; mastodon
(use-package mastodon
  :if load-mastodon
  :bind ("C-x y" . mastodon)
  :config
  (mastodon-discover)
  (mastodon-toot--enable-custom-emoji)
  (setq mastodon-tl--show-avatars t)
  (setq mastodon-toot--enable-custom-instance-emoji t))

(use-package lingva
  :if load-mastodon
  :config
  (setq lingva-target "ru"))
