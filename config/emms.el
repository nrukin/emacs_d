(use-package emms
  :ensure t
  :config
  (require 'emms-setup)
  (emms-all)
  (setq emms-player-list '(emms-player-mpv)
	emms-info-functions '(emms-info-native))
  (customize-set-variable 'emms-player-mpv-update-metadata t)
  (setq emms-streams-file "~/.emacs.d/.secret/streams.emms")

  (defun my/emms-track-description (track)
    (if (eq (emms-track-type track) 'url)
	(let ((rdo (emms-track-get track 'radio))
	      (infttl (emms-track-get track 'info-title)))
	  (if (and rdo infttl)	    
	      (format "%-50s: %s" rdo infttl)
	    (if rdo rdo
	      (emms-track-simple-description track))))
      (emms-track-simple-description track)))

  (setq emms-track-description-function 'my/emms-track-description))

