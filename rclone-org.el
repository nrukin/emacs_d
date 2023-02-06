;; rclone org-cat sync

(defvar rclone-path "" "path to rclone binary")
(defvar rclone-log-path "" "path to sync log file")
(defvar rclone-filter-path "" "path to rclone filter path")
(defvar rclone-local-path "" "path to local directory")
(defvar rclone-remote-path "" "path to remote directory")
(defvar rclone-remote-backup "" "path to remote backup directory")
(defvar rclone-local-backup "" "path to local backup directory")

(defun rclone-org-upload ()
  "upload org-files to remote via rclone"
  (interactive)
    (shell-command (format "%s sync --filter-from %s --log-file %s --log-level INFO --backup-dir %s %s %s"
	rclone-path
	rclone-filter-path
	rclone-log-path
	(concat rclone-remote-backup "remote\\" (format-time-string "%Y%m%d_%H%M%S" (current-time)))
	rclone-local-path
	rclone-remote-path))
    (message "Upload complete"))

(defun rclone-org-download ()
  "download org-files from remote via rclone"
  (interactive)
    (shell-command (format "%s sync --filter-from %s --log-file %s --log-level INFO --backup-dir %s %s %s"
	rclone-path
	rclone-filter-path
	rclone-log-path
	(concat rclone-local-backup (format-time-string "%Y%m%d_%H%M%S" (current-time)))
	rclone-remote-path
	rclone-local-path))
    (message "Download complete"))

(defun rclone-backup-upload ()
  "upload backup files to cloud storage"
  (interactive)
    (elfeed-db-save)
    (shell-command (format "%s move --log-file %s --log-level INFO --delete-empty-src-dirs %s %s"
	rclone-path
	rclone-log-path
	rclone-local-backup
	(concat rclone-remote-backup (system-name) "\\")
	))
    (message "Backup move complete"))

(defun save-sync-and-quit ()
  (interactive)
  (when (y-or-n-p "Save, sync and exit emacs?")
    (when (org-clocking-buffer)
      (org-clock-out))
    (save-some-buffers t)
    (rclone-org-upload)
    (kill-emacs)))

(global-set-key (kbd "C-c q") 'save-sync-and-quit)
