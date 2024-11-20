;; Исправление кодировки в консоли Windows

(defun windows-shell-encoding-config ()
  (defadvice shell (after my-shell-advice)
    (set-process-coding-system (get-buffer-process (current-buffer)) 'cp1251 'cp1251))
  (ad-activate 'shell))
