(use-package all-the-icons
  :ensure t
  :if (display-graphic-p))

(defmacro get-dashboard-faicon (icon icon-name)
  (if (display-graphic-p)
      (all-the-icons-faicon icon-name :height 1.1 :v-adjust 0.0)
    icon))
