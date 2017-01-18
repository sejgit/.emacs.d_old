;;; init-dashboard.el ---  Stephen's emacs init-dashboard.el
;;; Commentary:
;;; 2016 12 16
;; 2017 01 06 change from req-package to use-package

;;; Code:

(use-package dashboard
  :config
  (use-package page-break-lines
    :ensure t)
  (dashboard-setup-startup-hook)
  (setq dashboard-items '((recents  . 15)
			  (bookmarks . 15)))
  (dashboard-insert-startupify-lists))


(provide 'init-dashboard)
;;; init-dashboard.el ends here
