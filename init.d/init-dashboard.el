;;; init-dashboard.el ---  Stephen's emacs init-dashboard.el
;;; Commentary:
;;; 2016 12 16

;;; Code:

(require 'req-package)

(req-package dashboard
  :config
  (dashboard-setup-startup-hook)
  (setq dashboard-items '((recents  . 15)
			  (bookmarks . 15)))
  :require page-break-lines)

(provide 'init-dashboard)
;;; init-dashboard.el ends here

