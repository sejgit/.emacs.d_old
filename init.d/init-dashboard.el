;; Stephen's emacs init-dashboard.el
;; 2016 12 16

(require 'req-package)

(req-package dashboard
  :config
  (dashboard-setup-startup-hook)
  (setq dashboard-items '((recents  . 10)
			  (bookmarks . 5)))
  :require page-break-lines)

(provide 'init-dashboard)
