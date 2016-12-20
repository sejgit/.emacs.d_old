;;; init-browse-kill-ring.el --- init file for browse-kill-ring
;;; Commentary:
;; 2016 12 20 SeJ

;;; Code:

(require 'req-package)

(req-package browse-kill-ring
  :init (browse-kill-ring-default-keybindings))

(provide 'browse-kill-ring)
;;; init-browse-kill-ring.el ends here
