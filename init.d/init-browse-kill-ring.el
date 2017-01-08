;;; init-browse-kill-ring.el --- init file for browse-kill-ring
;;; Commentary:
;; 2016 12 20 SeJ
;; 2017 01 06 change from req-package to use-package

;;; Code:


(use-package browse-kill-ring
  :ensure t
  :init (browse-kill-ring-default-keybindings))

(provide 'browse-kill-ring)
;;; init-browse-kill-ring.el ends here

