;;; init-browse-kill-ring.el --- init file for browse-kill-ring
;;; Commentary:
;; 2016 12 20 SeJ
;; 2017 01 06 change from req-package to use-package
;; 2017 04 04 remove ensure went global ; defer not required for mode,bind,int

;;; Code:


(use-package browse-kill-ring
  :defer 2
  :init (browse-kill-ring-default-keybindings))

(provide 'browse-kill-ring)
;;; init-browse-kill-ring.el ends here

