;;; init-elfeed.el --- Initialize emacs elfeed
;;; Commentary:
;; 2017 01 05 init SeJ
;; 2017 01 06 change from req-package to use-package
;;; Code:

(use-package elfeed-org
  :defer t
  :ensure t
  :config (elfeed-org)
  (setq rmh-elfeed-org-files (list ".emacs.d/elfeed.org"))
  )

(use-package elfeed
  :defer t
  :ensure t)

(provide 'init-elfeed)
;;; init-elfeed.el ends here
