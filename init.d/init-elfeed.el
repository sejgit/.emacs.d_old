;;; init-elfeed.el --- Initialize emacs elfeed
;;; Commentary:
;; settings and packages for elfeed in Emacs

;;; ChangeLog
;; 2017 01 05 init SeJ
;; 2017 01 06 change from req-package to use-package
;; 2017 08 19 disabled for now until better use case

;;; Code:
(use-package elfeed-org
  :disabled ;; for now until better use case
  :defer t
  :ensure t
  :config (elfeed-org)
  (setq rmh-elfeed-org-files (list ".emacs.d/elfeed.org"))
  )

(use-package elfeed
  :disabled ;; for now until better use case
  :defer t
  :ensure t)

(provide 'init-elfeed)
;;; init-elfeed.el ends here
