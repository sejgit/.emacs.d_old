;;; init-batch.el --- Initialize emacs batch mode
;;; Commentary:
;; batch mode
;; 2016 12 16 init SeJ
;; 2017 01 06 change from req-package to use-package

;;; Code:


(use-package batch-mode
  :ensure t
  :mode "\\.bat\\'")

(provide 'init-batch)
;;; init-batch.el ends here
