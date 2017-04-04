;;; init-batch.el --- Initialize emacs batch mode
;;; Commentary:
;; batch mode
;; 2016 12 16 init SeJ
;; 2017 01 06 change from req-package to use-package
;; 2017 04 04 remove ensure went global ; defer not required for mode,bind,int

;;; Code:


(use-package batch-mode
  :mode "\\.bat\\'")

(provide 'init-batch)
;;; init-batch.el ends here
