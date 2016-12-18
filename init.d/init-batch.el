;;; init-batch.el --- Initialize emacs batch mode
;;; Commentary:
					; batch mode
					; 2016 12 16 init SeJ

;;; Code:


(require 'req-package)

(req-package batch-mode :mode "\\.bat\\'")

(provide 'init-batch)
;;; init-batch.el ends here
