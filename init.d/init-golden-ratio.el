;;; init-deft.el --- Initialize emacs golden-ratio
;;; Commentary:
					; 2017 01 05 init SeJ

;;; Code:


(require 'req-package)

(req-package golden-ratio
  :ensure t
  :diminish golden-ratio-mode
  :init
  (golden-ratio-mode 1))

(provide 'init-golden-ratio)
;;; init-golden-ratio.el ends here
