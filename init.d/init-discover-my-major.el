;;; init-discover-my-major.el ---  setup for discover-my-major mode
;;; Commentary:
;; 2016 12 20

;;; Code:

(require 'req-package)

(req-package discover-my-major
  :bind
  (("C-h C-m" . 'discover-my-major)
   ("C-h M-m") . 'discover-my-mode))

(provide 'init-discover-my-major)
;;; init-discover-my-major.el ends here
