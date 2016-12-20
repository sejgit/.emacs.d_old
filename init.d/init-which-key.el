;;; init-which-key.el --- Initialize emacs which-key
;;; Commentary:
;; 2016 12 19 init SeJ

;;; Code:

(require 'req-package)

(req-package which-key
  :init (which-key-mode))

(provide 'init-which-key)
;;; init-which-key.el ends here
