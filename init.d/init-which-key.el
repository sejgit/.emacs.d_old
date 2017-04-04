;;; init-which-key.el --- Initialize emacs which-key
;;; Commentary:
;; 2016 12 19 init SeJ
;; 2017 01 06 change from req-package to use-package
;; 2017 04 04 remove ensure went global ; defer not required for mode,bind,int

;;; Code:

(use-package which-key
  :defer 5
  :config (which-key-mode))

(provide 'init-which-key)
;;; init-which-key.el ends here
