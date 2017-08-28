;;; init-gist.el ---  Stephen's emacs gist
;;; Commentary:
;; 2016 12 16
;; 2017 01 06 change from req-package to use-package
;; 2017 04 04 remove ensure went global ; defer not required for mode,bind,int
;; 2017 08 25 add Hyper-g for starting gist
;;; Code:

;; gist client
(use-package gist
  :bind
  ("C-M-g" . gist-list)
  ("H-g" . gist-list)
  )


(provide 'init-gist)
;;; init-gist.el ends here


