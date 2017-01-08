;;; init-gist.el ---  Stephen's emacs gist
;;; Commentary:
;; 2016 12 16
;; 2017 01 06 change from req-package to use-package


;;; Code:

;; gist client
(use-package gist
  :ensure t
  :bind ("<f9>" . gist-list))


(provide 'init-gist)
;;; init-gist.el ends here


