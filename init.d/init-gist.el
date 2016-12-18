;;; init-gist.el ---  Stephen's emacs gist
;;; Commentary:
					; 2016 12 16


;;; Code:

(require 'req-package)

;; gist client

(req-package gist
  :bind (("<f9>" . gist-list)))


(provide 'init-gist)
;;; init-gist.el ends here


