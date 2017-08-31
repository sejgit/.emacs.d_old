;;; init-gist.el ---  Stephen's emacs gist
;;; Commentary:
;; gist settings for Emacs

;;; ChangeLog
;; 2016 12 16 init SeJ
;; 2017 01 06 change from req-package to use-package
;; 2017 04 04 remove ensure went global ; defer not required for mode,bind,int
;; 2017 08 25 add Hyper-g for starting gist
;; 2017 08 29 map to sej-mode-map

;;; Code:

;; gist client
(use-package gist
  :defines sej-mode-map
  :bind
  (:map sej-mode-map
	("C-M-g" . gist-list)
	("H-g" . gist-list)))


(provide 'init-gist)
;;; init-gist.el ends here


