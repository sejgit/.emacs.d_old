;;; init-aggressive-indent.el --- Stephen's emacs init-aggressive-indent.el file
;;; Commentary:
;; aggressive-indent-mode
;; 2016 12 16
;; 2017 01 06 change to use-package
;; 2017 04 04 add defer remove ensure

;;; Code:

;; el-get
(use-package el-get
  :defer t)


(use-package aggressive-indent
  :defer 2
  :config (progn (global-aggressive-indent-mode 1)
		 (add-to-list 'aggressive-indent-excluded-modes 'html-mode)))

(provide 'init-aggressive-indent)
;;; init-aggressive-indent.el ends here




