;;; init-aggressive-indent.el --- Stephen's emacs init-aggressive-indent.el file
;;; Commentary:
;; aggressive-indent-mode
;; 2016 12 16
;; 2017 01 06 change to use-package

;;; Code:

;; el-get
(use-package el-get
  :ensure t)


(use-package aggressive-indent
  :ensure t
  :config (progn (global-aggressive-indent-mode 1)
		 (add-to-list 'aggressive-indent-excluded-modes 'html-mode)))

(provide 'init-aggressive-indent)
;;; init-aggressive-indent.el ends here
