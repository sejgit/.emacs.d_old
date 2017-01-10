;;; init-aggressive-indent.el --- Stephen's emacs init-aggressive-indent.el file
;;; Commentary:
;; aggressive-indent-mode
;; 2016 12 16
;; 2017 01 06 change to use-package

;;; Code:

;; el-get
(use-package el-get
  :ensure t
  :config
 (add-to-list 'el-get-recipe-path "~/.emacs.d/el-get-user/recipes")
 (el-get 'sync))

(use-package aggressive-indent-mode
  :ensure t
  :config (progn (global-aggressive-indent-mode t)
		 (add-to-list 'aggressive-indent-excluded-modes 'html-mode)))

(provide 'init-aggressive-indent)
;;; init-aggressive-indent.el ends here
