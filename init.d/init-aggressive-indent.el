;;; init-aggressive-indent.el --- Stephen's emacs init-aggressive-indent.el file
;;; Commentary:
;; aggressive-indent-mode
;; 2016 12 16
;; 2017 01 06 change to use-package

;;; Code:

(use-package aggressive-indent-mode
  :config (progn (global-aggressive-indent-mode t)
		 (add-to-list 'aggressive-indent-excluded-modes 'html-mode)))

(provide 'init-aggressive-indent)
;;; init-aggressive-indent.el ends here
