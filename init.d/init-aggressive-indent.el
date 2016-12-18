;;; init-aggressive-indent.el --- Stephen's emacs init-aggressive-indent.el file
;;; Commentary:
					; aggressive-indent-mode
					; 2016 12 16

;;; Code:

(require 'req-package)

(req-package aggressive-indent-mode
  :init (global-aggressive-indent-mode t)
  :config (progn (global-aggressive-indent-mode t)
		 (add-to-list 'aggressive-indent-excluded-modes 'html-mode)))

(provide 'init-aggressive-indent)
;;; init-aggressive-indent.el ends here
