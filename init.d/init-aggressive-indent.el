;; Stephen's emacs init-aggressive-indent.el file
;; 2016 12 16


;; aggressive-indent-mode
(require 'req-package)

(req-package aggressive-indent-mode
  :init (global-aggressive-indent-mode t)
  :config (progn (global-aggressive-indent-mode t)
		 (add-to-list 'aggressive-indent-excluded-modes 'html-mode))
  :loader :el-get)

(provide 'init-aggressive-indent)
