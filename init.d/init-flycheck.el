;;; init-flycheck.el ---  Stephen's emacs init-flycheck.el
;;; Commentary:
;; 2016 12 16
;; 2017 01 06 change from req-package to use-package
;; 2017 04 04 remove ensure went global ; defer not required for mode,bind,int

;;; Code:

(use-package flycheck-color-mode-line
  :defer 2)

(use-package flycheck
  :defer 2
  :ensure flycheck-color-mode-line
  :preface
  (declare-function flycheck-next-error flycheck nil)
  (add-hook 'flycheck-mode-hook 'flycheck-color-mode-line-mode)
  (fringe-mode (quote (4 . 0)))
  (global-flycheck-mode 1)
  :config
  (setq flycheck-emacs-lisp-load-path 'inherit)
  (setq flycheck-python-flake8-executable "flake8")
  (setq flycheck-flake8-maximum-line-length 79)
  (setq flycheck-highlighting-mode 'lines)
  (progn 	  (set-face-attribute 'flycheck-warning nil
				      :inherit 'warning
				      :underline nil)
		  (set-face-attribute 'flycheck-error nil
				      :inherit 'error
				      :underline nil)))

(use-package flycheck-pos-tip
  :defer 2
  :commands flycheck-pos-tip-error-messages
  :config (setq flycheck-display-errors-function #'flycheck-pos-tip-error-messages))

(provide 'init-flycheck)
;;; init-flycheck.el ends here
