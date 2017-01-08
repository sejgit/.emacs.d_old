;;; init-flycheck.el ---  Stephen's emacs init-flycheck.el
;;; Commentary:
;; 2016 12 16
;; 2017 01 06 change from req-package to use-package


;;; Code:

(use-package flycheck
  :ensure t
  :config (progn (global-flycheck-mode 1)
		 (set-face-attribute 'flycheck-warning nil
				     :inherit 'warning
				     :underline nil)
		 (set-face-attribute 'flycheck-error nil
				     :inherit 'error
				     :underline nil)))

(use-package flycheck-pos-tip
  :ensure t
  :commands flycheck-pos-tip-error-messages
  :config (setq flycheck-display-errors-function #'flycheck-pos-tip-error-messages))

(provide 'init-flycheck)
;;; init-flycheck.el ends here
