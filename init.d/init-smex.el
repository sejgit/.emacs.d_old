;;; init-smex.el --- Stephen's emacs init-smex.el
;;; Commentary:
;; provides a convenient interface to your recently and most frequently used commands.

;; 2016 12 16
;; 2017 01 07 switch from req-package to use-package
;;; Code:

(use-package smex
  :ensure t
  :bind (("M-x" . smex)
	 ("M-X" . smex-major-mode-commands))
  :config (progn (setq smex-save-file (expand-file-name ".smex-items" user-emacs-directory))
		 (smex-initialize)))

(provide 'init-smex)
;;; init-smex ends here
