;;; init-smex.el --- Stephen's emacs init-smex.el
;;; Commentary:
;; provides a convenient interface to your recently and most frequently used commands.

;; 2016 12 16
;; 2017 01 07 switch from req-package to use-package
;; 2017 04 04 remove ensure went global ; defer not required for mode,bind,int
;; 2017 05 18 simplify

;;; Code:

(use-package smex
  :bind (("M-x" . smex)
	 ("M-X" . smex-major-mode-commands))
  :config
  (setq smex-save-file (expand-file-name ".smex-items" user-emacs-directory))
  (smex-initialize))

(provide 'init-smex)
;;; init-smex ends here
