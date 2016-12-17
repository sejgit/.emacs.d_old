;; Stephen's emacs init-smex.el
;; 2016 12 16

(require 'req-package)

(req-package smex
  :require ido
  :bind (("M-x" . smex)
	 ("M-X" . smex-major-mode-commands))
  :config (progn (setq smex-save-file (expand-file-name ".smex-items" user-emacs-directory))
		 (smex-initialize))
  :loader :el-get)

(provide 'init-smex)

