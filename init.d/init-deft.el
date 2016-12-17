;;; init-deft.el --- Initialize emacs deft
;;; Commentary:
					; 2016 12 16 init SeJ

;;; Code:


(require 'req-package)

(req-package deft
  :bind (("<f7>" . deft))
  :config (progn (if (string-equal system-type "windows-nt")
		     (setq deft-directory "C:/Users/NZ891R/gdrive/todo")
		   (setq deft-directory "~/gdrive/todo"))
		 (setq deft-use-filename-as-title t)
		 ;;(setq deft-extension "org")
		 (setq deft-text-mode 'org-mode)
		 (setq deft-org-mode-title-prefix t)
		 (setq deft-recursive t))

  :loader :el-get)

(provide 'init-deft)\n;;; init-deft.el ends here


