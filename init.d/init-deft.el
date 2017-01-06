;;; init-deft.el --- Initialize emacs deft
;;; Commentary:
;; 2016 12 16 init SeJ

;;; Code:


(require 'req-package)

(req-package deft
  :bind (("<f7>" . deft)
	 ("C-c d" . deft))
  :config (progn (if (string-equal system-type "windows-nt")
		     (setq deft-directory "C:/Users/NZ891R/gdrive/todo")
		   (setq deft-directory "~/gdrive/todo"))
		 (setq deft-use-filename-as-title t)
		 (setq deft-default-extension "org")
		 (setq deft-text-mode (quote (org-mode)))
		 (setq deft-org-mode-title-prefix t)
		 (setq deft-use-filter-string-for-filename t)
		 (setq deft-auto-save-interval 0)
		 (setq deft-recursive t)
		 (setq deft-extensions (quote ("org" "text" "md" "markdown" "txt")))
		 (setq deft-org-mode-title-prefix t)))

(provide 'init-deft)
;;; init-deft.el ends here

