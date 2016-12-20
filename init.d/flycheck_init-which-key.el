;;; init-which-key.el --- Initialize emacs which-key
;;; Commentary:
;; 2016 12 19 init SeJ

;;; Code:


(require 'req-package)

(req-package which-key
  :config (progn (if (string-equal system-type "windows-nt")
		     (setq deft-directory "C:/Users/NZ891R/gdrive/todo")
		   (setq deft-directory "~/gdrive/todo"))
		 (setq deft-use-filename-as-title t)
		 (setq deft-extension "org" "txt")
		 (setq deft-text-mode 'org-mode)
		 (setq deft-org-mode-title-prefix t)
		 (setq deft-recursive t)))

(provide 'init-deft)
;;; init-deft.el ends here
