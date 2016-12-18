;;; init-breadcrumb.el --- Initialize emacs breadcrumb
;;; Commentary:
					; 2016 12 16 init SeJ

;;; Code:


(require 'req-package)

(req-package breadcrumb
  :bind (("S-SPC" . bc-set)             ; shift space for set bookmark
	 ("M-j" . bc-previous)          ; M-j for jump to previous
	 ("S-M-J" . bc-next)            ; Shift-M-j for jump to next
	 ("M-<UP>" . bc-local-previous) ; M-up-arrow for local previous
	 ("M-<DOWN>" . bc-local-next)   ; M-down-arrow for local next
	 ("C-c j" . bc-goto-current)    ; C-c j for jump to current bookmark
	 ("C-x M-j" . bc-list)          ; C-x M-j for the bookmark menu list
	 ))

(provide 'init-breadcrumb)
;;; init-breadcrumb.el ends here
