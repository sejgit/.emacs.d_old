;;; init-registers.el --- Set up registers

;;; Commentary:

;; 2017 05 09 init copied from Part of the Emacs Starter Kit

;; Registers allow you to jump to a file or other location
;; quickly.  Use C-x r j followed by the letter of the register (i for
;; init.el, r for this file) to jump to it.

;; You should add registers here for the files you edit most often.


;;; Code:

(dolist (r `((?i (file . ,"~/.emacs.d/init.el"))
	     (?b (file . ,"~/.emacs.d/init.d/init-bindings-settings.el"))
	     (?m (file . ,"~/.emacs.d/init.d/init-misc-pkgs.el"))
	     (?r (file . ,"~/.emacs.d/init.d/init-registers.el"))))
  (set-register (car r) (cadr r)))

(provide 'init-registers)
;;; init-registers.el ends here

