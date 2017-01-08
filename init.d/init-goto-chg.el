;;; init-goto-chg.el --- Initialize emacs goto-chg
;;; Commentary:
;; go to the last change
;; 2016 12 16 init SeJ
;; 2017 01 06 change from req-package to use-package

;;; Code:

(use-package goto-chg
  :ensure t
  :bind (("C-." . goto-last-change)
	 ;; M-. can conflict with etags tag search. But C-. can get overwritten
	 ;; by flyspell-auto-correct-word. And goto-last-change needs a really fast key.
	 ("M-." . goto-last-change)
	 ;; ensure that even in worst case some goto-last-change is available
	 ("C-M-." . goto-last-change)
	 ;; added reverse below
	 ("C-," . goto-last-change-reverse)))

(provide 'init-goto-chg)
;;; init-goto-chg.el ends here
