;;; init-goto-chg.el --- Initialize emacs goto-chg
;;; Commentary:
;; go to the last change
;; 2016 12 16 init SeJ
;; 2017 01 06 change from req-package to use-package
;; 2017 04 04 remove ensure went global ; defer not required for mode,bind,int
;; 2017 08 30 map to sej-mode-map
;;; Code:

(use-package goto-chg
  :ensure t
  :defer t
  :defines sej-mode-map
  :bind (:map sej-mode-map
              ;;("C-." . goto-last-change)
              ;; M-. can conflict with etags tag search. But C-. can get overwritten
              ;; by flyspell-auto-correct-word. And goto-last-change needs a really fast key.
              ("M-." . goto-last-change)
              ;; ensure that even in worst case some goto-last-change is available
              ("C-M-." . goto-last-change)
              ;; added reverse below
              ("C-," . goto-last-change-reverse)))

(provide 'init-goto-chg)
;;; init-goto-chg.el ends here
