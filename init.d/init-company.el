;;; init-company.el --- Company-mode configuration

;;; Commentary:
;; 2016 12 16 SeJ
;; 2017 01 07 switch from req-package to use-package
;; 2017 04 04 remove ensure went global ; defer not required for mode,bind,int

;;; Code:

(use-package yasnippet
  :defer 2)

(use-package company
  :bind
  (("C-<tab>" . company-dabbrev)
   ("M-<tab>" . company-complete)
   ("C-c C-y" . company-yasnippet))
  :config (setq global-company-mode 1
		company-idle-delay 0.1
		company-show-numbers t
		company-minimum-prefix-length 2
		company-dabbrev-downcase nil
		company-dabbrev-other-buffers t
		company-auto-complete nil
		company-dabbrev-code-other-buffers 'all
		company-dabbrev-code-everywhere t
		company-dabbrev-code-ignore-case t))


(use-package company-quickhelp
  :defer 2
  :config (setq company-quickhelp-mode 1))

(use-package company-jedi
  :defer 2
  :config (add-to-list 'company-backends 'company-jedi))

(provide 'init-company)
;;; init-company.el ends here



