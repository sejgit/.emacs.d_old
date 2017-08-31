;;; init-deft.el --- Initialize emacs golden-ratio
;;; Commentary:
;; 2017 01 05 init SeJ
;; 2017 01 06 change from req-package to use-package
;; 2017 04 04 remove ensure went global ; defer not required for mode,bind,int
;; 2017 08 30 map to sej-mode-map & setq golden-ratio-auto-scale
;;; Code:

(use-package golden-ratio
  :ensure t
  :defer t
  :defines sej-mode-map
  :diminish golden-ratio-mode
  :bind (:map sej-mode-map
	      ("M-'" . next-multiframe-window))
  :config
  (golden-ratio-mode 1)
  (setq golden-ratio-auto-scale t)
  (add-to-list 'golden-ratio-extra-commands 'next-multiframe-window))

(provide 'init-golden-ratio)
;;; init-golden-ratio.el ends here
