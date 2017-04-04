;;; init-deft.el --- Initialize emacs golden-ratio
;;; Commentary:
;; 2017 01 05 init SeJ
;; 2017 01 06 change from req-package to use-package
;; 2017 04 04 remove ensure went global ; defer not required for mode,bind,int

;;; Code:

(use-package golden-ratio
  ;;:demand t
  :defer 2
  :diminish golden-ratio-mode
  :bind ("M-'" . next-multiframe-window)
  :config
  (golden-ratio-mode 1)
  (add-to-list 'golden-ratio-extra-commands 'next-multiframe-window))

(provide 'init-golden-ratio)
;;; init-golden-ratio.el ends here
