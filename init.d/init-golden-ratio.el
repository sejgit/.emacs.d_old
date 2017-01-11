;;; init-deft.el --- Initialize emacs golden-ratio
;;; Commentary:
;; 2017 01 05 init SeJ
;; 2017 01 06 change from req-package to use-package

;;; Code:

(use-package golden-ratio
  :ensure t
  :diminish golden-ratio-mode
  :config
  (golden-ratio-mode 1)
  (global-set-key (kbd "s-'") 'next-multiframe-window)
  (add-to-list 'golden-ratio-extra-commands 'next-multiframe-window))

(provide 'init-golden-ratio)
;;; init-golden-ratio.el ends here
