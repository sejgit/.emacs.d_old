;;; init-yasnippet.el --- Initialize emacs yasnippet
;;; Commentary:
;; 2017 03 29 SeJ init
;; 2017 04 04 remove ensure went global ; defer not required for mode,bind,int

;;; Code:

(use-package yasnippet
  :defer 2
  :disabled
  :config (yas-global-mode 1)
  )

(provide 'init-yasnippet)
;;; init-yasnippet.el ends here

