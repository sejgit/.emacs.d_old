;;; init-helm.el --- init helm
;;; Commentary:

;;; Log:
;; 2017 05 26 init SeJ

;;; Code:

(use-package helm
  :defer 2
  :bind
  (("C-c h" . helm-mini)
   ("C-h a" . helm-apropos)
   ("C-x C-b" . helm-buffers-list)
   ("M-y" . helm-show-kill-ring)
   ("C-x c o" . helm-occur)
   ("C-x c s" . helm-swoop)
   )
  :config
  (require 'helm-config))

(provide 'init-helm)
;;; init-helm.el ends here

