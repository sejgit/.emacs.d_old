;;; init-python.el ---  Stephen's emacs init-python.el
;;; Commentary:
;; 2017 03 29 SeJ init

;;; Code:

(use-package elpy
  :ensure t
  :config
  (elpy-enable)
  (setq elpy-rpc-python-command "python3")
  (elpy-use-ipython)
  )

(provide 'init-python)
;;; init-python.el ends here
