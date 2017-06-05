;;; init-projectile.el --- Projectile init

;;; Commentary:
;; 2017 05 14 SeJ init from purcell/.emacs.d
;; 2017 06 01 simplified & added helm-projectile

;;; Code:

(use-package projectile
  :config
  (add-hook 'after-init-hook 'projectile-global-mode))

(use-package helm-projectile
  :config
  (helm-projectile-on))

(provide 'init-projectile)
;;; init-projectile.el ends here
