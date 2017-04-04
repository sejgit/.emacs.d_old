;;; init-ido.el --- Stephen's emacs init-ido.el


;;; Commentary:

;;; Logs:
;; 2016 12 16
;; 2017 01 09 switch from req-package to use-package
;; 2017 04 04 remove ensure went global ; defer not required for mode,bind,int

;;; Code:

(use-package ido
  :init (defalias 'list-buffers 'ibuffer)
  :bind ("C-x C-f" . ido-find-file)
  :config
  (ido-mode t)
  (ido-everywhere t)
  (setq ido-enable-flex-matching t
	ido-use-virtual-buffers t))

(use-package ido-ubiquitous
  :defer 2
  :config (ido-ubiquitous-mode t))

(use-package flx-ido
  :defer 2
  :config
  (flx-ido-mode 1)
  (setq ido-enable-flex-matching t)
  (setq ido-use-faces nil))

(use-package ido-at-point
  :defer 2
  :config (ido-at-point-mode 1))

(provide 'init-ido)
;;; init-ido.el ends here



