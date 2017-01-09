;; Stephen's emacs init-ido.el
;; 2016 12 16
;; 2017 01 09 switch from req-package to use-package

(use-package ido
  :ensure t
  :init (defalias 'list-buffers 'ibuffer)
  :bind (("C-x C-f" . ido-find-file))
  :config
  (ido-mode t)
  (ido-everywhere t)
  (setq ido-enable-flex-matching t
	ido-use-virtual-buffers t))

(use-package ido-ubiquitous
  :ensure t
  :config (ido-ubiquitous-mode t))

(use-package flx-ido
  :ensure t
  :config
  (flx-ido-mode 1)
  (setq ido-enable-flex-matching t)
  (setq ido-use-faces nil))

(use-package ido-at-point
  :ensure t
  :config (ido-at-point-mode 1))

(provide 'init-ido)
;;; init-ido.el ends here



