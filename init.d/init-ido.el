;; Stephen's emacs init-ido.el
;; 2016 12 16


(require 'req-package)

(req-package ido
  :init (defalias 'list-buffers 'ibuffer)
  :bind (("C-x C-f" . ido-find-file))
  :config
  (ido-mode t)
  (ido-everywhere t)
  (setq ido-enable-flex-matching t
	ido-use-virtual-buffers t))

(req-package ido-ubiquitous
  :config (ido-ubiquitous-mode t)
  :loader :el-get)

(req-package flx-ido
  :require flx ido
  :config
  (flx-ido-mode 1)
  (setq ido-enable-flex-matching t)
  (setq ido-use-faces nil))

(req-package ido-at-point
  :require ido
  :config (ido-at-point-mode 1))

(provide 'init-ido)


