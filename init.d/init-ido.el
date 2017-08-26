;;; init-ido.el --- Stephen's emacs init-ido.el


;;; Commentary:

;;; Logs:
;; 2016 12 16
;; 2017 01 09 switch from req-package to use-package
;; 2017 04 04 remove ensure went global ; defer not required for mode,bind,int
;; 2017 05 16 add recentf
;; 2017 08 24 move smex to this file
;;; Code:

(use-package ido
  :init (defalias 'list-buffers 'ibuffer)
  :bind ("C-x C-f" . ido-find-file)
  :commands
  ido-everywhere
  :config
  (ido-mode t)
  (ido-everywhere t)
  (setq ido-enable-flex-matching nil
	ido-use-virtual-buffers t
	ido-enable-prefix nil
        ido-use-filename-at-point 'guess
        ido-max-prospects 10
	ido-create-new-buffer 'always
	ido-ignore-extensions t
	ido-file-extensions-order '(".org" ".txt" ".py" ".emacs" ".xml" ".el" ".ini" ".cfg" ".cnf")))

(use-package ido-completing-read+
  :defer 2
  :config (ido-ubiquitous-mode t))

(use-package flx-ido
  :defer 2
  :config
  (flx-ido-mode 1)
  (setq ido-use-faces nil))

(use-package ido-at-point
  :defer 2
  :config (ido-at-point-mode))

(use-package smex
  :bind (("M-x" . smex)
	 ("M-X" . smex-major-mode-commands))
  :config
  (setq smex-auto-update 60)
  (setq smex-save-file (expand-file-name ".smex-items" user-emacs-directory)))

(provide 'init-ido)
;;; init-ido.el ends here



