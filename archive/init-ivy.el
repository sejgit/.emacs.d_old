;;; init-ivy.el --- Initialize emacs ivy swiper counsel
;; Commentary:  Ivy is for quick and easy selection from a list. When Emacs prompts for a string from a list of several possible choices, Ivy springs into action to assist in narrowing and picking the right string from a vast number of choices.
;; 2016 12 16 init SeJ
;; 2017 01 06 change from req-package to use-package

;;; Code:

(use-package swiper
  :ensure t
  :bind ("C-s" . swiper)
  ("C-c C-s" . isearch-forward)
  ("C-x C-r" . ivy-recentf)
  ("M-x" . counsel-M-x)
  ("C-x C-f" . counsel-find-file)
  ("C-c g" . counsel-git)
  ("C-c j" . counsel-git-grep)
  ("C-c k" . counsel-ag)
  ("C-x l" . counsel-locate)
  ("C-S-o" . counsel-rhythmbox)
  ("C-c C-r" . ivy-resume)
  :config
  (setq ivy-use-virtual-buffers t
	ivy-count-format "(%d/%d) "
	ivy-display-style 'fancy)
  (ivy-mode))

(use-package counsel
  :ensure t
  :bind ("M-x" . counsel-M-x)
  ("C-x C-f" . counsel-find-file)
  :config
  (setq counsel-find-file-at-point t))

(use-package smex
  :ensure t
  :config (smex-initialize))


(provide 'init-ivy)
;;; init-ivy.el ends here

