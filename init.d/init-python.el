;;; init-python.el ---  Stephen's emacs init-python.el
;;; Commentary:
;; 2017 03 29 SeJ init

;;; Code:

(use-package ediff
  :config
  (setq ediff-shell (getenv "$SHELL"))
  (setq-default ediff-split-window-function
		(quote split-window-vertically)))

;; (use-package elpy
;;   :ensure t
;;   :config
;;   (elpy-enable)
;;   (setq elpy-rpc-python-command "python3")
;;   )

(use-package python
  :bind (("<kp-5>" . py-insert-debug)
         ("<f9>" . py-insert-debug))
  :mode (("\\.py$" . python-mode)
         ("\\.cpy$" . python-mode)
         ("\\.vpy$" . python-mode))
  :config
  (declare-function py-insert-debug netsight nil)
  (setq fill-column 79)
  (setq-default flycheck-flake8rc "~/.config/flake8rc")
  (setq python-check-command "flake8")
  (setq tab-width 4))

(use-package pyvenv)
(use-package pyenv-mode-auto)
(provide 'init-python)
(use-package magit)

(use-package jedi
  :init
  (autoload 'jedi:setup "jedi" nil t)
  (add-hook 'python-mode-hook 'jedi:setup)
  (setq jedi:setup-keys t)
  (setq jedi:complete-on-dot t)
  :preface
  (declare-function jedi:goto-definition jedi nil)
  (declare-function jedi:related-names jedi nil)
  (declare-function jedi:show-doc jedi nil)
  :bind (("C-." . jedi:goto-definition)
	 ("C-c r" . jedi:related-names)
	 ("C-?" . jedi:show-doc)))


;;; init-python.el ends here
