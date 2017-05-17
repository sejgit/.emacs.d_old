;;; init-python.el ---  Stephen's emacs init-python.el
;;; Commentary:
;; 2017 03 29 SeJ init
;; 2017 04 04 set python-interpreter
;; 2017 04 04 remove ensure went global ; defer not required for mode,bind,int
;; 2017 05 14 adds from purcell/.emacs.d
;;; Code:

(use-package pip-requirements)
(use-package python-environment)
(use-package ediff
  :defer t
  :config
  (setq ediff-shell (getenv "$SHELL"))
  (setq-default ediff-split-window-function
		(quote split-window-vertically)))

(use-package python
  :interpreter "python"
  :bind (("<kp-5>" . py-insert-debug)
         ("<f9>" . py-insert-debug))
  :mode (("\\.py$" . python-mode)
         ("\\.cpy$" . python-mode)
         ("\\.vpy$" . python-mode))
  :init
  (setq python-shell-interpreter "ipython3 --simple-prompt -i"
	python-shell-interpreter-args "--simple-prompt -i")
  :config
  (use-package anaconda-mode
    :config
    (add-hook 'python-mode-hook 'anaconda-mode)
    (add-hook 'python-mode-hook 'anaconda-eldoc-mode))
  (use-package company-anaconda
    :config
    (add-hook 'python-mode-hook
	      (lambda () (sanityinc/local-push-company-backend 'company-anaconda))))
  (declare-function py-insert-debug netsight nil)
  (setq fill-column 79)
  (setq-default flycheck-flake8rc "~/.config/flake8rc")
  (setq python-check-command "flake8")
  (setq tab-width 4))

(use-package pyvenv
  :defer t)
(use-package pyenv-mode-auto
  :defer t)

(use-package jedi
  :init
  (autoload 'jedi:setup "jedi" nil t)
  (add-hook 'python-mode-hook 'jedi:setup)
  (setq jedi:complete-on-dot t)
  :preface
  (declare-function jedi:goto-definition jedi nil)
  (declare-function jedi:related-names jedi nil)
  (declare-function jedi:show-doc jedi nil)
  :bind (("C-." . jedi:goto-definition)
	 ("C-c r" . jedi:related-names)
	 ("C-?" . jedi:show-doc)))

(provide 'init-python)
;;; init-python.el ends here
