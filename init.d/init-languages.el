;;; init-python.el ---  Stephen's emacs init-languages.el

;;; Commentary:
;; Python settings for Emacs
;; from lots of different sources

;; ChangeLog:
;; 2017 03 29 SeJ init
;; 2017 04 04 set python-interpreter
;; 2017 04 04 remove ensure went global ; defer not required for mode,bind,int
;; 2017 05 14 adds from purcell/.emacs.d
;; 2017 05 19 add mastering Emacs python debugging with compile
;; 2017 08 25 add from EOS insert-doc-string
;; 2017 08 30 map to sej-mode-map, ensure/defer, cleanup documentation
;; 2018 06 06 add company-jedi (not sure of interactions)
;; 2018 10 09 some changes to work with language-server-protocall
;; 2018 10 10 changed to init-languages.el to hold all lsp type stuff

;;; Code:

;;
;; LSP SETUP
;;

;; Basic lsp-mode config.
;; Language modules will add their own lsp setup if this is loaded.
(use-package lsp-mode
  :ensure t)

(use-package company-lsp
  :ensure t
  :after (company lsp-mode)
  :config
  (push 'company-lsp company-backends))

(use-package lsp-ui
  :ensure t
  :quelpa (lsp-ui :fetcher github :repo "emacs-lsp/lsp-ui")
  :after lsp-mode
  :hook (lsp-mode . lsp-ui-mode)
  :bind (:map lsp-ui-mode-map
              ("C-." . lsp-ui-peek-find-definitions)
              ("C-?" . lsp-ui-peek-find-references)
              ("C-c C-j" . lsp-ui-imenu)
              ("C-\'" . lsp-ui-imenu)
              ))

;;
;; C, C++
;;

(require 'cc-mode)

;; ;; Load CEDET.
;; ;; See cedet/common/cedet.info for configuration details.
;; ;; IMPORTANT: For Emacs >= 23.2, you must place this *before* any
;; ;; CEDET component (including EIEIO) gets activated by another
;; ;; package (Gnus, auth-source, ...).
;; (load-file "~/.emacs.d/vendor/cedet/cedet-devel-load.el")

;; ;; Add further minor-modes to be enabled by semantic-mode.
;; ;; See doc-string of `semantic-default-submodes' for other things
;; ;; you can use here.
(add-to-list 'semantic-default-submodes 'global-semantic-idle-summary-mode t)
(add-to-list 'semantic-default-submodes 'global-semantic-idle-completions-mode t)
(add-to-list 'semantic-default-submodes 'global-cedet-m3-minor-mode t)

;; ;; Enable Semantic
(semantic-mode 1)

;; ;; Enable EDE (Project Management) features
;;(global-ede-mode 1)


;; Flycheck supports C, so we switch it on
(add-hook 'c-mode-common-hook #'flycheck-mode)

;; always indent with 4 spaces ; in the Linuxx kernel style
(setq-default c-default-style "linux"
	            c-basic-offset 4)

;; hungry delete is useful in C ; remove up to the next non-whitespace
(setq-default c-hungry-delete-key t)


;;
;; C#
;;

;; arduino-mode
(use-package arduino-mode
  :ensure t
  :mode "\\.ino$"
  :config
  (setq arduino-mode-home "/Users/stephenjenkins/Projects/sej/Arduino")
  (setq arduino-executable "/Applications/Arduino.app/Contents/MacOS/Arduino"))


;;
;; html
;;

(use-package web-mode
  :ensure t
  :defines web-mode-enable-comment-keywords
  :mode (("\\.phtml\\'" . web-mode)
	       ("\\.tpl\\.php\\'" . web-mode)
	       ("\\.blade\\.php\\'" . web-mode)
	       ("\\.jsp\\'" . web-mode)
	       ("\\.as[cp]x\\'" . web-mode)
	       ("\\.erb\\'" . web-mode)
	       ("\\.html?\\'" . web-mode)
	       ("\\.ejs\\'" . web-mode)
	       ("\\.php\\'" . web-mode)
	       ("\\.mustache\\'" . web-mode)
	       ("/\\(views\\|html\\|theme\\|templates\\)/.*\\.php\\'" . web-mode))
  :init
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-attr-indent-offset 2)
  (setq web-mode-attr-value-indent-offset 2)
  (setq web-mode-code-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-code-indent-offset 2)
  (setq web-mode-enable-auto-closing t)
  (setq web-mode-enable-auto-pairing t)
  (setq web-mode-enable-comment-keywords t)
  (setq web-mode-enable-current-element-highlight t))

(use-package company-web
  :ensure t
  :hook (web-mode . (lambda ()
		                  (add-to-list 'company-backends 'company-web-html)
		                  (add-to-list 'company-backends 'company-web-jade)
		                  (add-to-list 'company-backends 'company-web-slim))))

(use-package emmet-mode
  :ensure t
  :hook (web-mode sgml-mode html-mode css-mode))


;;
;; BASH (SH-MODE)
;;

(use-package lsp-sh
  :ensure t
  :functions
  :init
  (add-hook 'sh-mode-hook #'lsp-sh-enable))

(use-package company-shell
  :ensure t
  :after company
  :config
  (push 'company-shell company-backends))

;;
;; PYTHON
;;

(use-package lsp-python
  :ensure t
  :after lsp-mode
  :init
  (add-hook 'python-mode-hook #'lsp-python-enable)
  (lsp-define-stdio-client lsp-python "python"
			                     (lsp-make-traverser #'(lambda (dir)
						                                       (not (directory-files
						                                             dir
						                                             nil
						                                             "__init__.py"))))
			                     '("pyls")))

(use-package python
  :ensure t
  :after lsp-mode
  :interpreter "python"
  :defines flycheck-disabled-checkers
  :bind (:map python-mode-map
              ("<backtab>" . python-back-indent)
			        ("<f9>" . py-insert-debug))
  :hook ((python-mode . flycheck-mode)
         (python-mode . (lambda ()
		                      (add-to-list 'flycheck-disabled-checkers 'python-pylint))))
  :mode (("\\.py$" . python-mode)
         ("\\.cpy$" . python-mode)
         ("\\.vpy$" . python-mode))
  :init
  ;;(setq python-shell-interpreter "ipython"
	;;	    python-shell-interpreter-args "--simple-prompt -i")
  :config
  (define-skeleton python-insert-docstring
    "Insert a Python docstring."
    "This string is ignored!"
    "\"\"\"" - "\n\n    \"\"\"")

  (define-key python-mode-map (kbd "s-\\") 'python-insert-docstring)

  (setq fill-column 79)
  (setq-default flycheck-flake8rc "~/.config/flake8rc")
  (setq python-check-command "flake8")
  (setq tab-width 2))

;; major mode for editing pip requirement files
(use-package pip-requirements
  :ensure t
  :defer t)

;; virtualenv api in Emacs
(use-package python-environment
  :ensure t
  :defer t)

;; file comparison
(use-package ediff
  :ensure t
  :defer t
  :config
  (setq ediff-shell (getenv "$SHELL"))
  (setq-default ediff-split-window-function
		            (quote split-window-vertically)))

(use-package pyvenv
  :ensure t
  :hook (pyvenv-post-activate . pyvenv-restart-python))

(use-package company-jedi
  :ensure t
  :after company
  :config (add-to-list 'company-backends 'company-jedi))


;; (use-package jedi
;;   :ensure t
;;   :defer t
;;   :hook ((python-mode . jedi:setup)
;; 	       (lsp-mode . jedi:setup)
;;          )
;;   :preface
;;   (declare-function jedi:goto-definition jedi nil)
;;   (declare-function jedi:related-names jedi nil)
;;   (declare-function jedi:show-doc jedi nil)
;;   :bind (:map python-mode-map
;; 	            ("C-." . jedi:goto-definition)
;; 	            ("C-c r" . jedi:related-names)
;; 	            ("C-?" . jedi:show-doc))
;;   :config
;;   (autoload 'jedi:setup "jedi" nil t)
;;   (setq jedi:complete-on-dot t)
;;   )

;; (use-package company-jedi
;;   :ensure t
;;   :init
;;   (add-hook 'python-mode-hook (add-to-list 'company-backends 'company-jedi)))

;; (use-package py-autopep8
;;   :ensure t
;;   :hook (python-mode . py-autopep8-enable-on-save))

(provide 'init-languages)
;;; init-languages.el ends here
