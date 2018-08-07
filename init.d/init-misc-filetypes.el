;;; init-misc-filetypes.el --- settings for miscellaneous filetypes

;;; Commentary:
;; Lots of filetype modes not deserving of their own file so far.

;;; ChangeLog:
;; 2017 09 07 init SeJ moved simple modes from init-misc.pkgs & others to its own file
;; 2018 06 06 added JSON & web-mode etc from dotemacs/emacs.org at master · vidjuheffex/dotemacs
;; 2018 08 06 deleted init-js and added here js2-mode

;;; Table of contents
;; arduino-mode
;; batch-mode
;; conf-mode
;; crontab-mode
;; csv-mode
;; csv-nav
;; nov-mode ;; nov.el for epub
;; php-mode
;; textile-mode
;; yaml-mode
;; JSON-mode
;; js2-mode for javascript
;; web-mode with company-web emmet-mode rainbow-mode


;;; Code:

;; arduino-mode
(use-package arduino-mode
  :ensure t
  :mode "\\.ino$"
  :config
  ;; ;; Load CEDET.
  ;; ;; See cedet/common/cedet.info for configuration details.
  ;; ;; IMPORTANT: For Emacs >= 23.2, you must place this *before* any
  ;; ;; CEDET component (including EIEIO) gets activated by another
  ;; ;; package (Gnus, auth-source, ...).
  ;; (load-file "~/.emacs.d/vendor/cedet/cedet-devel-load.el")

  ;; ;; Add further minor-modes to be enabled by semantic-mode.
  ;; ;; See doc-string of `semantic-default-submodes' for other things
  ;; ;; you can use here.
  ;; (add-to-list 'semantic-default-submodes 'global-semantic-idle-summary-mode t)
  ;; (add-to-list 'semantic-default-submodes 'global-semantic-idle-completions-mode t)
  ;; (add-to-list 'semantic-default-submodes 'global-cedet-m3-minor-mode t)

  ;; ;; Enable Semantic
  ;; (semantic-mode 1)

  ;; ;; Enable EDE (Project Management) features
  ;; (global-ede-mode 1)

  ;; ;; Configure arduino OS X dirs.
  ;; (setq ede-arduino-appdir "/Applications/Arduino.app/Contents/Resources/Java")
  )

(use-package batch-mode
  :load-path "lisp/batch-mode"
  :mode "\\.bat\\'")

;; major mode for editing conf/ini/properties files
(use-package conf-mode
  :ensure t
  :diminish conf-mode
  :mode "\\.gitconfig$")


;; ;; editing of crontab scheduling files (removed as seems not on melpa)
;; (use-package crontab-mode
;;   :ensure t
;;   :defer t
;;   :mode "\\.?cron\\(tab\\)?\\'")

;; major mode for csv
(use-package csv-mode
  :load-path "lisp/csv-mode"
  :mode "\\.[Cc][Ss][Vv]\\'"
  :config
  (setq csv-separators '("," ";" "|" " ")))

;; navigate and edit CSV files
(use-package csv-nav
  :load-path "lisp/csv-nav"
  :after csv-mode)

;; nov-mode ;; nov.el for epub
(use-package nov-mode
  :ensure nov
  :mode "\\.epub\\'")

;; major mode for editing PHP code
(use-package php-mode
  :ensure t
  :mode (("\\.module$" . php-mode)
	 ("\\.inc$" . php-mode)
	 ("\\.install$" . php-mode)
	 ("\\.engine$" . php-mode)))

;; textile markup editing major mode
(use-package textile-mode
  :ensure t
  :mode "\\.textile\\'")

;; YAML support
(use-package yaml-mode
  :ensure t
  :mode
  (("\\.yml$" . yaml-mode)
   ("\\.yaml$" . yaml-mode)))

;; JSON
;; (use-package json-mode
;;   :ensure t
;;   :mode (("\\.json\\'" . json-mode)
;;	 ("\\manifest.webapp\\'" . json-mode )
;;       ("\\.tern-project\\'" . json-mode)))

;; javascript
(use-package js2-mode
  :ensure t
  :mode (("\\.js\\'" . js2-mode)
	 ("\\.json\\'" . js2-mode)
	 ("\\manifest.webapp\\'" . js2-mode )
	 ("\\.tern-project\\'" . js2-mode)))

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

(use-package rainbow-mode
  :ensure t
  :pin gnu
  :hook css-mode)

(provide 'init-misc-filetypes)
;;; init-misc-filetypes.el ends here
