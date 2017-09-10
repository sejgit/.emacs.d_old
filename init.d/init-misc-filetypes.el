;;; init-misc-filetypes.el --- settings for miscellaneous filetypes

;;; Commentary:
;; Lots of filetype modes not deserving of their own file so far.

;;; ChangeLog:
;; 2017 09 07 init SeJ moved simple modes from init-misc.pkgs & others to its own file

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

;;; Code:

;; arduino-mode
(use-package arduino-mode
  :mode "\\.ino$")

(use-package batch-mode
  :mode "\\.bat\\'")

;; major mode for editing conf/ini/properties files
(use-package conf-mode
  :ensure t
  :defer t
  :diminish conf-mode
  :mode "\\.gitconfig$")


;; editing of crontab scheduling files
(use-package crontab-mode
  :ensure t
  :defer t
  :mode "\\.?cron\\(tab\\)?\\'")

;; major mode for csv
(use-package csv-mode
  :ensure t
  :defer t
  :mode "\\.[Cc][Ss][Vv]\\'"
  :config
  (setq csv-separators '("," ";" "|" " ")))

;; navigate and edit CSV files
(use-package csv-nav
  :ensure t
  :defer t)

;; nov-mode ;; nov.el for epub
(use-package nov-mode
  :ensure nov
  :defer t
  :mode "\\.epub\\'")

;; major mode for editing PHP code
(use-package php-mode
  :ensure t
  :defer t
  :config
  (use-package smarty-mode))

;; textile markup editing major mode
(use-package textile-mode
  :ensure t
  :defer t
  :mode "\\.textile\\'")

;; YAML support
(use-package yaml-mode
  :defer t
  :ensure t
  :mode
  "\\.yml$"
  "\\.yaml$"
  )

(provide 'init-misc-filetypes)
;;; init-misc-filetypes.el ends here









