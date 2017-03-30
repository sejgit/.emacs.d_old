;;; init-autocomplete.el --- Initialize emacs autocomplete
;;; Commentary:
;; 2017 03 29 init SeJ


;;; Code:


;; autocomplete
(use-package auto-complete
  :ensure t
  :config
  (ac-config-default)
  (global-auto-complete-mode t)
  )

(provide 'init-autocomplete)
;;; init-autocomplete.el ends here
