;;; init-autocomplete.el --- Initialize emacs autocomplete
;;; Commentary:
;; 2017 03 29 init SeJ
;; 2017 04 04 add defer

;;; Code:


;; autocomplete
(use-package auto-complete
  :defer 2
  :config
  (ac-config-default)
  (global-auto-complete-mode t)
  )

(provide 'init-autocomplete)
;;; init-autocomplete.el ends here
