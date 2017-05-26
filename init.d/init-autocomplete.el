;;; init-autocomplete.el --- Initialize emacs autocomplete
;;; Commentary:
;; 2017 03 29 init SeJ
;; 2017 04 04 add defer
;; 2017 05 19 add ielm auto complete
;;; Code:


;; autocomplete
(use-package auto-complete
  :defer 2
  :config
  (ac-config-default)
  (global-auto-complete-mode t)
  )

(defun ielm-auto-complete ()
  "Enables `auto-complete' support in \\[ielm]."
  (setq ac-sources '(ac-source-functions
		     ac-source-variables
		     ac-source-features
		     ac-source-symbols
		     ac-source-words-in-same-mode-buffers))
  (add-to-list 'ac-modes 'inferior-emacs-lisp-mode)
  (auto-complete-mode 1))
(add-hook 'ielm-mode-hook 'ielm-auto-complete)


(provide 'init-autocomplete)
;;; init-autocomplete.el ends here
