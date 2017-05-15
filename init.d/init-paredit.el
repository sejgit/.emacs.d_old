;;; init-paredit.el --- Stephen's paredit settings
;;; Commentary:
;; Paredit helps keep parentheses balanced and adds many keys for moving S-expressions and moving around in S-expressions for Lisp

;; 2017 05 15 init SeJ

;;; Code:

(use-package paredit
  :defer 5
  :config
  (autoload 'enable-paredit-mode "paredit"
    "Turn on pseudo-structural editing of Lisp code."
    t)
  (add-hook 'emacs-lisp-mode-hook       'enable-paredit-mode)
  (add-hook 'lisp-mode-hook             'enable-paredit-mode)
  (add-hook 'lisp-interaction-mode-hook 'enable-paredit-mode)
  (add-hook 'scheme-mode-hook           'enable-paredit-mode)
  )

(use-package paredit-everywhere
  :config
  (add-hook 'prog-mode-hook 'paredit-everywhere-mode))

(provide 'init-paredit)
;;; init-paredit.el ends here
