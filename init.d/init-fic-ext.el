;;; init-fic-ext.el --- fic-ext-mode initialization
;;; Commentary:
;; Highlight TODO and FIXME and BUG in comments
;;2016 12 16 init
;;2016 12 17 update
;; 2017 01 06 change from req-package to use-package

;;; Code:

(use-package fic-ext-mode
  :ensure t
  :config
  (defun add-something-to-mode-hooks (mode-list something)
    "helper function to add a callback to multiple hooks"
    (dolist (mode mode-list)
      (add-hook (intern (concat (symbol-name mode) "-mode-hook")) something)))
  (add-something-to-mode-hooks '( c++ tcl emacs-lisp arduino python text markdown latex) 'fic-ext-mode))

(provide 'init-fic-ext)
;;; init-fic-ext.el ends here
