;;; init-fic-ext.el --- fic-ext-mode initialization
;;; Commentary:
;; Highlight TODO and FIXME and BUG in comments
;;2016 12 16 init
;;2016 12 17 update
;; 2017 01 06 change from req-package to use-package
;; 2017 04 04 remove ensure went global ; defer not required for mode,bind,int

;;; Code:

(use-package fic-ext-mode
  :defer 2
  :config
  (defun add-something-to-mode-hooks (mode-list something)
    "helper function to add a callback to multiple hooks"
    (dolist (mode mode-list)
      (add-hook (intern (concat (symbol-name mode) "-mode-hook")) something)))
  (add-something-to-mode-hooks '( c++ tcl emacs-lisp arduino python text markdown latex) 'fic-ext-mode))

(provide 'init-fic-ext)
;;; init-fic-ext.el ends here
