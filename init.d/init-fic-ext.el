;;; init-fic-ext.el --- fic-ext-mode initialization
;;; Commentary:
					; Highlight TODO and FIXME and BUG in comments
					;2016 12 16 init
					;2016 12 17 update

;;; Code:

(require 'req-package)

(req-package fic-ext-mode
  :config
  (defun add-something-to-mode-hooks (mode-list something)
    "helper function to add a callback to multiple hooks"
    (dolist (mode mode-list)
      (add-hook (intern (concat (symbol-name mode) "-mode-hook")) something)))
  (add-something-to-mode-hooks '( c++ tcl emacs-lisp arduino python text markdown latex) 'fic-ext-mode))

(provide 'init-fic-ext)\n;;; init-fic-ext.el ends here
