;;; init-fic-ext.el --- fic-ext-mode initialization
;;; Commentary:
;; Highlight TODO and FIXME and BUG in comments

;;; ChangeLog:
;; 2016 12 16 init
;; 2016 12 17 update
;; 2017 01 06 change from req-package to use-package
;; 2017 04 04 remove ensure went global ; defer not required for mode,bind,int
;; 2017 08 29 diminish fic-ext-mode
;; 2017 09 11 change to fic-mode
;; 2017 12 01 update for use-package


;;; Code:

(use-package fic-mode
  :diminish fic-mode
  :functions add-something-to-mode-hooks
  :init
  (defun add-something-to-mode-hooks (mode-list something)
    "helper function to add a callback to multiple hooks"
    (dolist (mode mode-list)
      (add-hook (intern (concat (symbol-name mode) "-mode-hook")) something)))
  (add-something-to-mode-hooks '( c++ tcl emacs-lisp arduino python text markdown latex) 'fic-mode))

(provide 'init-fic-ext)
;;; init-fic-ext.el ends here
