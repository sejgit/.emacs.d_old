;;; init-templates.el --- templates for auto-insertion

;;; Commentary:
;;to be auto inserted in new files

;;; Log
;; 2017 05 17 init SeJ

;;; Code:

(use-package autoinsert
  :init
  (setq auto-insert-directory "~/.emacs.d/templates/")
  (setq auto-insert-query nil)
  (add-hook 'find-file-hook 'auto-insert)
  (auto-insert-mode 1)
  :config
  (define-auto-insert ".*\\.py[3]?$" "template.py")
  (define-auto-insert ".*\\.el" "template.el")
  )

(provide 'templates)
;;; init-templates.el ends here
