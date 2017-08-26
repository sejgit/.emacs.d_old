;;; init-lisp.el --- elisp setups

;;; Commentary:
;; settings for elisp

;;; Log
;; 2017 08 25 init SeJ

;;; Code:

;; toggle-debug-on-error
(define-key emacs-lisp-mode-map (kbd "C-c d") 'toggle-debug-on-error)
;; macrostep allows us to incrementally expand the macros in our elisp file
(define-key emacs-lisp-mode-map (kbd "C-c d") 'toggle-debug-on-error)

;; Paredit for editing within lisp
(add-hook 'emacs-lisp-mode-hook
	  (lambda () (paredit-mode 1)))



(provide 'init-lisp)
;;; init-lisp.el ends here
