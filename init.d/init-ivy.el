;;; init-ivy.el --- Stephen's emacs init-ivy.el


;;; Commentary:

;;; Logs:
;; 2017 05 13 init sej

;;; Code:

(use-package ivy
  :diminish
  ivy-mode
  :config
  (setq-default ivy-use-virtual-buffers t
		ivy-count-format ""
		projectile-completion-system 'ivy
		ivy-initial-inputs-alist
		'((man . "^")
		  (woman . "^")))
  ;; IDO-style directory navigation
  (define-key ivy-minibuffer-map (kbd "C-j") #'ivy-immediate-done)
  (define-key ivy-minibuffer-map (kbd "RET") #'ivy-alt-done)
  (defun sanityinc/enable-ivy-flx-matching ()
    "Make `ivy' matching work more like IDO."
    (interactive)
    (use-package flx)
    (setq-default ivy-re-builders-alist
		  '((t . ivy--regex-fuzzy))))

  (add-hook 'after-init-hook
	    (lambda ()
	      (when (bound-and-true-p ido-ubiquitous-mode)
		(ido-ubiquitous-mode -1))
	      (when (bound-and-true-p ido-mode)
		(ido-mode -1))
	      (ivy-mode 1))))




(use-package ivy-historian
  :config
  (lambda () (ivy-historian-mode t)))


(use-package counsel
  :config
  (setq-default counsel-mode-override-describe-bindings t)
  (counsel-mode)
  :diminish
  counsel-mode)

(provide 'init-ivy)
;;; init-ivy.el ends here


