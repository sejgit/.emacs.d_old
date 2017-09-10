;;; init-flycheck.el ---  Stephen's emacs init-flycheck.el

;;; Commentary:
;; flycheck & flymake settings for Emacs

;;; ChangeLog
;; 2016 12 16
;; 2017 01 06 change from req-package to use-package
;; 2017 04 04 remove ensure went global ; defer not required for mode,bind,int
;; 2017 08 24 add flymake to this file
;; 2017 08 28 add some flycheck settings & helm-flycheck
;; 2017 08 29 map to sej-mode-hook
;; 2017 80 30 update some binds

;;; Code:

(use-package flymake
  :ensure t
  :defer t
  :defines sej-mode-map
  :bind
  (:map sej-mode-map
	("H-[" . flymake-goto-prev-error)
	("H-]" . flymake-goto-next-error))
  :config
  (defun flymake-error-at-point ()
    "Show the flymake error in the minibuffer when point is on an invalid line."
    (when (get-char-property (point) 'flymake-overlay)
      (let ((help (get-char-property (point) 'help-echo)))
	(if help (message "%s" help)))))

  (add-hook 'post-command-hook 'flymake-error-at-point)

  (defun flymake-error-at-point ()
    "Show the flymake error in the minibuffer when point is on an invalid line."
    (when (get-char-property (point) 'flymake-overlay)
      (let ((help (get-char-property (point) 'help-echo)))
	(if help (message "%s" help)))))

  (add-hook 'post-command-hook 'flymake-error-at-point))


(use-package flycheck-color-mode-line
  :ensure t
  :defer 15)

(use-package flycheck
  :ensure t
  :defer 15
  :diminish flycheck-mode
  :defines sej-mode-map
  :bind
  (:map sej-mode-map
	("s-]" . flycheck-previous-error)
	("s-[" . flycheck-next-error)
	("C-c f" . flycheck-list-errors))
  :config
  (defadvice flycheck-next-error (before wh/flycheck-next-error-push-mark activate)
    (push-mark))
  (global-flycheck-mode 1)
  (add-hook 'flycheck-mode-hook 'flycheck-color-mode-line-mode)
  (setq flycheck-indication-mode 'right-fringe
        flycheck-check-syntax-automatically '(save mode-enabled))
  (custom-set-faces
   '(flycheck-error ((((class color)) (:underline "Red"))))
   '(flycheck-warning ((((class color)) (:underline "Orange")))))

  (setq flycheck-emacs-lisp-load-path 'inherit)
  (setq flycheck-python-flake8-executable "flake8")
  (setq flycheck-flake8-maximum-line-length 79)
  (setq flycheck-highlighting-mode 'lines)
  (progn 	  (set-face-attribute 'flycheck-warning nil
				      :inherit 'warning
				      :underline nil)
		  (set-face-attribute 'flycheck-error nil
				      :inherit 'error
				      :underline nil)))

(use-package flycheck-pos-tip
  :ensure t
  :defer 15
  :commands flycheck-pos-tip-error-messages
  :config
  (flycheck-pos-tip-mode)
  (setq flycheck-pos-tip-timeout 10
	flycheck-display-errors-delay 0.5)
  (setq flycheck-display-errors-function #'flycheck-pos-tip-error-messages))

(use-package helm-flycheck
  :ensure t
  :defer t
  :bind
  (:map sej-mode-map
	("C-c s h" . helm-flycheck)
	("s-h" . helm-flycheck)))

(provide 'init-flycheck)
;;; init-flycheck.el ends here


