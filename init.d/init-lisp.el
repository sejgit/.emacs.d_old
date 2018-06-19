;;; init-lisp.el --- elisp setups

;;; Commentary:
;; settings for elisp

;;; ChangeLog
;; 2017 08 25 init SeJ
;; 2017 08 28 added some paredit, eldoc, & ielm settings from EOS
;; 2017 08 30 clean up some comments & defers
;; 2017 09 20 move autocomplete from init-autocomplete.el & delete file
;;            move paredit defun from init-bindings-settings.el
;; 2017 11 18 swap paredit for smartparens
;; 2017 12 01 some mods for use-package & removal of autocomplete
;; 2018 04 04 adding abo-abo's lispy package for specifically better reading of LISP


;;; Code:

(define-key emacs-lisp-mode-map (kbd "C-c d") 'toggle-debug-on-error)

;; Paredit for editing within lisp
(use-package smartparens
  :ensure t
  :config
  (smartparens-global-mode t)  )

;; like rainbow-delimiters in elisp modes
(add-hook 'emacs-lisp-mode-hook 'rainbow-delimiters-mode)

;; we don't want this minor mode to be shown in the minibuffer, however
(use-package eldoc
  :ensure t
  :defer t
  :diminish
  eldoc-mode
  :hook  ;; we use eldoc to show the signature of the function at point in the minibuffer
  ((emacs-lisp-mode . eldoc-mode)
   (ielm-mode-hook . eldoc-mode)
   (lisp-interaction-mode-hook . eldoc-mode))
  :config
  (setq eldoc-idle-delay 0.1))

;; add a nice popup for ielm
(defun ielm-other-window ()
  "Run ielm on other window."
  (interactive)
  (switch-to-buffer-other-window
   (get-buffer-create "*ielm*"))
  (call-interactively 'ielm))

(define-key emacs-lisp-mode-map (kbd "H-i") 'ielm-other-window)
(define-key lisp-interaction-mode-map (kbd "H-i") 'ielm-other-window)

;; use flycheck in elisp
(add-hook 'emacs-lisp-mode-hook 'flycheck-mode)

;; enable dash for Emacs lisp highlighting
(eval-after-load "dash" '(dash-enable-font-lock))

;; turn on elisp-slime-nav if available so M-. works to jump to function definitions
(use-package elisp-slime-nav
  :ensure t
  :diminish elisp-slime-nav-mode
  :hook (emacs-lisp-mode . elisp-slime-nav-mode))

(use-package lispy
  :ensure t
  :defines sej-mode-map
  :bind (:map sej-mode-map
	      ("s-8" . lispy-multiline)
	      ("s-*" . lispy-oneline)))


(provide 'init-lisp)
;;; init-lisp.el ends here
