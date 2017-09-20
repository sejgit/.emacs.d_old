;;; init-lisp.el --- elisp setups

;;; Commentary:
;; settings for elisp

;;; ChangeLog
;; 2017 08 25 init SeJ
;; 2017 08 28 added some paredit, eldoc, & ielm settings from EOS
;; 2017 08 30 clean up some comments & defers
;; 2017 09 20 move autocomplete from init-autocomplete.el & delete file
;;            move paredit defun from init-bindings-settings.el

;;; Code:

;; autocomplete
(use-package auto-complete
  :defer 2
  :config
  (ac-config-default)
  (global-auto-complete-mode t)
  )

(defun ielm-auto-complete ()
  "Enables `auto-complete' support in \\[ielm]."
  (setq ac-sources '(ac-source-functions
		     ac-source-variables
		     ac-source-features
		     ac-source-symbols
		     ac-source-words-in-same-mode-buffers))
  (add-to-list 'ac-modes 'inferior-emacs-lisp-mode)
  (auto-complete-mode 1))
(add-hook 'ielm-mode-hook 'ielm-auto-complete)

;; toggle-debug-on-error
(define-key emacs-lisp-mode-map (kbd "C-c d") 'toggle-debug-on-error)

;; Paredit for editing within lisp
(use-package paredit
  :ensure t
  :defer t
  :commands
  paredit-mode
  paredit-kill
  :diminish
  paredit-mode
  :config
  (add-hook 'emacs-lisp-mode-hook #'paredit-mode)
  (add-hook 'ielm-mode-hook #'paredit-mode)
  (when (eq system-type 'darwin)
    ;; C-left
    (define-key paredit-mode-map (kbd "M-[")
      'paredit-forward-barf-sexp)
    ;; C-right
    (define-key paredit-mode-map (kbd "M-]")
      'paredit-forward-slurp-sexp)
    ;; ESC-C-left
    (define-key paredit-mode-map (kbd "ESC M-[")
      'paredit-backward-slurp-sexp)
    ;; ESC-C-right
    (define-key paredit-mode-map (kbd "ESC M-]")
      'paredit-backward-barf-sexp))
  (defun paredit-duplicate-current-line ()
    "Paredit duplicate current line."
    (back-to-indentation)
    (let (kill-ring kill-ring-yank-pointer)
      (paredit-kill)
      (yank)
      (newline-and-indent)
      (yank)))  )

;; like rainbow-delimiters in elisp modes
(add-hook 'emacs-lisp-mode-hook 'rainbow-delimiters-mode)

;; we don't want this minor mode to be shown in the minibuffer, however
(use-package eldoc
  :ensure t
  :defer t
  :diminish
  eldoc-mode
  :config
  ;; we use eldoc to show the signature of the function at point in the minibuffer
  (add-hook 'emacs-lisp-mode-hook #'eldoc-mode)
  (add-hook 'ielm-mode-hook #'eldoc-mode)
  (add-hook 'lisp-interaction-mode-hook #'eldoc-mode)
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
  :defer t
  :diminish elisp-slime-nav-mode
  :config (add-hook 'emacs-lisp-mode-hook #'elisp-slime-nav-mode))

(provide 'init-lisp)
;;; init-lisp.el ends here
