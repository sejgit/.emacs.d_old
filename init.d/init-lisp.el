;;; init-lisp.el --- elisp setups

;;; Commentary:
;; settings for elisp

;;; ChangeLog
;; 2017 08 25 init SeJ
;; 2017 08 28 added some paredit settings & parinfer to try

;;; Code:


;; toggle-debug-on-error
(define-key emacs-lisp-mode-map (kbd "C-c d") 'toggle-debug-on-error)
;; macrostep allows us to incrementally expand the macros in our elisp file
(define-key emacs-lisp-mode-map (kbd "C-c d") 'toggle-debug-on-error)

;; Paredit for editing within lisp
;; (use-package paredit
;;   :config
;;   (add-hook 'emacs-lisp-mode-hook
;;	    (lambda () (paredit-mode 1)))
;;   (when (eq system-type 'darwin)
;;     ;; C-left
;;     (define-key paredit-mode-map (kbd "M-[")
;;       'paredit-forward-barf-sexp)
;;     ;; C-right
;;     (define-key paredit-mode-map (kbd "M-]")
;;       'paredit-forward-slurp-sexp)
;;     ;; ESC-C-left
;;     (define-key paredit-mode-map (kbd "ESC M-[")
;;       'paredit-backward-slurp-sexp)
;;     ;; ESC-C-right
;;     (define-key paredit-mode-map (kbd "ESC M-]")
;;       'paredit-backward-barf-sexp)))

(use-package parinfer
  :ensure t
  :bind
  (("C-," . parinfer-toggle-mode)
   :map parinfer-mode-map
   ;; C-left
   ("M-[" . paredit-forward-barf-sexp)
   ;; C-right
   ("M-]" . paredit-forward-slurp-sexp)
   ;; ESC-C-left
   ("ESC M-[" . paredit-backward-slurp-sexp)
   ;; ESC-C-right
   ("ESC M-]" . paredit-backward-barf-sexp))
  :defines
  parinfer-extensions
  :config
  (setq parinfer-extensions
	'(defaults       ; should be included.
	   pretty-parens  ; different paren styles for different modes.
	   ;; evil           ; If you use Evil.
	   ;; lispy          ; If you use Lispy. With this extension, you should install Lispy and do not enable lispy-mode directly.
	   paredit        ; Introduce some paredit commands.
	   smart-tab      ; C-b & C-f jump positions and smart shift with tab & S-tab.
	   smart-yank))   ; Yank behavior depend on mode.
  (add-hook 'clojure-mode-hook #'parinfer-mode)
  (add-hook 'emacs-lisp-mode-hook #'parinfer-mode)
  (add-hook 'common-lisp-mode-hook #'parinfer-mode)
  (add-hook 'scheme-mode-hook #'parinfer-mode)
  (add-hook 'lisp-mode-hook #'parinfer-mode))


;; like rainbow-delimiters in elisp modes
(add-hook 'emacs-lisp-mode-hook 'rainbow-delimiters-mode)

;; we don't want this minor mode to be shown in the minibuffer, however
(use-package eldoc
  :config
  (diminish 'eldoc-mode)
  ;; we use eldoc to show the signature of the function at point in the minibuffer
  (add-hook 'emacs-lisp-mode-hook 'eldoc-mode))

;; use flycheck in elisp
(add-hook 'emacs-lisp-mode-hook 'flycheck-mode)

;; enable dash for Emacs lisp highlighting
(eval-after-load "dash" '(dash-enable-font-lock))


(provide 'init-lisp)
;;; init-lisp.el ends here
