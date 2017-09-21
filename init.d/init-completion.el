;;; init-completion.el --- Completion configuration

;;; Commentary:
;; settings for company-mode in my Emacs

;; ChangeLog:
;; 2016 12 16 SeJ
;; 2017 01 07 switch from req-package to use-package
;; 2017 04 04 remove ensure went global ; defer not required for mode,bind,int
;; 2017 08 29 remove yasnippet
;; 2017 08 30 change binding to sej-map
;; 2017 09 03 rename to init-completion.el update company settings with EOS
;; 2017 09 20 move hippie-expand settings from init-bindings-settings.el

;;; Code:

(setq hippie-expand-try-functions-list
      '(try-complete-file-name-partially
	try-expand-dabbrev
	try-expand-dabbrev-all-buffers
	try-expand-dabbrev-from-kill
	try-complete-file-name-partially
	try-complete-file-name
	try-expand-all-abbrevs
	try-expand-list
	try-expand-line
	try-expand-line-all-buffers
	try-complete-lisp-symbol-partially
	try-compelete-lisp-symbol))

(use-package company
  :ensure t
  :defer 10
  :diminish company-mode
  :defines sej-mode-map
  :bind
  (:map sej-mode-map
        ("C-<tab>" . company-complete)
        ("M-<tab>" . company-complete))
  :init
  (add-hook 'after-init-hook #'global-company-mode)
  (use-package company-quickhelp
    :ensure t
    :init (add-hook 'company-mode-hook #'company-quickhelp-mode)
    :config (setq company-quickhelp-delay 2))
  ;; Set up statistics for company completions
  (use-package company-statistics
    :ensure t
    :demand
    :init (add-hook 'after-init-hook #'company-statistics-mode))
  (use-package company-jedi
    :ensure t
    :defer 10
    :config (add-to-list 'company-backends 'company-jedi))
  :config (setq global-company-mode t
                ;; do or don't automatically start completion after <idle time>
                company-idle-delay 0.1
                company-show-numbers t
                ;; at least 3 letters need to be there though
                company-minimum-prefix-length 3
                company-auto-complete nil
                company-selection-wrap-around t
                ;; show completion numbers for hotkeys
                company-show-numbers t
                ;; align annotations to the right
                company-tooltip-align-annotations t
                company-search-regexp-function #'company-search-flex-regexp)
  (bind-keys :map company-active-map
             ("C-n" . company-select-next)
             ("C-p" . company-select-previous)
             ("C-d" . company-show-doc-buffer)
             ("C-l" . company-show-location)
             ("<tab>" . company-complete)))

(use-package company-dabbrev
  :ensure nil
  :defines
  company-dabbrev-ignore-case
  company-dabbrev-downcase
  :init
  (setq company-dabbrev-ignore-case nil
        ;; don't downcase dabbrev suggestions
        company-dabbrev-downcase nil))

(use-package company-dabbrev-code
  :ensure nil
  :defines
  company-dabbrev-code-modes
  company-dabbrev-code-ignore-case
  :init
  (setq company-dabbrev-code-modes t
        company-dabbrev-code-ignore-case nil))

(use-package smart-tab
  :ensure t
  :defer t
  :diminish ""
  :defines
  smart-tab-using-hippie-expand
  :init
  (setq smart-tab-using-hippie-expand t)
  :config
  (global-smart-tab-mode 1)
  (add-to-list 'smart-tab-disabled-major-modes 'mu4e-compose-mode)
  (add-to-list 'smart-tab-disabled-major-modes 'erc-mode)
  (add-to-list 'smart-tab-disabled-major-modes 'shell-mode))

(provide 'init-completion)
;;; init-completion.el ends here




