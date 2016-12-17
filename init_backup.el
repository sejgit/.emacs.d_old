

;; spelling
(setq ispell-program-name "aspell")
(setq ispell-extra-args '("--sug-mode=ultra" "lang=en"))
(global-set-key (kbd "<f8>") 'ispell-word)
(global-set-key (kbd "C-<f8>") 'flyspell-mode)
(defun flyspell-check-next-highlighed-word ()
  "Custom function to spell check next highlighted word"
  (interactive)
  (flyspell-goto-next-error)
  (ispell-word))
(global-set-key (kbd "M-<f8>") 'flyspell-check-next-highlighted-word)
(setq flyspell-issue-welcome-flag nil)
(setq-default ispell-list-command "list")


;; save the place in files
(require 'saveplace)
(setq-default save-place t)

;; autocomplete
(require 'auto-complete-config)
(ac-config-default)

;; arduino-mode
(add-to-list 'auto-mode-alist ' ("\\.ino$" . arduino-mode))

;; conf-mode
(add-to-list 'auto-mode-alist '("\\.gitconfig$" . conf-mode))

;; yaml-mode
(add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))
(add-to-list 'auto-mode-alist '("\\.yaml$" . yaml-mode))

;; markdown mode
(add-to-list 'auto-mode-alist '("\\.md$" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.mdown$" . markdown-mode))
(add-hook 'markdown-mode-hook
          (lambda ()
            (visual-line-mode t)
            (writegood-mode t)
            (flyspell-mode t)))
(setq markdown-command "pandoc --smart -f markdown -t html")


;; color codes
(require 'ansi-color)
(defun colorize-compilation-buffer ()
  (toggle-read-only)
  (ansi-color-apply-on-region (point-min) (point-max))
  (toggle-read-only))
(add-hook 'compilation-filter-hook 'colorize-compilation-buffer)

;; dired-toggle-sudo
(require 'dired-toggle-sudo)
(define-key dired-mode-map (kbd "C-c C-s") 'dired-toggle-sudo)
(eval-after-load 'tramp
  '(progn
     ;; Allow to use: /sudo:user@host:/path/to/file
     (add-to-list 'tramp-default-proxies-alist
		  '(".*" "\\`.+\\'" "/ssh:%h:"))))
