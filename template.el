;; Stephen's emacs init-real.el file
;; 2016 12 16


;; org-mode settings
(global-set-key (kbd "<f1>") 'org-mode)
(setq org-log-done t
      org-todo-keywords '((sequence "TODO" "INPROGRESS" "DONE"))
      org-todo-keyword-faces '(("INPROGRESS" . (:foreground "blue" :weight bold))))
(add-hook 'org-mode-hook
          (lambda ()
            (flyspell-mode)))
(add-hook 'org-mode-hook
          (lambda ()
            (writegood-mode)))

;;deft
(if (string-equal system-type "windows-nt")
    (setq deft-directory "C:/Users/NZ891R/gdrive/todo")
  (setq deft-directory "~/gdrive/todo")
  )
(setq deft-use-filename-as-title t)
;;(setq deft-extension "org")
(setq deft-text-mode 'org-mode)
(setq deft-org-mode-title-prefix t)
(setq deft-recursive t)
(global-set-key [f7] 'deft)

;; org capture
(setq org-default-notes-file (concat deft-directory "/notes.org"))
     (define-key global-map "\C-cc" 'org-capture)
(setq org-capture-templates
      '(("t" "Todo" entry (file+headline (concat deft-directory "/gtd.org") "Tasks")
             "* TODO %?\n  %i\n  %a")
        ("j" "Journal" entry (file+datetree (concat deft-directory "/journal.org"))
	 "* %?\nEntered on %U\n  %i\n  %a")))

;; smex
(setq smex-save-file (expand-file-name ".smex-items" user-emacs-directory))
(smex-initialize)
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)

;; Ido
(ido-mode t)
(ido-everywhere t)
(setq ido-enable-flex-matching t
      ido-use-virtual-buffers t)
(require 'ido-ubiquitous)
(ido-ubiquitous-mode t)
(defalias 'list-buffers 'ibuffer)

;; bookmarks
(setq bookmark-default-file  (concat user-emacs-directory "bookmarks"))
(setq inhibit-splash-screen t)
(require 'bookmark)
(bookmark-bmenu-list)
(switch-to-buffer "*Bookmark List*")

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

;; electric-pair-mode
(electric-pair-mode t)

;; go to the last change
(require 'goto-chg)
(global-set-key [(control .)] 'goto-last-change)
;; M-. can conflict with etags tag search. But C-. can get overwritten
;; by flyspell-auto-correct-word. And goto-last-change needs a really fast key.
(global-set-key [(meta .)] 'goto-last-change)
;; ensure that even in worst case some goto-last-change is available
(global-set-key [(control meta .)] 'goto-last-change)
;; added reverse below
(global-set-key [(control ?,)] 'goto-last-change-reverse)

;; Highlight TODO and FIXME and BUG in comments
(require 'fic-ext-mode)
(defun add-something-to-mode-hooks (mode-list something)
  "helper function to add a callback to multiple hooks"
  (dolist (mode mode-list)
    (add-hook (intern (concat (symbol-name mode) "-mode-hook")) something)))
(add-something-to-mode-hooks '( c++ tcl emacs-lisp arduino python text markdown latex) 'fic-ext-mode)

;; save the place in files
(require 'saveplace)
(setq-default save-place t)

;; macro saving
(defun save-macro (name)
  "save a macro. take a name as argument and save the last defined macro under this name at the end of your init file"
  (interactive "SName of the macro :")
  (kmacro-name-last-macro name)
  (find-file user-init-file)
  (goto-char (point-max))
  (newline)
  (insert-kbd-macro name)
  (newline)
  (switch-to-buffer nil))

;;breadcrumb
(require 'breadcrumb)
;;(autoload 'bc-set               "breadcrumb" "Set bookmark in current point."   t)
;;(autoload 'bc-previous          "breadcrumb" "Go to previous bookmark."         t)
;;(autoload 'bc-next              "breadcrumb" "Go to next bookmark."             t)
;;(autoload 'bc-local-previous    "breadcrumb" "Go to previous local bookmark."   t)
;;(autoload 'bc-local-next        "breadcrumb" "Go to next local bookmark."       t)
;;(autoload 'bc-goto-current      "breadcrumb" "Go to the current bookmark."      t)
;;(autoload 'bc-list              "breadcrumb" "List all bookmarks in menu mode." t)
;;(autoload 'bc-clear             "breadcrumb" "Clear all bookmarks."             t)

;;  Examples below assign a set of keys to the breadcrumb bookmark functions.

(global-set-key (kbd "S-SPC") 'bc-set)           ;; shift space for set bookmark
(global-set-key (kbd "M-j") 'bc-previous)       ;; M-j for jump to previous
(global-set-key (kbd "M-J") 'bc-next)           ;; Shift-M-j for jump to next
(global-set-key (kbd "C-j") 'bc-local-previous) ;; M-up-arrow for local previous
(global-set-key (kbd "C-J") 'bc-local-next)     ;; M-down-arrow for local next
(global-set-key (kbd "C-c j") 'bc-goto-current) ;; C-c j for jump to current bookmark
(global-set-key (kbd "C-x M-j") 'bc-list)       ;; C-x M-j for the bookmark menu list

;; lisp-mode settings
(setq lisp-modes '(lisp-mode
                   emacs-lisp-mode
                   common-lisp-mode
                   scheme-mode)
      )

(defvar lisp-power-map (make-keymap))
(define-minor-mode lisp-power-mode "Fix keybindings; add power."
  :lighter " (power)"
  :keymap lisp-power-map
  (paredit-mode t))
(define-key lisp-power-map [delete] 'paredit-forward-delete)
(define-key lisp-power-map [backspace] 'paredit-backward-delete)

(defun abedra/engage-lisp-power ()
  (lisp-power-mode t))

(dolist (mode lisp-modes)
  (add-hook (intern (format "%s-hook" mode))
            #'abedra/engage-lisp-power))

(setq inferior-lisp-program "clisp")
(setq scheme-program-name "racket")

;; autocomplete
(require 'auto-complete-config)
(ac-config-default)

;; indentation and buffer cleanup
(defun untabify-buffer ()
  (interactive)
  (untabify (point-min) (point-max)))

(defun indent-buffer ()
  (interactive)
  (indent-region (point-min) (point-max)))

(defun cleanup-buffer ()
  "Perform a bunch of operations on the whitespace content of a buffer."
  (interactive)
  (indent-buffer)
  (untabify-buffer)
  (delete-trailing-whitespace))

(defun cleanup-region (beg end)
  "Remove tmux artifacts from region."
  (interactive "r")
  (dolist (re '("\\\\│\·*\n" "\W*│\·*"))
    (replace-regexp re "" nil beg end)))

(global-set-key (kbd "C-x M-t") 'cleanup-region)
(global-set-key (kbd "C-c n") 'cleanup-buffer)

(setq-default show-trailing-whitespace t)

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

;; dashboard
(require 'dashboard)
(dashboard-setup-startup-hook)
(setq dashboard-items '((recents . 5)
			(bookmarks . 5)
			(projects . 5)))

;; wind move built in package
(when (fboundp 'windmove-default-keybindings)
  (windmove-default-keybindings))
;; above not always wonking ; trying below
(global-set-key (kbd "C-c <left>")  'windmove-left)
(global-set-key (kbd "C-c <right>") 'windmove-right)
(global-set-key (kbd "C-c <up>")    'windmove-up)
(global-set-key (kbd "C-c <down>")  'windmove-down)

;; rainbow-delimiters-mode
(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)

;; dired-toggle-sudo
(require 'dired-toggle-sudo)
(define-key dired-mode-map (kbd "C-c C-s") 'dired-toggle-sudo)
(eval-after-load 'tramp
  '(progn
     ;; Allow to use: /sudo:user@host:/path/to/file
     (add-to-list 'tramp-default-proxies-alist
		  '(".*" "\\`.+\\'" "/ssh:%h:"))))

;; aggressive-indent-mode
(require 'aggressive-indent)
(add-hook 'emacs-lisp-mode-hook #'aggressive-indent-mode)
(add-hook 'css-mode-hook #'aggressive-indent-mode)
(global-aggressive-indent-mode 1)
(add-to-list 'aggressive-indent-excluded-modes 'html-mode)

