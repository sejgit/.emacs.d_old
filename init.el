;; Stephen's emacs init file
;; 2016-02-24 init
;; 2016 03 17 good ideas from aaron bedra's emacs configuration

;; whoami
(setq user-full-name "Stephen Jenkins")
(setq user-mail-address "stephenearljenkins@gmail.com")

;; paths
(setq load-path (append (list (expand-file-name "~/.emacs.d/lisp")) load-path))
(setenv "PATH" (concat "/usr/local/bin:/opt/local/bin:/usr/bin:/bin:/home/pi/bin" (getenv "PATH")))

;; require common lisp
(require 'cl)

;; proxy
;;(setq url-proxy-services (quote (("http" . "naproxy.gm.com:80"))))

;; nt paths
(when (string-equal system-type "windows-nt")
  (let (
        (mypaths
         '(

           "C:/Users/NZ891R/Google Drive/emacs-24.3-bin-i386/emacs-24.3/bin/"
           "C:/Users/NZ891R/Google Drive/emacs-24.3-bin-i386/emacs-24.3/"
           "C:/Users/NZ891R/Google Drive/emacs-24.3-bin-i386/"
           "C:/Users/NZ891R/Google Drive/"
           ) )
        )

    (setenv "PATH" (mapconcat 'identity mypaths ";") )

    (setq exec-path (append mypaths (list "." exec-directory)) )
    ) )

;; set up package manager
(load "package")
(package-initialize)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)

(setq package-archive-enable-alist '(("melpa" deft magit)))

(defvar sej/packages '(ac-slime
		          arduino-mode
                          auto-complete
                          autopair
                          clojure-mode
                          coffee-mode
                          csharp-mode
                          deft
                          flycheck
                          gist
                          magit
                          markdown-mode
                          marmalade
                          org
                          paredit
                          restclient
                          smex
                          solarized-theme
                          web-mode
                          writegood-mode
                          yaml-mode)
  "Default packages")

;; make sure all above packages are installed if not have ELPA take cate of it
(defun sej/packages-installed-p ()
  (loop for pkg in sej/packages
        when (not (package-installed-p pkg)) do (return nil)
        finally (return t)))

(unless (sej/packages-installed-p)
  (message "%s" "Refreshing package database...")
  (package-refresh-contents)
  (dolist (pkg sej/packages)
    (when (not (package-installed-p pkg))
      (package-install pkg))))

;; keybindings
(global-set-key (kbd "RET") 'newline-and-indent)
(global-set-key (kbd "C-;") 'comment-or-uncomment-region)
(global-set-key (kbd "M-/") 'hippie-expand)
(global-set-key (kbd "C-+") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)
(global-set-key (kbd "C-c C-k") 'compile)
(global-set-key (kbd "C-x g") 'magit-status)

(global-set-key (kbd "M-3") 'delete-other-windows)
(global-set-key (kbd "M-4") 'split-window-vertically)
(global-set-key (kbd "M-2") 'delete-window)
(global-set-key (kbd "M-s") 'other-window)

(global-set-key (kbd "<f1>") 'org-mode)

;; some beginning settings

(scroll-bar-mode -1)
(tool-bar-mode -1)
;(menu-bar-mode -1)
(setq column-number-mode t)

;; marking text and clipboard settings
(delete-selection-mode t)
(transient-mark-mode t)
(setq x-select-enable-clipboard t)

;; empty line settings
(setq-default indicate-empty-lines t)
(when (not indicate-empty-lines)
  (toggle-indicate-empty-lines))

;; indentation
(setq tab-width 2
      indent-tabs-mode nil)

;; yes and no settings
(defalias 'yes-or-no-p 'y-or-n-p)

;; temporary fie management
(setq backup-directory-alist `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms `((".*" ,temporary-file-directory t)))

;; echo keystrokes ; no dialog boxes ; visable bell ; highlight parens
(setq echo-keystrokes 0.1
      use-dialog-box nil
      visible-bell t)
(show-paren-mode t)

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
(setq deft-directory "~/sf_gdrive/todo")
(setq deft-use-filename-as-title t)
(setq deft-extension "org")
(setq deft-text-mode 'org-mode)

;; smex
(setq smex-save-file (expand-file-name ".smex-items" user-emacs-directory))
(smex-initialize)
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)

;; Ido
(ido-mode t)
(setq ido-enable-flex-matching t
      ido-use-virtual-buffers t)

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
(defun flyspell-check-next-highlighed-word ()
  "Custom function to spell check next highlighted word"
  (interactive)
  (flyspell-goto-next-error)
  (ispell-word))
(global-set-key (kbd "M-<f8>") 'flyspell-check-next-highlighted-word)
(setq flyspell-issue-welcome-flag nil)
(setq-default ispell-list-command "list")

;; remember
(require 'remember)
(define-key global-map (kbd "<f9> r") 'remember)
(define-key global-map (kbd "<f9> R") 'remember-region)

;; autopair-mode
(require 'autopair)

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

(global-set-key (kbd "<f2>") 'bc-set)           ;; f2 for set bookmark
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
                   scheme-mode
                   clojure-mode))

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

;; conf-mode
(add-to-list 'auto-mode-alist '("\\.gitconfig$" . conf-mode))

;; yaml-mode
(add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))
(add-to-list 'auto-mode-alist '("\\.yaml$" . yaml-mode))

;; coffee script mode
(defun coffee-custom ()
  "coffee-mode-hook"
  (make-local-variable 'tab-width)
  (set 'tab-width 2))

(add-hook 'coffee-mode-hook 'coffee-custom)

;; markdown mode
(add-to-list 'auto-mode-alist '("\\.md$" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.mdown$" . markdown-mode))
(add-hook 'markdown-mode-hook
          (lambda ()
            (visual-line-mode t)
            (writegood-mode t)
            (flyspell-mode t)))
(setq markdown-command "pandoc --smart -f markdown -t html")

;; themes
(if window-system
    (load-theme 'solarized-light t)
  (load-theme 'wombat t))

;; color codes
(require 'ansi-color)
(defun colorize-compilation-buffer ()
  (toggle-read-only)
  (ansi-color-apply-on-region (point-min) (point-max))
  (toggle-read-only))
(add-hook 'compilation-filter-hook 'colorize-compilation-buffer)







