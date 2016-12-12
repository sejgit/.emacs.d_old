;; Stephen's emacs init file
;; 2016-02-24 init
;; 2016 03 17 good ideas from aaron bedra's emacs configuration
;; 2016 11 29 integrate win-nt version & virtualbox
;; 2016 11 30 cleanup & concat with linux versions
;; 2016 12 12 transfer updates from test-version

;; whoami
(setq user-full-name "Stephen Jenkins")
(setq user-mail-address "stephenearljenkins@gmail.com")

;; require common lisp
(require 'cl)

;; paths (generic, nt, then others

;; generic pathes
(setq load-path (append (list (expand-file-name "~/.emacs.d/lisp")) load-path))

(if (equal system-type 'windows-nt)
    (;; nt paths
     progn
      (setq mypaths
        ( list
	   "C:Program Files/Git/usr/bin"
	   "C:/Program Files/Git/bin/"
	   "C:/Program Files/Git/cmd/"
           "C:/Users/NZ891R/Google Drive/emacs/bin/"
           "C:/Users/NZ891R/Google Drive/emacs/Aspell/bin/"
           "C:/Users/NZ891R/Google Drive/emacs/"
           "C:/Users/NZ891R/Google Drive/"
           ) )

    (setenv "PATH" (concat (mapconcat 'identity mypaths ";") (getenv "PATH")))

    (setq exec-path (append mypaths (list "." exec-directory)) )
    (setq ispell-personal-dictionary "C:/Users/NZ891R/Google Drive/emacs/sej.ispell")

      ;; below has been commented out due to using shell level vars keeping 'just in case'
      ;;
      ;; (setq url-proxy-services
      ;; 	    '(("no_proxy" . "^\\(localhost\\|10.*\\)")
      ;; 	      ("http" . "naproxy.gm.com:80")
      ;; 	      ("https" . "naproxy.gm.com:80")))

      ;; (setq url-http-proxy-basic-auth-storage
      ;; 	    (list (list "naproxy.gm.com:80"
      ;; 			(cons "Input your LDAP UID !"
      ;; 			      (base64-encode-string "LOGIN:PASSWORD")))))

      ;; shell
      ;; (setq explicit-shell-file-name
      ;; 	    "C:/Program Files/Git/bin/bash.exe")
      ;; (setq shell-file-name "bash")
      ;; (setq explicit-bash-args '("--login" "-i"))
      )

  (;; non-nt path
   progn
   (setenv "PATH" (concat "/usr/local/bin:/opt/local/bin:/usr/bin:/bin:~/bin:" (getenv "PATH")))
   )
  )


;; set up package manager
(require 'package)
(load "package")
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/") t)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/") t)
(package-initialize)

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
                          web-mode
                          writegood-mode
                          yaml-mode
			  goto-chg
			  fic-ext-mode
			  saveplace
		       ido-ubiquitous
		       projectile
		       page-break-lines
		       dashboard
		       dired-avfs
		       rainbow-delimiters
		       dired-toggle-sudo
		       aggressive-indent
		       )
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
(if (display-graphic-p)
  (scroll-bar-mode -1)
  (tool-bar-mode -1)
  ;;(menu-bar-mode -1)
  )
(setq column-number-mode t)

;; marking text and clipboard settings
(delete-selection-mode t)
(transient-mark-mode t)
(setq x-select-enable-clipboard t)

;; empty line settings
(setq-default indicate-empty-lines nil)
(when (not indicate-empty-lines)
  (toggle-indicate-empty-lines))

;; indentation
(setq tab-width 2
      indent-tabs-mode nil)

;; yes and no settings
(defalias 'yes-or-no-p 'y-or-n-p)

;; echo keystrokes ; no dialog boxes ; visable bell ; highlight parens
(setq echo-keystrokes 0.1
      use-dialog-box nil
      visible-bell t)
(show-paren-mode t)

;;load files in vendor area which are independent packages
(defvar sej/vendor-dir (expand-file-name "vendor" user-emacs-directory))
(add-to-list 'load-path sej/vendor-dir)

(dolist (project (directory-files sej/vendor-dir t "\\w+"))
  (when (file-directory-p project)
    (add-to-list 'load-path project)))

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
   (setq deft-directory "C:/Users/NZ891R/Google Drive/todo")
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

;; autopair-mode
;;(require 'autopair)
;; below is supposedly superior
(electric-pair-mode t)

;; Add proper word wrapping
(global-visual-line-mode t)

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

(setq backup-by-copying t      ; don't clobber symlinks
      backup-directory-alist
      '(("." . ".saves"))    ; don't litter my fs tree
      delete-old-versions t
      kept-new-versions 6
      kept-old-versions 2
      version-control t)       ; use versioned backups

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
(if (display-graphic-p)
    ;;(load-theme 'wombat t)
    ;;(load-theme 'atom-dark t)
    (load-theme 'tango-dark t)
  )

;; set frame size and position
(defun set-frame-size-according-to-resolution ()
  (interactive)
  (if window-system
  (progn
    ;; use 120 char wide window for largeish displays
    ;; and smaller 80 column windows for smaller displays
    ;; pick whatever numbers make sense for you
    (if (> (x-display-pixel-width) 1280)
	(add-to-list 'default-frame-alist (cons 'width 100))
      ;;(add-to-list 'default-frame-alist (cons 'width 80))
	)
    ;; for the height, subtract a couple hundred pixels
    ;; from the screen height (for panels, menubars and
    ;; whatnot), then divide by the height of a char to
    ;; get the height we want
    (add-to-list 'default-frame-alist
         (cons 'height (/ (- (x-display-pixel-height) 110)
			  (frame-char-height))))
    (modify-frame-parameters
  nil '((user-position . t) (left . (- +160))))
    (modify-frame-parameters
  nil '((user-position . t) (top . (+ +20))))

    )))
(set-frame-size-according-to-resolution)

;; color codes
(require 'ansi-color)
(defun colorize-compilation-buffer ()
  (toggle-read-only)
  (ansi-color-apply-on-region (point-min) (point-max))
  (toggle-read-only))
(add-hook 'compilation-filter-hook 'colorize-compilation-buffer)

;; project mode
(require 'projectile)
(projectile-mode t)

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

;; dired-avfs
;; dependant on dired-hacks-utils dash

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

;; el-get
(add-to-list 'load-path "~/.emacs.d/el-get/el-get")

(unless (require 'el-get nil 'noerror)
  (with-current-buffer
      (url-retrieve-synchronously
       "https://raw.githubusercontent.com/dimitri/el-get/master/el-get-install.el")
    (goto-char (point-max))
    (eval-print-last-sexp)))

(add-to-list 'el-get-recipe-path "~/.emacs.d/el-get-user/recipes")
(el-get 'sync)




(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" default))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
