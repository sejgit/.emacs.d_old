;;; init.el ---  Stephen's emacs init file
;;; Commentary:
;; my core Emacs file over time

;;; Changelog
;; 2016-02-24 init
;; 2016 03 17 good ideas from aaron bedra's Emacs configuration
;; 2016 11 29 integrate win-nt version & virtualbox
;; 2016 11 30 cleanup & concat with linux versions
;; 2016 12 12 transfer updates from test-version
;; 2016 12 15 updates due to win move to wsys2/ming64
;; 2017 01 06 bring together init & real-init
;; 2017 01 10 change setup of initial frame size and position
;; 2017 01 18 changes to allow setup to work on nox versions
;; 2017 01 19 add gc-cons garbage collection limit
;; 2017 03 31 black background to tango dark theme
;; 2017 04 04 remove recompile at kill in favour of auto-compile package
;; 2017 05 08 add code to move custom-sets/faces to custom.el
;; 2017 05 12 additions from purcell-emacs.d
;; 2017 08 22 additions from EOS Emacs operating System by Lee Hinman
;; 2017 09 11 package load re-introduced
;; 2017 09 21 reordering per ideas from magnars
;; 2017 11 29 package load removed with new use-package understanding
;; 2018 01 02 limited initial package install put in
;; 2018 06 26 add quelpa quelpa-use-package to first installed packages
;; 2018 07 03 add require for above
;; 2018 08 02 moved around todos & loaddir & others
;; 2018 08 13 clean-up some vars
;; 2018 08 15 adds for windows set-up

;;; Code:

;; debugger on
(setq debug-on-error t)
(setq debug-on-quit t)

(defvar emacs-start-time (current-time)
  "Time Emacs was started.")

;; Turn off mouse interface early in startup to avoid momentary display
(if (fboundp 'menu-bar-mode) (menu-bar-mode t))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

;; No splash screen
(setq inhibit-startup-message t)

;; reduce garbage collection
(setq gc-cons-threshold (* 50 1024 1024))

;; wrap init in this to reduce file access times
(let ((file-name-handler-alist nil))

  ;; turn on syntax highlightng for all buffers
  (global-font-lock-mode t)

  ;; raise the maximum number of logs in the *Messages* buffer
  (setq message-log-max 16384)

  ;; wait a bit longer than the default 0.5s before assuming Emacs is idle
  (setq idle-update-delay 2)

  ;; make gnutls a bit safer
  (setq gnutls-min-prime-bits 4096)

  ;; remove irritating 'got redefined' messages
  (setq ad-redefinition-action 'accept)

  ;; place to hold specific & secret stuff
  (let ((secret.el (expand-file-name "secret.el" "~/.ssh/")))
    (when (file-exists-p secret.el)
      (load secret.el)))

  ;; set-up the init-directory
  (defvar init-dir)
  (setq init-dir
	(expand-file-name "init.d" user-emacs-directory))
  (add-to-list 'load-path init-dir)
  (add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

  (setq custom-file (expand-file-name "init-custom.el" init-dir))
  (load custom-file 'noerror)

  (set-language-environment 'utf-8)
  (setq locale-coding-system 'utf-8)
  (set-default-coding-systems 'utf-8)
  (set-terminal-coding-system 'utf-8)
  (prefer-coding-system 'utf-8)

  (require 'package)
  (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/"))
  (add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/"))

  (setq load-prefer-newer t)

  ;; Fire up package.el
  (setq package-enable-at-startup nil)
  (package-initialize)

  ;; fetch the list of packages available
  (unless package-archive-contents
    (package-refresh-contents))

  ;; list the packages you want
  (defvar package-list
    '(delight diminish use-package load-dir quelpa quelpa-use-package))

  ;; install the missing packages
  (dolist (package package-list)
    (unless (package-installed-p package)
      (package-install package)))

  ;; set-up use-package
  (require 'delight)
  (require 'diminish)
  (require 'use-package)
  (require 'quelpa)
  (require 'quelpa-use-package)
  (require 'cl)
  (require 'bind-key)
  (require 'cl-lib)

  ;; Use latest Org
  (use-package org
    ;;:pin org
    :ensure org-plus-contrib)

  ;; Lame, server has bad autoloads
  (require 'server nil t)
  (use-package server
    :if window-system
    :functions server-running-p
    :init
    (when (not (server-running-p server-name))
      (server-start)))

  ;; set up edit-server
  (use-package edit-server
    :ensure t
    :config
    (edit-server-start))

  ;; Remove security vulnerability
  (eval-after-load "enriched"
    '(defun enriched-decode-display-prop (start end &optional param)
       (list start end)))

  ;; benchmarking see results with below two commands
  ;; benchmark-init/show-durations-tabulated
  ;; benchmark-init/show-durations-tree
  (use-package benchmark-init
    :ensure t
    :config
    ;; To disable collection of benchmark data after init is done.
    (add-hook 'after-init-hook 'benchmark-init/deactivate))

  (use-package cyberpunk-theme
    :functions load-cyberpunk-theme
    :ensure t
    :config
    (defun load-cyberpunk-theme (frame)
      "Load cyberpunk theme in current FRAME."
      (select-frame frame)
      (load-theme 'cyberpunk t))
    (load-cyberpunk-theme(selected-frame)))

  ;; so theme works with daemon server
  (if (daemonp)
      (add-hook 'after-make-frame-functions #'load-cyberpunk-theme)
    (load-theme 'cyberpunk t) )

  (switch-to-buffer "*dashboard*")

  ;; The EMACS environment variable being set to the binary path of emacs.
  (setenv "EMACS"
	  (file-truename (expand-file-name invocation-name invocation-directory)))

  ;; Works in the same way as os.path.join in python
  (defun sej/join-paths (root &rest dirs)
    (let ((result root))
      (cl-loop for dir in dirs do
	       (setq result (concat (file-name-as-directory result) dir)))
      result))

  ;; for future use
  (defvar sej/gpg-key)

  ;; The packages in this section provide no functionality on their own,
  ;; but provide support for writing custom elisp.
  (use-package s
    :ensure t
    :demand t)

  (use-package dash
    :ensure t
    :demand t
    :config
    (progn
      (dash-enable-font-lock)))

  (use-package gh
    :ensure t
    :demand t)

  (use-package request
    :ensure t
    :defer t)

  (defun eshell-parse-colon-path (path-env)
    "Split string with `parse-colon-path'.
Prepend remote identification of `default-directory', if any."
    (let ((remote (file-remote-p default-directory)))
      (if remote
	  (mapcar
	   (lambda (x) (concat remote x))
	   (parse-colon-path path-env))
	(parse-colon-path path-env))))

  (defun sej/download-to-buffer (uri)
    (interactive (list (read-string "Enter uri: ")))
    (require 'request)
    (request uri
	     :parser 'buffer-string
	     :success (cl-function
		       (lambda (&key data &allow-other-keys)
			 (let ((created-buffer (get-buffer-create uri)))
			   (with-current-buffer created-buffer
			     (insert data))
			   (switch-to-buffer created-buffer))))))

  (defun sej/get-executables-at-path (filepath)
    (when (and (file-exists-p filepath) (f-directory? filepath))
      (--filter (let ((fullpath (imalison:join-paths filepath it)))
		  (and (file-executable-p fullpath)
		       (not (f-directory? fullpath))))
		(directory-files filepath))))

  (defun sej/get-executables-on-path ()
    (mapcan 'sej/get-executables-at-path (eshell-parse-colon-path (getenv "PATH"))))

  (defun sej/edit-script ()
    (interactive)
    (find-file (executable-find
		(ido-completing-read "Select a script to edit: "
				     (sej/get-executables-on-path)))))

  ;; load files from init.d
  ;; check OS type
  (cond
   ((string-equal system-type "windows-nt") ; Microsoft Windows
    (progn
      (message "Microsoft Windows")
      ;;see if we can get some speed improvements
      (use-package auto-compile
	:ensure t
	:demand t
	:config
	(progn
	  (auto-compile-on-load-mode)
	  (auto-compile-on-save-mode)))

      ;; load AutoHotkey mode
      (load-library "xahk-mode")

      ;; load the init files we want in windows land
      (load (expand-file-name "init+bindings.el" init-dir))
      (load (expand-file-name "init+settings.el" init-dir))
      (load (expand-file-name "init-appearance.el" init-dir))
      ;;(load (expand-file-name "init-c.el" init-dir))
      (load (expand-file-name "init-completion.el" init-dir))
      ;;(load (expand-file-name "init-custom.el" init-dir))
      (load (expand-file-name "init-dashboard.el" init-dir))
      ;;(load (expand-file-name "init-deft.el" init-dir))
      (load (expand-file-name "init-dired.el" init-dir))
      ;;(load (expand-file-name "init-elfeed.el" init-dir))
      (load (expand-file-name "init-flycheck.el" init-dir))
      (load (expand-file-name "init-frame-cmds.el" init-dir))
      (load (expand-file-name "init-git.el" init-dir))
      (load (expand-file-name "init-ido-ivy-helm.el" init-dir))
      (load (expand-file-name "init-lisp.el" init-dir))
      ;;(load (expand-file-name "init-misc-defuns.el" init-dir))
      (load (expand-file-name "init-misc-filetypes.el" init-dir))
      (load (expand-file-name "init-misc-pkgs" init-dir))
      (load (expand-file-name "init-org.el" init-dir))
      (load (expand-file-name "init-projectile.el" init-dir))
      ;;(load (expand-file-name "init-python.el" init-dir))
      (load (expand-file-name "init-registers.el" init-dir))
      ;;(load (expand-file-name "init-shell.el" init-dir))
      (load (expand-file-name "init-spelling.el" init-dir))
      (load (expand-file-name "init-templates.el" init-dir))
      ;;(load (expand-file-name "init-tramp.el" init-dir))
      (load (expand-file-name "init-view.el" init-dir))
      (load (expand-file-name "init-writing.el" init-dir))
      ))
   ((string-equal system-type "darwin") ; Mac OS X
    (progn
      (message "Mac OS X")
      ;; load-dir init.d
      (use-package load-dir
	:ensure t
	:functions load-dir-one
	:config
	(random t)
	(setq force-load-messages t)
	(setq load-dir-debug nil)
	(setq load-dir-recursive nil)
	(load-dir-one init-dir))
      ))
   ((string-equal system-type "gnu/linux") ; linux
    (progn
      (message "Linux")
      ;; load-dir init.d
      (use-package load-dir
	:ensure t
	:functions load-dir-one
	:config
	(random t)
	(setq force-load-messages t)
	(setq load-dir-debug nil)
	(setq load-dir-recursive nil)
	(load-dir-one init-dir))
      )))


  ;; save histories
  (use-package savehist
    :ensure nil
    :config
    (setq savehist-file (concat user-emacs-directory "savehist"))
    (savehist-mode 1)
    (setq savehist-save-minibuffer-history 1)
    (setq savehist-additional-variables
	  '(kill-ring
	    search-ring
	    regexp-search-ring))
    (setq-default save-place t)    )

  (use-package uptimes
    :ensure t)

  (setq debug-on-error nil)
  (setq debug-on-quit nil)

  ) ;; end of let file wrapper

(provide 'init)
;;; init.el ends here



;;  TODO: better docs for what needs installed on base computer ag pass projectile common-tools?(gls)

;; TODO: make settings generic to individual like user-full-name etc (see below for details)

;; Need different settings for different machines?
;; ;; Settings for currently logged in user
;; (setq user-settings-dir
;;       (concat user-emacs-directory "users/" user-login-name))

;; ;; Conclude init by setting up specifics for the current user
;; (when (file-exists-p user-settings-dir)
;;   (mapc 'load (directory-files user-settings-dir nil "^[^#].*el$")))
;; These are the last lines of my init.el. They will load any *.el files in the ~/.emacs.d/users/user-login-name/ folder.

;; Anything specific for that machine goes there.
