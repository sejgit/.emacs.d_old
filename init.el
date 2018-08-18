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

(let ((secret.el (expand-file-name "secret.el" "~/.ssh/")))
  (when (file-exists-p secret.el)
    (load secret.el)))

;; directories for windows setup
(cond
 ((string-equal system-type "windows-nt") ; running on windows
  (progn
    (defvar emax-root (concat (expand-file-name "~") "/emax"))
    (defvar emax-bin (concat emax-root "/bin"))
    (defvar emax-bin64 (concat emax-root "/bin64"))
    (defvar emax-mingw64 (concat emax-root "/mingw64/bin"))
    (defvar emax-lisp (concat emax-root "/lisp"))

    (setq exec-path (cons emax-bin exec-path))
    (setenv "PATH" (concat emax-bin ";" (getenv "PATH")))

    (setq exec-path (cons emax-bin64 exec-path))
    (setenv "PATH" (concat emax-bin64 ";" (getenv "PATH")))

    (setq exec-path (cons emax-mingw64 exec-path))
    (setenv "PATH" (concat emax-mingw64 ";" (getenv "PATH")))

    (setenv "PATH" (concat "C:\\msys64\\usr\\bin;C:\\msys64\\mingw64\\bin;" (getenv "PATH")))

    (dolist (dir '("~/emax/" "~/emax/bin/" "~/emax/bin64/" "~/emax/mingw64/bin/" "~/emax/lisp/"))
      (add-to-list 'load-path dir))
    ;;(setq user-emacs-directory "~/emax")
    ;;(setq init-dir "~/emax")
    )))

(defvar init-dir)
(setq init-dir
      (expand-file-name "init.d" user-emacs-directory))

(let ((minver "24.1"))
  (when (version<= emacs-version minver)
    (error "Your Emacs is too old -- this config requires v%s or higher" minver)))
(when (version<= emacs-version "24.4")
  (message "Your Emacs is old, and some functionality in this config will be disabled. Please upgrade if possible."))

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

  (add-to-list 'load-path init-dir)
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

  (let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                      (not (gnutls-available-p))))
	 (proto (if no-ssl "http" "https")))
    ;; Comment/uncomment these two lines to enable/disable MELPA and MELPA Stable as desired
    (add-to-list 'package-archives (cons "melpa" (concat proto "://melpa.org/packages/")) t)
    ;;(add-to-list 'package-archives (cons "melpa-stable" (concat proto "://stable.melpa.org/packages/")) t)
    (add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/"))
    (when (< emacs-major-version 24)
      ;; For important compatibility libraries like cl-lib
      (add-to-list 'package-archives '("gnu" . (concat proto "://elpa.gnu.org/packages/")))))


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
    :init
    (when (not (server-running-p server-name))
      (server-start)))

  ;; Remove security vulnerability
  (eval-after-load "enriched"
    '(defun enriched-decode-display-prop (start end &optional param)
       (list start end)))

  (use-package cyberpunk-theme
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

  ;; load files from init.d
  ;; check OS type
  (cond
   ((string-equal system-type "windows-nt") ; Microsoft Windows
    (progn
      (message "Microsoft Windows")
      ;; Tangle configuration
      ;; (org-babel-load-file (expand-file-name "~/emax/emax.org" user-emacs-directory))
      ;;(garbage-collect))

      (load-library "xahk-mode")
      ;;(load (expand-file-name "init+bindings.el" init-dir))
      ;;(load (expand-file-name "init+settings.el" init-dir))
      ;;(load (expand-file-name "init-appearance.el" init-dir))    
      ))
   ((string-equal system-type "darwin") ; Mac OS X
    (progn
      (message "Mac OS X")
      ))
   ((string-equal system-type "gnu/linux") ; linux
    (progn
      (message "Linux")
      )))

  ;; load-dir init.d
  (use-package load-dir
    :ensure t
    :config
    (random t)
    (setq force-load-messages t)
    (setq load-dir-debug nil)
    (setq load-dir-recursive nil)
    (load-dir-one init-dir))

  
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
