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
;;            TODO make settings generic to individual
;;            TODO better docs for what needs installed on base computer

;;; Code:

;; debugger on
(setq debug-on-error t)
(setq debug-on-quit t)

(defvar emacs-start-time (current-time)
  "Time Emacs was started.")

;; define a const for mac behaviour
(defconst *is-a-mac* (eq system-type 'darwin))

;; Turn off mouse interface early in startup to avoid momentary display
(if (fboundp 'menu-bar-mode) (menu-bar-mode t))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

;; No splash screen
(setq inhibit-startup-message t)

;; reduce garbage collection
(setq gc-cons-threshold (* 50 1024 1024))

;; Bootstrap config
;; whoami
(setq user-full-name "Stephen Jenkins")
(setq user-mail-address "stephenearljenkins@gmail.com")

(defvar init-dir
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

  (require 'package)
  (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/"))
  ;;(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
  (add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/"))
  (setq load-prefer-newer t)

  ;; list the packages you want
  (defvar package-list
    '(use-package diminish cyberpunk-theme load-dir frame-cmds))

  ;; Fire up package.el
  (setq package-enable-at-startup nil)
  (package-initialize)

  ;; fetch the list of packages available
  (unless package-archive-contents
    (package-refresh-contents))

  ;; install the missing packages
  (dolist (package package-list)
    (unless (package-installed-p package)
      (package-install package)))
  
  (eval-when-compile
    (require 'use-package)
    (setq use-package-always-ensure t)
    )
  (require 'cl)
  (require 'diminish)
  (require 'bind-key)
  (require 'cl-lib)

  ;; Lame, server has bad autoloads :(
  (require 'server nil t)
  (use-package server
    ;;  :if window-system
    :init
    (when (not (server-running-p server-name))
      (server-start)))

  ;; Remove security vulnerability
  (eval-after-load "enriched"
    '(defun enriched-decode-display-prop (start end &optional param)
       (list start end)))

  ;; load preferred theme at startup
  (if (daemonp)
      (add-hook 'after-make-frame-functions #'load-cyberpunk-theme)
    (load-theme 'cyberpunk t) )

  ;; save histories
  (use-package savehist
    :config
    (setq savehist-file (concat user-emacs-directory "savehist"))
    (savehist-mode 1)
    (setq savehist-save-minibuffer-history 1)
    (setq savehist-additional-variables
	  '(kill-ring
	    search-ring
	    regexp-search-ring))
    (setq-default save-place t)    )

  ;; recompile configs
  (defconst my-init-dir "~/.emacs.d/init.d")
  (use-package auto-compile
    :ensure t
    :config
    (auto-compile-on-load-mode)
    (auto-compile-on-save-mode))

  ;; themes
  (use-package cyberpunk-theme
    :ensure t
    :config
    (defun load-cyberpunk-theme (frame)
      "Load cyberpunk theme in current FRAME."
      (select-frame frame)
      (load-theme 'cyberpunk t)
      ;;(set-frame-size-according-to-resolution)
      (switch-to-buffer "*dashboard*")))

  ;; init.d
  (random t)
  (use-package load-dir
    :ensure t
    :defines
    load-dir-debug
    load-dir-recursive
    :functions load-dir-one
    :init
    (setq force-load-messages t)
    (setq load-dir-debug nil)
    (setq load-dir-recursive nil)
    :config
    (load-dir-one my-init-dir))

  (declare-function sej-frame-resize-r (expand-file-name "init-frame-cmds" init-dir) nil)
  (when (display-graphic-p) (sej-frame-resize-r))

  ;; Prefer g-prefixed coreutils version of standard utilities when available
  (when (executable-find "gls") (setq insert-directory-program "gls"
				      dired-use-ls-dired t))

  (load-cyberpunk-theme(selected-frame))
  ;;(switch-to-buffer "*dashboard*")
  (use-package uptimes)

  (setq debug-on-error nil)
  (setq debug-on-quit nil)
  )

(provide 'init)
;;; init.el ends here


;;; ideas for later maybe

;; Need different settings for different machines?
;; ;; Settings for currently logged in user
;; (setq user-settings-dir
;;       (concat user-emacs-directory "users/" user-login-name))

;; ;; Conclude init by setting up specifics for the current user
;; (when (file-exists-p user-settings-dir)
;;   (mapc 'load (directory-files user-settings-dir nil "^[^#].*el$")))
;; These are the last lines of my init.el. They will load any *.el files in the ~/.emacs.d/users/user-login-name/ folder.

;; Anything specific for that machine goes there.

