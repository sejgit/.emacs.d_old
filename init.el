;;; init.el ---  Stephen's emacs init file
;;; Commentary:
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

;;; Code:

;; gc-cons garbage collection up the limit
(setq gc-cons-threshold 20000000)

;; whoami
(setq user-full-name "Stephen Jenkins")
(setq user-mail-address "stephenearljenkins@gmail.com")

;; package
(require 'package)
(setq package-enable-at-startup nil)

;; elpa
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/"))
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/"))
(add-to-list 'package-archives '("sunrise" . "http://joseito.republika.pl/sunrise-commander/"))
(setq load-prefer-newer t)
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package)
  (setq use-package-always-ensure t))
(require 'diminish)
(require 'bind-key)


;; recompile configs
(defconst my-init-dir "~/.emacs.d/init.d")
(use-package auto-compile
  :ensure t
  :init
  ;;(auto-compile-on-load-mode)
  (auto-compile-on-save-mode)
  )

;; themes
;; tango-dark
(defun load-tango-theme (frame)
  "Load tango-dark theme in current FRAME."
  (select-frame frame)
  (load-theme 'tango-dark t)
  ;;(set-frame-size-according-to-resolution)
  (set-background-color "black")
  (switch-to-buffer "*dashboard*"))

;; load preferred theme at startup
(if (daemonp)
    (add-hook 'after-make-frame-functions #'load-tango-theme)
  (load-theme 'tango-dark t))

;; remove irritating 'got redefined' messages
(setq ad-redefinition-action 'accept)

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


;; load preferred theme at startup
(if (daemonp)
    (add-hook 'after-make-frame-functions #'load-tango-theme)
  (load-theme 'tango-dark t)
  (set-background-color "black"))

(switch-to-buffer "*dashboard*")
(declare-function sej-frame-resize-r "init-frame-cmds.el" nil)
(when (display-graphic-p) (sej-frame-resize-r))

(setq custom-file "~/.emacs.d/init.d/init-custom.el")
(load custom-file 'noerror)

(load "server")
(unless (server-running-p) (server-start))

(provide 'init)
;;; init.el ends here

