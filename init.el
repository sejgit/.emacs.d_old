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

;;; Code:

;; debugger on
(setq debug-on-error t)
(setq debug-on-quit t)

(defvar emacs-start-time (current-time)
  "Time Emacs was started.")

;; reduce garbage collection
(setq gc-cons-threshold (* 50 1024 1024))

;; Bootstrap config
;; whoami
(setq user-full-name "Stephen Jenkins")
(setq user-mail-address "stephenearljenkins@gmail.com")

(let ((minver "24.1"))
  (when (version<= emacs-version minver)
    (error "Your Emacs is too old -- this config requires v%s or higher" minver)))
(when (version<= emacs-version "24.4")
  (message "Your Emacs is old, and some functionality in this config will be disabled. Please upgrade if possible."))

;; wrap init in this to reduce file access times
(let ((file-name-handler-alist nil))

  ;; define a const for mac behaviour
  (defconst *is-a-mac* (eq system-type 'darwin))

  ;; turn on syntax highlightng for all buffers
  (global-font-lock-mode t)

  ;; raise the maximum number of logs in the *Messages* buffer
  (setq message-log-max 16384)

  ;; wait a bit longer than the default 0.5s before assuming Emacs is idle
  (setq idle-update-delay 2)

  ;; make gnutls a bit safer
  (setq gnutls-min-prime-bits 4096)

  (add-to-list 'load-path (expand-file-name "init.d" user-emacs-directory))
  (require 'init-utils)

  (require 'package)
  (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/"))
  (add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
  (add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/"))
  (setq load-prefer-newer t)

  ;; package list
  (setq package-list
        '(use-package
           aggressive-indent
           auto-complete
           bash-completion
           company-shell
           framemove
           buffer-move
           browse-kill-ring
           company
           company-quickhelp
           company-statistics
           smart-tab
           dashboard
           page-break-lines
           deft
           dired+
           dired-collapse
           async
           browse-at-remote
           dired-rainbow
           dired-open
           dired-launch
           dired-sort
           dired-narrow
           async
           quick-preview
           all-the-icons
           all-the-icons-dired
           ;;elfeed
           ;;elfeed-org
           fic-mode
           flymake
           flycheck-color-mode-line
           flycheck
           flycheck-pos-tip
           helm-flycheck
           frame-cmds
           gist
           magit
           git-blamed
           gitignore-mode
           gitconfig-mode
           git-timemachine
           fullframe
           git-messenger
           github-clone
           github-issues
           magit-gh-pulls
           golden-ratio
           goto-chg
           helm
           helm-swoop
           helm-descbinds
           ido
           flx-ido
           smex
           paredit
           eldoc
           elisp-slime-nav
           arduino-mode
           batch-mode
           conf-mode
           crontab-mode
           csv-mode
           csv-nav
           nov
           php-mode
           textile-mode
           yaml-mode
           highlight-numbers
           dtrt-indent
           undo-tree
           expand-region
           vlf
           midnight
           beginend
           beacon
           drag-stuff
           avy
           google-this
           crux
           volatile-highlights
           rainbow-delimiters
           saveplace
           bookmark+
           rpn-calc
           wgrep
           ag
           wgrep-ag
           help-fns+
           helpful
           whole-line-or-region
           mode-icons
           org
           org-bullets
           org-dashboard
           projectile
           helm-projectile
           helm-ag
           grep
           pip-requirements
           python-environment
           ediff
           python
           anaconda-mode
           pyvenv
           jedi
           exec-path-from-shell
           with-editor
           keychain-environment
           eshell
           eshell-prompt-extras
           flyspell
           thesaurus
           synosaurus
           autoinsert
           tramp
           pass
           view
           doc-view
           which-key
           indent-guide
           page-break-lines
           whitespace-cleanup-mode
           markdown-mode
           adoc-mode
           skeleton
           ))

  ;; Fire up package.el
  (setq package-enable-at-startup nil)
  (package-initialize)

  (unless package-archive-contents
    (package-refresh-contents))

  ;; install the missing packages
  (dolist (package package-list)
    (unless (package-installed-p package)
      (package-install package)))

  (eval-when-compile
    (require 'use-package)
    (setq use-package-always-ensure t))
  (require 'cl)
  (require 'diminish)
  (require 'bind-key)
  (require 'cl-lib)

  (set-fontset-font "fontset-default"
                    (cons page-break-lines-char page-break-lines-char)
                    (face-attribute 'default :family))

  ;; save histories
  (require 'savehist)
  (setq savehist-file (concat user-emacs-directory "savehist"))
  (savehist-mode 1)
  (setq savehist-save-minibuffer-history 1)
  (setq savehist-additional-variables
        '(kill-ring
          search-ring
          regexp-search-ring))
  (setq-default save-place t)

  ;; recompile configs
  (defconst my-init-dir "~/.emacs.d/init.d")
  (use-package auto-compile
    :ensure t
    :init
    (auto-compile-on-load-mode)
    (auto-compile-on-save-mode)
    )

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
      (add-hook 'after-make-frame-functions #'load-cyberpunk-theme)
    (load-theme 'cyberpunk t) )

  (switch-to-buffer "*dashboard*")
  (declare-function sej-frame-resize-r "init-frame-cmds.el" nil)
  (when (display-graphic-p) (sej-frame-resize-r))

  (setq custom-file "~/.emacs.d/init.d/init-custom.el")
  (load custom-file 'noerror)

  ;; Lame, server has bad autoloads :(
  (require 'server nil t)
  (use-package server
    ;;  :if window-system
    :init
    (when (not (server-running-p server-name))
      (server-start)))

  (use-package uptimes)

  (setq debug-on-error nil)
  (setq debug-on-quit nil)
  )

(provide 'init)
;;; init.el ends here

