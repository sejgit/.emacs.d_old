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

;;; Code:

;; whoami
(setq user-full-name "Stephen Jenkins")
(setq user-mail-address "stephenearljenkins@gmail.com")

;; package
(require 'package)
(setq package-enable-at-startup nil)

;; elpa
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives '("elpa" . "http://tromey.com/elpa/"))
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/"))
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/"))
(add-to-list 'package-archives '("sunrise" . "http://joseito.republika.pl/sunrise-commander/"))
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))
(require 'diminish)
(require 'bind-key)

;; recompile configs
(add-hook 'kill-emacs-hook (lambda () (byte-recompile-directory my-init-dir 0 t)))

;; themes
;; tango-dark
(defun load-tango-theme (frame)
  (select-frame frame)
  (load-theme 'tango-dark t)
  (set-frame-size-according-to-resolution)
  (switch-to-buffer "*dashboard*"))

;; load preferred theme at startup
(if (daemonp)
    (add-hook 'after-make-frame-functions #'load-tango-theme)
  (load-theme 'tango-dark t))

(defconst my-init-dir "~/.emacs.d/init.d")

;; remove irritating 'got redefined' messages
(setq ad-redefinition-action 'accept)

;; init.d
(random t)
(use-package load-dir
  :ensure t
  :init
  (setq force-load-messages t)
  (setq load-dir-debug nil)
  (setq load-dir-recursive nil)
  :config
  (load-dir-one my-init-dir))


;; load preferred theme at startup
(if (daemonp)
    (add-hook 'after-make-frame-functions #'load-tango-theme)
  (load-theme 'tango-dark t))

(switch-to-buffer "*dashboard*")
(dashboard-insert-startupify-lists)
(sej-frame-resize-r)

(provide 'init)
;;; init.el ends here

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (frame-cmds el-get aggressive-indent zenburn-theme yasnippet yaml-mode writegood-mode which-key volatile-highlights use-package sunrise-x-loop smex shell-pop rainbow-delimiters org-dashboard org-cliplink org-bullets markdown-mode macrostep log4e load-dir ido-ubiquitous ido-at-point goto-chg google-this golden-ratio gist furl flycheck-pos-tip flx-ido fic-ext-mode emr elisp-slime-nav elfeed-org dummy-h-mode dired-toggle-sudo dired-ranger dired-rainbow dired-open dired-narrow dired-launch diff-hl deft dashboard crux counsel company-shell company-quickhelp company-irony browse-kill-ring batch-mode bash-completion avy auto-complete async arduino-mode))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
