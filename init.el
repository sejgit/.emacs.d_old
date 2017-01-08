;;; init.el ---  Stephen's emacs init file
;;; Commentary:
;; 2016-02-24 init
;; 2016 03 17 good ideas from aaron bedra's Emacs configuration
;; 2016 11 29 integrate win-nt version & virtualbox
;; 2016 11 30 cleanup & concat with linux versions
;; 2016 12 12 transfer updates from test-version
;; 2016 12 15 updates due to win move to wsys2/ming64
;; 2017 01 06 bring together init & real-init

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

;; el-get
(add-to-list 'load-path "~/.emacs.d/el-get/el-get")
(unless (require 'el-get nil 'noerror)
  (with-current-buffer
      (url-retrieve-synchronously
       "https://raw.githubusercontent.com/dimitri/el-get/master/el-get-install.el")
    (goto-char (point-max))
    (eval-print-last-sexp)))
(add-to-list 'el-get-recipe-path "~/.emacs.d/el-get-user/recipes")
(require 'el-get)
(el-get 'sync)

;; recompile configs
(add-hook 'kill-emacs-hook (lambda () (byte-recompile-directory my-init-dir 0 t)))

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

(provide 'init)
;;; init.el ends here
