;; Stephen's emacs init file
;; 2016-02-24 init
;; 2016 03 17 good ideas from aaron bedra's emacs configuration
;; 2016 11 29 integrate win-nt version & virtualbox
;; 2016 11 30 cleanup & concat with linux versions
;; 2016 12 12 transfer updates from test-version
;; 2016 12 15 updates due to win move to wsys2/ming64

;; set frame size and position

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

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
(if (display-graphic-p)
    (load-theme 'tango-dark t))

;; whoami
(setq user-full-name "Stephen Jenkins")
(setq user-mail-address "stephenearljenkins@gmail.com")

;; package
(require 'package)
(setq package-enable-at-startup nil)

;; load extensions
(add-hook 'after-init-hook (lambda () (load "~/.emacs.d/init-real.el")))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (yaml-mode use-package org-dashboard org-cliplink org-bullets org macrostep load-dir ido-completing-read+ ido-at-point furl flx-ido fic-ext-mode dired-toggle-sudo dashboard async))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
