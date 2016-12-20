;;; init.el ---  Stephen's emacs init file
;;; Commentary:
;; 2016-02-24 init
;; 2016 03 17 good ideas from aaron bedra's Emacs configuration
;; 2016 11 29 integrate win-nt version & virtualbox
;; 2016 11 30 cleanup & concat with linux versions
;; 2016 12 12 transfer updates from test-version
;; 2016 12 15 updates due to win move to wsys2/ming64


;;; Code:

;; set frame size and position


;;(package-initialize)

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
;; force emacsclient to use right theme & place window in right place
(defun load-tango-theme (frame)
  (select-frame frame)
  (load-theme 'tango-dark t)
  (set-frame-size-according-to-resolution)
  (switch-to-buffer "*dashboard*"))
(if (daemonp)
    (add-hook 'after-make-frame-functions #'load-tango-theme)
  (load-theme 'tango-dark t))

;; whoami
(setq user-full-name "Stephen Jenkins")
(setq user-mail-address "stephenearljenkins@gmail.com")

;; package
(require 'package)
(setq package-enable-at-startup nil)

;; load extensions
(add-hook 'after-init-hook (lambda () (load "~/.emacs.d/init-real.el")))

(provide 'init)
;;; init.el ends here


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (browse-kill-ring discover-my-major which-key writegood-mode yasnippet yaml-mode sunrise-x-loop smex shell-pop req-package rainbow-delimiters org-dashboard org-cliplink org-bullets org markdown-mode macrostep load-dir ido-ubiquitous ido-at-point goto-chg gist furl flycheck-pos-tip flx-ido fic-ext-mode emr elisp-slime-nav dummy-h-mode dired-toggle-sudo dired-rainbow dired-open dired-launch diff-hl deft dashboard company-shell company-quickhelp company-irony batch-mode bash-completion auto-complete async arduino-mode))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
