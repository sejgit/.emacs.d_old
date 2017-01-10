;;; init-misc-pkgs.el --- miscilaneous settings and a few small packages
;;; Commentary:
;; 2017 01 06 init SeJ moved from init-look-and-feel.el the package setups
;; 2017 01 06 google-this ::search google with C-/ return
;; 2017 01 06 volatile-highlights  ::temporarily highlight pasting changes
;; 2017 01 06 rainbow-delimiters ::dired mode for colours
;; 2017 01 06 saveplace ::return to the same place in saved file
;; 2017 01 06 conf-mode :: for editing conf/ini files
;; 2017 01 06 zenburn-theme ::used from pragmatic Emacs
;; 2017 01 06 change from req-package to use-package
;; 2017 01 10 add swiper to M-s from pragmatic Emacs
;;; Code:


;; swiper
(use-package swiper
  :ensure t
  :bind ("C-s" . swiper))

;; google-this  C-/ <ret> to activate
(use-package google-this
  :ensure t
  :config
  (google-this-mode 1))

;; volatile highlights - temporarily highlight changes from pasting etc
(use-package volatile-highlights
  :ensure t
  :config
  (volatile-highlights-mode t))

;; rainbow-delimiters-mode
(use-package rainbow-delimiters
  :ensure t
  :config
  (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))

;; save the place in files
(use-package saveplace
  :ensure t
  :config
  (setq-default save-place t))

;; conf-mode  see below for assignments made in package
;; (define-key map "\C-c\C-u" 'conf-unix-mode)
;; (define-key map "\C-c\C-w" 'conf-windows-mode)
;; (define-key map "\C-c\C-j" 'conf-javaprop-mode)
;; (define-key map "\C-c\C-s" 'conf-space-keywords)
;; (define-key map "\C-c " 'conf-space-keywords)
;; (define-key map "\C-c\C-c" 'conf-colon-mode)
;; (define-key map "\C-c:" 'conf-colon-mode)
;; (define-key map "\C-c\C-x" 'conf-xdefaults-mode)
;; (define-key map "\C-c\C-p" 'conf-ppd-mode)
;; (define-key map "\C-c\C-q" 'conf-quote-normal)
;; (define-key map "\C-c\"" 'conf-quote-normal)
;; (define-key map "\C-c'" 'conf-quote-normal)
;; (define-key map "\C-c\C-a" 'conf-align-assignments)

(use-package conf-mode
  :ensure t
  :mode "\\.gitconfig$")

(provide 'init-misc-pkgs)
;;; init-misc-pkgs.ini ends here

