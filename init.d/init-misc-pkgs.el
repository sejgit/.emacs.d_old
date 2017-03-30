;;; init-misc-pkgs.el --- miscilaneous settings and a few small packages
;;; Commentary:
;; 2017 01 06 init SeJ moved from init-look-and-feel.el the package setups
;; 2017 01 06 add google-this ::search google with C-/ return
;; 2017 01 06 add volatile-highlights  ::temporarily highlight pasting changes
;; 2017 01 06 add rainbow-delimiters ::dired mode for colours
;; 2017 01 06 add saveplace ::return to the same place in saved file
;; 2017 01 06 add conf-mode :: for editing conf/ini files
;; 2017 01 06 remove zenburn-theme ::used from pragmatic Emacs
;; 2017 01 06 change from req-package to use-package
;; 2017 01 10 add swiper to M-s from pragmatic Emacs
;; 2017 01 10 add crux to move to biginning of line intelligently
;; 2017 01 10 add avy for efficient movement through search
;; 2017 01 10 move swiper to own file & add ivy-dired-recent-dirs()
;; 2017 01 16 add drag-stuff to move highlighted region around
;; 2017 01 16 add beacon mode to highlight cursor when moved
;;; Code:

(use-package beacon
  :ensure t
  :defer 2
  :diminish beacon-mode
  :config
  (beacon-mode 1))

;; Moves selected region around.
(use-package drag-stuff
  :ensure t
  :diminish drag-stuff-mode
  :bind (("M-<down>" . drag-stuff-down)
         ("M-<up>" . drag-stuff-up))
  :config
  (drag-stuff-global-mode))

;; efficient moving through search terms
(use-package avy
  :ensure t
  :bind (("M-s" . avy-goto-word-1)))

;; crux - smart moving to beginning of line or to beginning of text on line
(use-package crux
  :ensure t
  :bind ("C-a" . crux-move-beginning-of-line))

;; google-this  C-/ <ret> to activate
(use-package google-this
  :defer t
  :ensure t
  :bind ("C-/" google-this)
  :config
  (google-this-mode 1))

;; volatile highlights - temporarily highlight changes from pasting etc
(use-package volatile-highlights
  :ensure t
  :diminish volatile-highlights-mode
  :config
  (volatile-highlights-mode t))

;; rainbow-delimiters-mode - multicoloured brackets
(use-package rainbow-delimiters
  :ensure t
  :diminish rainbow-delimiters-mode
  :init
  (add-hook 'prog-mode-hook 'rainbow-delimiters-mode)
  :config
  (custom-set-faces
   ;; custom-set-faces was added by Custom.
   ;; If you edit it by hand, you could mess it up, so be careful.
   ;; Your init file should contain only one such instance.
   ;; If there is more than one, they won't work right.

   '(rainbow-delimiters-depth-1-face ((t (:foreground "red" :height 1.0))))
   '(rainbow-delimiters-depth-2-face ((t (:foreground "orange" :height 1.0))))
   '(rainbow-delimiters-depth-3-face ((t (:foreground "yellow" :height 1.0))))
   '(rainbow-delimiters-depth-4-face ((t (:foreground "green" :height 1.0))))
   '(rainbow-delimiters-depth-5-face ((t (:foreground "blue" :height 1.0))))
   '(rainbow-delimiters-depth-6-face ((t (:foreground "violet" :height 1.0))))
   '(rainbow-delimiters-depth-7-face ((t (:foreground "purple" :height 1.0))))
   '(rainbow-delimiters-depth-8-face ((t (:foreground "black" :height 1.0))))
   '(rainbow-delimiters-unmatched-face ((t (:background "cyan" :height 1.0))))
   ))


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
  :defer t
  :ensure t
  :diminish conf-mode
  :mode "\\.gitconfig$")

(provide 'init-misc-pkgs)
;;; init-misc-pkgs.el ends here

