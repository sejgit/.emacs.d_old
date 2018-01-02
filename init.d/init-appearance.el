;;; init-appearance.el --- packages & settings for appearance

;;; Commentary:
;; merging of settings and other package files into an appearance centric file

;;; ChangeLog:
;; 2017 01 06 init SeJ moved from init-misc-pkgs.el
;;                                init-aggressive-indent.el
;;                                init-golden-ratio.el
;; 2017 11 29 clean up use-package and add :hook
;; 2017 12 26 add dimmer
;;; Code:

(use-package dimmer
  :ensure t
  :defer 5
  :config
  (setq dimmer-percent 0.40)
  (dimmer-activate))

(use-package golden-ratio
  :ensure t
  :defines sej-mode-map
  :diminish golden-ratio-mode
  :bind (:map sej-mode-map
	      ("M-'" . next-multiframe-window))
  :config
  (golden-ratio-mode 1)
  (setq golden-ratio-auto-scale t)
  (add-to-list 'golden-ratio-extra-commands 'next-multiframe-window))

;; hightlight-numbers in a special way
(use-package highlight-numbers
  :ensure t
  :hook (prog-mode . highlight-numbers-mode))

;; dtrt-indent to automatically set the right indent for other people's files
(use-package dtrt-indent
  :ensure t
  :diminish
  :config
  ;; (setq dtrt-indent-active-mode-line-info "")
  )

(use-package aggressive-indent
  :ensure t
  :config
  (global-aggressive-indent-mode 1)
  (add-to-list 'aggressive-indent-excluded-modes 'html-mode))

;; Highlight the cursor whenever the window scrolls
(use-package beacon
  :ensure t
  :diminish beacon-mode
  :config
  (beacon-mode 1))

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
  :hook (prog-mode . rainbow-delimiters-mode)
  :config
  (custom-set-faces
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

;; show icons for modes
(use-package mode-icons
  :ensure t
  :config
  (mode-icons-mode))


(provide 'init-appearance)
;;; init-appearance.el ends here










