;;; init-appearance.el --- packages & settings for appearance

;;; Commentary:
;; merging of settings and other package files into an appearance centric file

;;; ChangeLog:
;; 2017 01 06 init SeJ moved from init-misc-pkgs.el
;;                                init-aggressive-indent.el
;;                                init-golden-ratio.el


;;; Code:

(use-package golden-ratio
  :ensure t
  :defer 5
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
  :defer 10
  :config
  (add-hook 'prog-mode-hook #'highlight-numbers-mode))

;; dtrt-indent to automatically set the right indent for other people's files
(use-package dtrt-indent
  :ensure t
  :defer 10
  :diminish t
  :config
  ;; (setq dtrt-indent-active-mode-line-info "")
  )

(use-package aggressive-indent
  :defer 2
  :config (progn (global-aggressive-indent-mode 1)
		 (add-to-list 'aggressive-indent-excluded-modes 'html-mode)))

;; Highlight the cursor whenever the window scrolls
(use-package beacon
  :ensure t
  :defer 5
  :diminish beacon-mode
  :config
  (beacon-mode 1))

;; volatile highlights - temporarily highlight changes from pasting etc
(use-package volatile-highlights
  :ensure t
  :defer 10
  :diminish volatile-highlights-mode
  :config
  (volatile-highlights-mode t))

;; rainbow-delimiters-mode - multicoloured brackets
(use-package rainbow-delimiters
  :ensure t
  :defer 10
  :diminish rainbow-delimiters-mode
  :config
  (add-hook 'prog-mode-hook 'rainbow-delimiters-mode)
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









