;;; init-misc-pkgs.el --- miscilaneous settings and a few small packages

;;; Commentary:
;;Lots of small packages not deserving of their own file so far.

;;; ChangeLog:
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
;; 2017 03 30 move magit & pyenv-mode-auto to init-python.el
;; 2017 04 04 remove ensure went global ; defer not required for mode,bind,int
;; 2017 05 10 add bookmark+
;; 2017 05 10 add rpn-calc
;; 2017 05 12 mods from purcell/emacs.d
;; 2017 05 17 add help-fns+.el
;; 2017 05 28 add whole-line-or-region
;; 2017 06 12 add mode-icons
;; 2017 06 19 add no-littering
;; 2017 08 02 add beginend mode
;; 2017 08 22 add midnight mode
;; 2017 08 23 add comments to packages
;; 2017 08 23 add expand-region, vlf
;; 2017 08 25 add undo-tree
;; 2017 08 28 add smartscan & dtrt-indent & highlight-numbers
;; 2017 08 30 clean-up, defer, map to sej-mode-map
;; 2017 09 01 turn off for now as not using features
;;; Code:

;; hightlight-numbers in a special way
(use-package highlight-numbers
  :ensure t
  :defer 10
  :init
  (add-hook 'prog-mode-hook #'highlight-numbers-mode))

;; dtrt-indent to automatically set the right indent for other people's files
(use-package dtrt-indent
  :ensure t
  :defer 10
  :diminish t
  :config
  ;; (setq dtrt-indent-active-mode-line-info "")
  )

;; undo tree mode to improve undo features remove C-/ in my keymap for use with dabbrev
(use-package undo-tree
  :ensure t
  :diminish undo-tree-mode
  :config
  (global-undo-tree-mode)
  (setq undo-tree-visualizer-timestamps t))

;; expand selection region larger & smaller
(use-package expand-region
  :ensure t
  :defer t
  :defines sej-mode-map
  :bind (:map sej-mode-map
	      ("s-=" . er/expand-region)
	      ("s--" . er/contract-region)))

;; vlf lets you handle very large files for viewing
(use-package vlf-setup
  :ensure vlf)

;; midnight mode to clear buffers at midnight
(use-package midnight
  :config
  (customize-set-variable 'midnight-mode t)
  )

;; redefine M-< and M-> for some modes
(use-package beginend               ; smart M-< & M->
  :ensure t
  :defer 10
  :config
  (beginend-global-mode)
  )

;; help keeping ~/.emacs.d clean
(use-package no-littering               ; Keep .emacs.d clean
  :ensure t
  :config
  (require 'recentf)
  (add-to-list 'recentf-exclude no-littering-var-directory)
  (add-to-list 'recentf-exclude no-littering-etc-directory))

;; Highlight the cursor whenever the window scrolls
(use-package beacon
  :ensure t
  :defer 5
  :diminish beacon-mode
  :config
  (beacon-mode 1))

;; Moves selected region around
(use-package drag-stuff
  :ensure t
  :defer 5
  :diminish drag-stuff-mode
  :defines sej-mode-map
  :bind (:map sej-mode-map
	      ("M-<down>" . drag-stuff-down)
	      ("M-<up>" . drag-stuff-up))
  :config
  (drag-stuff-global-mode))

;; efficient moving through search terms
(use-package avy
  :ensure t
  :defer 10
  :defines sej-mode-map
  :bind (:map sej-mode-map
	      ("C-<return>" . avy-goto-word-1)))

;; google-this
(use-package google-this
  :ensure t
  :defer 10
  :diminish google-this-mode
  :defines sej-mode-map
  :bind (:map sej-mode-map
	      ("C-c x" . google-this)
	      ("s-g" . google-this))
  :config
  (google-this-mode 1))

;; crux - smart moving to beginning of line or to beginning of text on line
(use-package crux
  :ensure t
  :defer 10
  :defines sej-mode-map
  :bind (:map sej-mode-map
	      ("C-a" . crux-move-beginning-of-line)))

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
  :init
  (add-hook 'prog-mode-hook 'rainbow-delimiters-mode)
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

;; save the place in files
(use-package saveplace
  :ensure t
  :defer 10
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

;; extensions to standard library 'bookmark.el'
(use-package bookmark+
  :disabled ;; turn off for now as not using features
  :ensure t
  :defer 10)

;; quick RPN calculator for hackers
(use-package rpn-calc
  :ensure t
  :defer t)

;; writable grep buffer and apply the changes to files
(use-package wgrep
  :ensure t
  :defer t
  :init
  (setq-default grep-highlight-matches t
		grep-scroll-output t)
  :config
  (when *is-a-mac*
    (setq-default locate-command "which")
    (setq exec-path (append exec-path '("/usr/local/bin"))))

  (when (executable-find "ag")
    (use-package ag)
    (use-package wgrep-ag)
    (setq-default ag-highlight-search t)
    (define-key sej-mode-map (kbd "M-?") 'ag-project)))

;; show vertical lines to guide indentation
(use-package indent-guide
  :ensure t
  :defer 10
  :config
  (add-hook 'prog-mode-hook 'indent-guide-mode)
  :diminish
  indent-guide-mode)

;; display ^L page breaks as tidy horizontal lines
(use-package page-break-lines
  :ensure t
  :defer 10
  :config
  (setq global-page-break-lines-mode t)
  :diminish
  psge-break-lines-mode)

;; extentions to 'help-fns.el'
(use-package help-fns+
  :ensure t
  :defer 5)

;; helful is an improved help-fns & help-fns+
(use-package helpful
  :ensure t
  :defer 10
  :defines sej-mode-map
  :bind (:map sej-mode-map
	      ("C-h f" . helpful-function)
	      ("C-h c" . helpful-command)
	      ("C-h M" . helpful-macro)
	      ("C-h v" . helpful-variable)
	      ("C-h i" . helpful-at-point)))

;; operate on current line if region undefined
(use-package whole-line-or-region
  :ensure t
  :defer 10
  :config
  (whole-line-or-region-global-mode t))

;; TODO move simple modes to own file with batch, yaml, etc
(use-package crontab-mode
  :ensure t
  :defer t
  :config
  (add-auto-mode 'crontab-mode "\\.?cron\\(tab\\)?\\'"))

;; textile markup editing major mode
(use-package textile-mode
  :ensure t
  :defer t
  :config
  (setq auto-mode-alist
	(cons '("\\.textile\\'" . textile-mode) auto-mode-alist)))

;; intelligently call whitespace-cleanup on save
(use-package whitespace-cleanup-mode
  :ensure t
  :defer 10
  ;; intelligently call whitespace-cleanup on save
  :config
  (global-whitespace-cleanup-mode t))

;; major mode for csv
(use-package csv-mode
  :ensure t
  :defer t
  :config
  (add-auto-mode 'csv-mode "\\.[Cc][Ss][Vv]\\'")
  (setq csv-separators '("," ";" "|" " ")))

;; navigate and edit CSV files
(use-package csv-nav
  :ensure t
  :defer t)

;; major mode for editing PHP code
(use-package php-mode
  :ensure t
  :defer t
  :config
  (use-package smarty-mode))

;; major mode for editing conf/ini/properties files
(use-package conf-mode
  :ensure t
  :defer t
  :diminish conf-mode
  :mode "\\.gitconfig$")

;; show icons for modes
(use-package mode-icons
  :ensure t
  :config
  (mode-icons-mode)
  )

(provide 'init-misc-pkgs)
;;; init-misc-pkgs.el ends here









