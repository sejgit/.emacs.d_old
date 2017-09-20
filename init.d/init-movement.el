;;; init-movement.el --- packages and settings for movement

;;; Commentary:
;; merging of settings and other package files into a movement centric file

;;; ChangeLog:
;; 2017 09 20 init SeJ moved from init-misc-pakgs.el
;;                                init-goto-chg.el
;;                                init-

;;; Code:

;; framemove will move frames when at limits of current frame
(use-package framemove
  :ensure t
  :config
  (setq framemove-hook-into-windmove t))

;; buffer-move to swap buffers between windows
(use-package buffer-move
  :ensure t
  :defer t
  :bind (:map sej-mode-map
              ("<s-up>" . buf-move-up)
              ("<s-down>" . buf-move-down)
              ("<s-left>" . buf-move-left)
              ("<s-right>" . buf-move-right)))

(use-package goto-chg
  :ensure t
  :defer t
  :defines sej-mode-map
  :bind (:map sej-mode-map
              ;;("C-." . goto-last-change)
              ;; M-. can conflict with etags tag search. But C-. can get overwritten
              ;; by flyspell-auto-correct-word. And goto-last-change needs a really fast key.
              ("M-." . goto-last-change)
              ;; ensure that even in worst case some goto-last-change is available
              ("C-M-." . goto-last-change)
              ;; added reverse below
              ("C-," . goto-last-change-reverse)))

;; redefine M-< and M-> for some modes
(use-package beginend               ; smart M-< & M->
  :ensure t
  :defer 10
  :config
  (beginend-global-mode)
  )

;; efficient moving through search terms
(use-package avy
  :ensure t
  :defer 10
  :defines sej-mode-map
  :bind (:map sej-mode-map
	      ("C-<return>" . avy-goto-word-1)))

;; crux - smart moving to beginning of line or to beginning of text on line
(use-package crux
  :ensure t
  :defer 10
  :defines sej-mode-map
  :bind (:map sej-mode-map
	      ("C-a" . crux-move-beginning-of-line)))

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

;; save the place in files
(use-package saveplace
  :ensure t
  :defer 10
  :config
  (setq-default save-place t))

;; TODO extensions to standard library 'bookmark.el'
;; (use-package bookmark+
;;   :ensure t
;;   :defer 10)


(provide 'init-movement)
;;; init-movement.el ends here









