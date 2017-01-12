;;; init-frame-cmds.el --- Initialize frame-cmds and setup functions
;;; Commentary:
;; 2017 01 10 init SeJ


;;; Code:


(use-package frame-cmds
  :ensure t
  :bind (("<f7>" . deft)
	 ("C-c d" . deft))
  :config (progn (if (string-equal system-type "windows-nt")
		     (setq deft-directory "C:/Users/NZ891R/gdrive/todo")
		   (setq deft-directory "~/gdrive/todo"))
		 (setq deft-use-filename-as-title t)
		 (setq deft-default-extension "org")
		 (setq deft-text-mode (quote (org-mode)))
		 (setq deft-org-mode-title-prefix t)
		 (setq deft-use-filter-string-for-filename t)
		 (setq deft-auto-save-interval 0)
		 (setq deft-recursive t)
		 (setq deft-extensions (quote ("org" "text" "md" "markdown" "txt")))
		 (setq deft-org-mode-title-prefix t)))

;;set frame full height and 86 columns wide
;;and position at screen left
(defun bjm-frame-resize-l ()
  "set frame full height and 86 columns wide and position at screen left"
  (interactive)
  (set-frame-width (selected-frame) 86)
  (maximize-frame-vertically)
  (set-frame-position (selected-frame) 0 0)
  )

;;set frame full height and 86 columns wide
;;and position at screen right
(defun bjm-frame-resize-r ()
  "set frame full height and 86 columns wide and position at screen right"
  (interactive)
  (set-frame-width (selected-frame) 86)
  (maximize-frame-vertically)
  (set-frame-position (selected-frame) (- (display-pixel-width) (frame-pixel-width)) 0)
  )

;;set frame full height and 86 columns wide
;;and position at screen right of left hand screen in 2 monitor display
;;assumes monitors are same resolution
(defun bjm-frame-resize-r2 ()
  "set frame full height and 86 columns wide and position at screen right of left hand screen in 2 monitor display assumes monitors are same resolution"
  (interactive)
  (set-frame-width (selected-frame) 86)
  (maximize-frame-vertically)
  (set-frame-position (selected-frame) (- (/ (display-pixel-width) 2) (frame-pixel-width)) 0)
  )

;;set keybindings
(global-set-key (kbd "C-c b <left>") 'bjm-frame-resize-l)
(global-set-key (kbd "C-c b <right>") 'bjm-frame-resize-r)
(global-set-key (kbd "C-c b <S-right>") 'bjm-frame-resize-r2)

(provide 'init-deft)
;;; init-deft.el ends here

