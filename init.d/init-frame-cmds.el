;;; init-frame-cmds.el --- Initialize frame-cmds and setup functions
;;; Commentary:
;; 2017 01 10 init SeJ


;;; Code:

(use-package frame-cmds
  :ensure t
  :bind (("C-c b <left>" . sej-frame-resize-l)
	 ("C-c b <right>" . sej-frame-resize-r)
	 ("C-c b <S-right>" . sej-frame-resize-r2))
  :init
  ;;set frame full height and 86 columns wide
  ;;and position at screen left
  (defun sej-frame-resize-l ()
    "set frame full height and 86 columns wide and position at screen left"
    (interactive)
    (set-frame-width (selected-frame) 86)
    (maximize-frame-vertically)
    (set-frame-position (selected-frame) 0 0)
    )

  ;;set frame full height and 86 columns wide
  ;;and position at screen right
  (defun sej-frame-resize-r ()
    "set frame full height and 86 columns wide and position at screen right"
    (interactive)
    (set-frame-width (selected-frame) 86)
    (maximize-frame-vertically)
    (set-frame-position (selected-frame) (- (display-pixel-width) (frame-pixel-width)) 0)
    )

  ;;set frame full height and 86 columns wide
  ;;and position at screen right of left hand screen in 2 monitor display
  ;;assumes monitors are same resolution
  (defun sej-frame-resize-r2 ()
    "set frame full height and 86 columns wide and position at screen right of left hand screen in 2 monitor display assumes monitors are same resolution"
    (interactive)
    (set-frame-width (selected-frame) 86)
    (maximize-frame-vertically)
    (set-frame-position (selected-frame) (- (/ (display-pixel-width) 2) (frame-pixel-width)) 0)
    )
  )

(provide 'init-frame-cmds)
;;; init-frame-cmds.el ends here

