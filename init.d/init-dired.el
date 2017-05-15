;;; init-dired.el --- Initialize emacs dired mode
;;; Commentary:
;; 2016 12 16 init SeJ
;; 2017 01 06 change from req-package to use-package
;; 2017 01 11 add dired-narrow :: filter dired screen
;; 2017 04 04 remove ensure went global ; defer not required for mode,bind,int
;; 2017 05 12 updates from purcell/emacs.d

;;; Code:

(use-package dired+
  :init
  (auto-revert-mode 1)
  (diff-hl-dired-mode 1)
  (setq diredp-hide-details-initially-flag nil
	dired-recursive-deletes 'top
	dired-hide-details nil
	dired-dwim-target t)) 

(use-package dired-sort)

;; Prefer g-prefixed coreutils version of standard utilities when available
(let ((gls (executable-find "gls")))
  (when gls (setq insert-directory-program gls)))

;;autorevert
(use-package autorevert
  :defer t)

;;diff-hl
(use-package diff-hl
  :defer t
  :config
  (add-hook 'dired-mode-hook 'diff-hl-dired-mode)
  (add-hook 'prog-mode-hook 'turn-on-diff-hl-mode)
  (add-hook 'vc-dir-mode-hook 'turn-on-diff-hl-mode))

(use-package browse-at-remote)


;; dired rainbow
(use-package dired-rainbow
  :defer t)

;; dired open
(use-package dired-open
  :defer t)

(use-package dired-launch
  :defer t
  :config (dired-launch-enable))

;;narrow dired to match filter
(use-package dired-narrow
  :bind (:map dired-mode-map
              ("/" . dired-narrow)))

(provide 'init-dired)
;;; init-dired.el ends here



