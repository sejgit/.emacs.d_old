;;; init-dired.el --- Initialize emacs dired mode
;;; Commentary:
;; 2016 12 16 init SeJ
;; 2017 01 06 change from req-package to use-package
;; 2017 01 11 add dired-narrow :: filter dired screen

;;; Code:


;;autorevert
(use-package autorevert
  :ensure t)

;;diff-hl
(use-package diff-hl
  :ensure t)

;; single dired
(use-package dired
  :commands dired
  :bind (:map dired-mode-map
              ("M-i" . helm-swoop)
              ("M-RET" . dired-find-file-other-window))
  :config
  (auto-revert-mode 1)
  (diff-hl-dired-mode 1))

(use-package dired-ranger
  :ensure t
  :bind (:map dired-mode-map
              ("W" . dired-ranger-copy)
              ("X" . dired-ranger-move)
              ("Y" . dired-ranger-paste)))

;; sunrise commander
(use-package sunrise-commander
  :ensure t
  :commands sunrise-cd)

(use-package sunrise-x-loop
  :ensure t)

;; dired rainbow
(use-package dired-rainbow
  :ensure t)

;; dired open
(use-package dired-open
  :ensure t)

(use-package dired-launch
  :ensure t
  :config (dired-launch-enable))

;;narrow dired to match filter
(use-package dired-narrow
  :ensure t
  :bind (:map dired-mode-map
              ("/" . dired-narrow)))

(provide 'init-dired)
;;; init-dired.el ends here
