;;; init-dired.el --- Initialize emacs dired mode
;;; Commentary:
					; 2016 12 16 init SeJ

;;; Code:


(require 'req-package)

;; single dired
(req-package dired
  :commands dired
  :require autorevert diff-hl
  :bind (:map dired-mode-map
              ("M-i" . helm-swoop)
              ("M-RET" . dired-find-file-other-window))
  :config
  (add-hook-exec 'dired-mode (lambda () (auto-revert-mode 1)))
  (add-hook-exec 'dired-mode (lambda () (diff-hl-dired-mode 1))))

;; sunrise commander
(req-package sunrise-commander :commands sunrise-cd)

(req-package sunrise-x-loop :require sunrise-commander)

;; dired rainbow
(req-package dired-rainbow :require dired)

;; dired open
(req-package dired-open :require dired)

(req-package dired-launch
  :require dired
  :init (dired-launch-enable))

;; dired-toggle-sudo
(req-package dired-toggle-sudo
  :config
  (define-key dired-mode-map (kbd "C-c C-s") 'dired-toggle-sudo)
  (eval-after-load 'tramp
    '(progn
       ;; Allow to use: /sudo:user@host:/path/to/file
       (add-to-list 'tramp-default-proxies-alist
		    '(".*" "\\`.+\\'" "/ssh:%h:")))))


(provide 'init-dired)
;;; init-dired.el ends here
