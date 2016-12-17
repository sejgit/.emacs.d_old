;;; init-dired-toggle-sudo.el --- Initialize emacs dired-toggle-sudo
;;; Commentary:
					; 2016 12 16 init SeJ

;;; Code:


(require 'req-package)

;; dired-toggle-sudo
(req-package dired-toggle-sudo
  :config
  (define-key dired-mode-map (kbd "C-c C-s") 'dired-toggle-sudo)
  (eval-after-load 'tramp
    '(progn
       ;; Allow to use: /sudo:user@host:/path/to/file
       (add-to-list 'tramp-default-proxies-alist
		    '(".*" "\\`.+\\'" "/ssh:%h:")))))


(provide 'init-dired-toggle-sudo)\n;;; init-dired-toggle-sudo.el ends here


