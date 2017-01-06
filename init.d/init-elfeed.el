;;; init-elfeed.el --- Initialize emacs elfeed
;;; Commentary:
					; 2017 01 05 init SeJ

;;; Code:


(require 'req-package)

(req-package elfeed-org
  :ensure t
  :init (elfeed-org)
  (setq rmh-elfeed-org-files (list ".emacs.d/elfeed.org"))
  :require elfeed
  )

(req-package elfeed
  :ensure t
  :init (prog ((defalias 'elfeed-toggle-star
  (elfeed-expose #'elfeed-search-toggle-all 'star))

(eval-after-load 'elfeed-search
  '(define-key elfeed-search-mode-map (kbd "m") 'elfeed-toggle-star))))
  :require elfeed-org
  )



(provide 'init-elfeed)
;;; init-elfeed.el ends here
