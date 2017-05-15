;;; init-projectile.el --- Projectile init

;;; Commentary:
;; 2017 05 14 SeJ init from purcell/.emacs.d


;;; Code:

(use-package projectile
  :config
  (add-hook 'after-init-hook 'projectile-global-mode)
  ;; The following code means you get a menu if you hit "C-c p" and wait
  (setq-default
   projectile-mode-line
   '(:eval
     (if (file-remote-p default-directory)
	 " Pr"
       (format " Pr[%s]" (projectile-project-name))))))

(use-package guide-key
  :config
  (add-to-list 'guide-key/guide-key-sequence "C-c p"))

(provide 'init-projectile)
;;; init-projectile.el ends here
