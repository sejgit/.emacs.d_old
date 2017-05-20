;;; init-bash.el --- Initialize emacs bash-completions

;;; Commentary:


;; 2016 12 16 init SeJ
;; 2017 01 06 change from req-package to use-package
;; 2017 04 04 remove ensure went global ; defer not required for mode,bind,int
;; 2017 05 19 change from shell-pop to shell
;; 2017 05 19 add compilation-shell-minor-mode from mastering Emacs

;;; Code:

(use-package bash-completion
  :defer 2
  :commands bash-completion-dynamic-complete
  :defines explicit-shell-file-name
  :config
  (setq explicit-shell-file-name "bash")
  (setq comint-process-echoes t)
  (setq bash-completion-process-timeout 0.5)
  (add-hook 'shell-mode-hook 'compilation-shell-minor-mode)  )

(use-package company-shell
  :defer 2)

(provide 'init-bash)
;;; init-bash.el ends here
