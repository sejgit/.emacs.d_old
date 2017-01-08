;;; init-bash.el --- Initialize emacs bash-completions
;;; Commentary:
;; 2016 12 16 init SeJ
;; 2017 01 06 change from req-package to use-package
;;; Code:


(use-package bash-completion
  :ensure t
  :commands bash-completion-dynamic-complete
  :init
  (setq explicit-shell-file-name "bash")
  (setq comint-process-echoes t)
  (setq bash-completion-process-timeout 0.5))

(use-package company-shell
  :ensure t)

(use-package shell-pop
  :ensure t
  :bind ("M-\"" . shell-pop))

(provide 'init-bash)
;;; init-bash.el ends here
