;;; init-bash.el --- Initialize emacs bash-completions
;;; Commentary:
;; 2016 12 16 init SeJ
;; 2017 01 06 change from req-package to use-package
;; 2017 04 04 remove ensure went global ; defer not required for mode,bind,int

;;; Code:


(use-package bash-completion
  :defer 2
  :commands bash-completion-dynamic-complete
  :defines explicit-shell-file-name
  :config
  (setq explicit-shell-file-name "bash")
  (setq comint-process-echoes t)
  (setq bash-completion-process-timeout 0.5))

(use-package company-shell
  :defer 2)

(use-package shell-pop
  :bind ("M-\"" . shell-pop))

(provide 'init-bash)
;;; init-bash.el ends here
