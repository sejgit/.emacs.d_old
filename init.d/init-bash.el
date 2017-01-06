;;; init-bash.el --- Initialize emacs bash-completions
;;; Commentary:
;; 2016 12 16 init SeJ

;;; Code:

(require 'req-package)

(req-package bash-completion
  :require shell
  :commands bash-completion-dynamic-complete
  :init
  (setq explicit-shell-file-name "bash")
  (setq comint-process-echoes t)
  (setq bash-completion-process-timeout 0.5)
  (add-hook-exec 'shell-mode 'shell-dynamic-complete-functions
                 'bash-completion-dynamic-complete)
  (add-hook-exec 'shell-mode 'shell-command-complete-functions
                 'bash-completion-dynamic-complete))

(req-package company-shell)

(req-package shell-pop
  :bind ("M-\"" . shell-pop))

(provide 'init-bash)
;;; init-bash.el ends here
