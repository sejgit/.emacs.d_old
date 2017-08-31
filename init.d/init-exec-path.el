;;; init-exec-path.el --- Initialize emacs exec-path
;;; Commentary:
;; exec path and shell init

;; ChangeLog
;; 2017 05 26 add standard use-package terminology
;; 2017 08 25 add exec-path-from-shell from EOS

;;; Code:

(use-package exec-path-from-shell
  :config
  (dolist (var '("SSH_AUTH_SOCK" "SSH_AGENT_PID" "GPG_AGENT_INFO" "LANG" "LC_CTYPE"))
    (add-to-list 'exec-path-from-shell-variables var))
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize)))

(use-package exec-path-from-shell
  :init
  (setq exec-path-from-shell-check-startup-files nil)
  (exec-path-from-shell-initialize))


(provide 'init-exec-path)
;;; init-exec-path.el ends here
