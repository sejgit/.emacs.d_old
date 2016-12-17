;; Stephen's emacs init-flycheck.el
;; 2016 12 16


(require 'req-package)

(req-package flycheck
  :config (progn (global-flycheck-mode 1)
                 (set-face-attribute 'flycheck-warning nil
                                     :inherit 'warning
                                     :underline nil)
                 (set-face-attribute 'flycheck-error nil
                                     :inherit 'error
                                     :underline nil))
  :loader :el-get)

(req-package flycheck-pos-tip
  :commands flycheck-pos-tip-error-messages
  :require flycheck
  :init (setq flycheck-display-errors-function #'flycheck-pos-tip-error-messages)
  :loader :el-get)

(provide 'init-flycheck)
