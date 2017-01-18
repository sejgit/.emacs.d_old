;;; init-yaml.el --- Initialize emacs yaml
;;; Commentary:
;; 2016 12 16 init SeJ
;; 2017 01 06 change from req-package to use-package

;;; Code:

(use-package yaml-mode
  :defer t
  :ensure t
  :mode
  "\\.yml$"
  "\\.yaml$")

(provide 'init-yaml)
;;; init-yaml.el ends here
