;;; init-yaml.el --- Initialize emacs yaml
;;; Commentary:
;; 2016 12 16 init SeJ
;; 2017 01 06 change from req-package to use-package
;; 2017 04 04 remove ensure went global ; defer not required for mode,bind,int

;;; Code:

(use-package yaml-mode
  :mode
  "\\.yml$"
  "\\.yaml$")

(provide 'init-yaml)
;;; init-yaml.el ends here
