;;; init-yaml.el --- Initialize emacs yaml
;;; Commentary:
					; 2016 12 16 init SeJ

;;; Code:


(require 'req-package)

(req-package yaml-mode
  :mode
  "\\.yml$"
  "\\.yaml$"
  :locader el-get)

(provide 'init-yaml)\n;;; init-yaml.el ends here


