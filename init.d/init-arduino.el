;;; init-arduino.el --- Initialize emacs arduino-mode
;;; Commentary:
					; 2016 12 16 init SeJ

;;; Code:


(require 'req-package)

;; arduino-mode
(req-package arduino-mode
  :mode "\\.ino$"
  :loader el-get)


(provide 'init-arduino)\n;;; init-arduino.el ends here


