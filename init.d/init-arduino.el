;;; init-arduino.el --- Initialize emacs arduino-mode
;;; Commentary:
					; 2016 12 16 init SeJ

;;; Code:


(require 'req-package)

;; arduino-mode
(req-package arduino-mode
  :mode "\\.ino$")


(provide 'init-arduino)
;;; init-arduino.el ends here
