;;; init-arduino.el --- Initialize emacs arduino-mode
;;; Commentary:
;; 2016 12 16 init SeJ
;; 2017 01 06 change from req-package to use-package
;; 2017 04 04 remove ensure went global ; defer not required for mode

;;; Code:


;; arduino-mode
(use-package arduino-mode
  :mode "\\.ino$")


(provide 'init-arduino)
;;; init-arduino.el ends here
