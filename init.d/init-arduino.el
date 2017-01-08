;;; init-arduino.el --- Initialize emacs arduino-mode
;;; Commentary:
;; 2016 12 16 init SeJ
;; 2017 01 06 change from req-package to use-package


;;; Code:


;; arduino-mode
(use-package arduino-mode
  :ensure t
  :mode "\\.ino$")


(provide 'init-arduino)
;;; init-arduino.el ends here
