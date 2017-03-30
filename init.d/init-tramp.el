;;; init-tramp.el --- Stephen's emacs init-tramp.el
;;; Commentary:
;; tramp setup.

;; 2017 03 14 init SeJ
;; 2017 03 28 fix settings

;;; Code:

(setq
 tramp-default-method "ssh"
 tramp-default-user "pi"
 tramp-default-host "home"
 password-cache-expiry nil)

(defadvice tramp-handle-write-region
    (after tramp-write-beep-advice activate)
  "Make tramp beep after writing a file."
  (interactive)
  (beep))

(defadvice tramp-handle-do-copy-or-rename-file
    (after tramp-copy-beep-advice activate)
  "Make tramp beep after copying a file."
  (interactive)
  (beep))

(defadvice tramp-handle-insert-file-contents
    (after tramp-insert-beep-advice activate)
  "Make tramp beep after inserting a file."
  (interactive)
  (beep))



(provide 'init-tramp)
;;; init-tramp.el ends here
