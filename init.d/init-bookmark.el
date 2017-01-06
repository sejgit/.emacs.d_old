;;; init-bookmark.el -- Stephen's emacs init-bookmark.el
;;; Commentary:
;; 2016 12 16 init

;;; Code:

(require 'req-package)

(req-package bookmark
  :config (setq bookmark-default-file  (concat user-emacs-directory "bookmarks")))

(provide 'init-bookmark)
;;; init-bookmark.el ends here

