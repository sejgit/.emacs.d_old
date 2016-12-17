;; Stephen's emacs init-bookmark.el
;; 2016 12 16


(require 'req-package)

(req-package bookmark
  :config (setq bookmark-default-file  (concat user-emacs-directory "bookmarks")))

(provide 'init-bookmark)
