;;; init-elfeed.el --- Initialize emacs elfeed
;;; Commentary:
;; settings and packages for elfeed in Emacs

;;; ChangeLog
;; 2017 01 05 init SeJ
;; 2017 01 06 change from req-package to use-package
;; 2017 08 19 disabled for now until better use case
;; 2017 11 17 another try

;;; Code:


(use-package elfeed
  :ensure t
  :functions
  elfeed-search-untag-all-unread
  elfeed-db-load
  elfeed-search-update--force
  elfeed-db-save
  elfeed-expose
  elfeed-search-toggle-all
  :bind (:map elfeed-search-mode-map
	      ("q" . bjm/elfeed-save-db-and-bury)
	      ("Q" . bjm/elfeed-save-db-and-bury)
	      ("m" . elfeed-toggle-star)
	      ("M" . elfeed-toggle-star)
	      ("C-<ret>" . elfeed-mark-all-as-read))
  :config
  (defun elfeed-mark-all-as-read ()
    "Wrapper to mar all items as read."
    (interactive)
    (push-mark (point))
    (push-mark (point-max) nil t)
    (goto-char (point-min))
    (elfeed-search-untag-all-unread))

  ;;functions to support syncing .elfeed between machines
  ;;makes sure elfeed reads index from disk before launching
  (defun bjm/elfeed-load-db-and-open ()
    "Wrapper to load the elfeed db from disk before opening."
    (interactive)
    (elfeed-db-load)
    (elfeed)
    (elfeed-search-update--force))

  ;;write to disk when quiting
  (defun bjm/elfeed-save-db-and-bury ()
    "Wrapper to save the elfeed db to disk before burying buffer."
    (interactive)
    (elfeed-db-save)
    (quit-window))

  (defalias 'elfeed-toggle-star
    (elfeed-expose #'elfeed-search-toggle-all 'star))
  )

(use-package elfeed-goodies
  :ensure t
  :after elfeed
  :config
  (elfeed-goodies/setup))

(use-package elfeed-org
  :ensure t
  :after elfeed
  :config (elfeed-org)
  (setq rmh-elfeed-org-files (list "~/.emacs.d/elfeed.org"))
  )

(provide 'init-elfeed)
;;; init-elfeed.el ends here
