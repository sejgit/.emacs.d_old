;;; init-dashboard.el ---  Stephen's emacs init-dashboard.el
;;; Commentary:
;; set-up for dashboard

;;; ChangeLog
;; 2016 12 16 init sej
;; 2017 01 06 change from req-package to use-package
;; 2017 11 30 updates to dashboard-items
;; 2018 07 12 update projects items
;; 2018 07 22 remove hook as part of startup
;; 2018 08 02 fixed crashing (I hope)
;;            added initial-buffer-choice for use in emacsclient
;;            added bind & hook

;;; Code:

(use-package dashboard
  :ensure t
  :defines sej-mode-map
  :hook (after-init . dashboard-refresh-buffer)
  :bind (:map sej-mode-map
	      ("C-c s d" . sej/daboard-fix-registers))
  :config
  ;; Set the banner
  (setq dashboard-startup-banner 'official)
  ;; Value can be
  ;; 'official which displays the official emacs logo
  ;; 'logo which displays an alternative emacs logo
  ;; 1, 2 or 3 which displays one of the text banners
  ;; "path/to/your/image.png which displays whatever image you would prefer

  (setq dashboard-items '((recents . 15)
			  (bookmarks . 15)
			  (projects . 15)
			  (registers . 5)
			  ))

  (defun remove-nth-element (nth list)
    (if (zerop nth) (cdr list)
      (let ((last (nthcdr (1- nth) list)))
	(setcdr last (cddr last))
	list)))

  (defun sej/dashboard-fix-registers nil
    "My fix to cleanse the register-alist of magit-fullscreen return info"
    (interactive)
    (let ((count (safe-length register-alist)))
      (while (>= count 0)
	(if (equal :magit-fullscreen (-first-item (nth count register-alist)))
	    (setq register-alist (remove-nth-element count register-alist))
	  )
	(setq count (- count 1))
	))
    (dashboard-refresh-buffer)
    )

  ;; To display today’s agenda items on the dashboard, add agenda to dashboard-items:

  ;; (add-to-list 'dashboard-items '(agenda) t)
  ;; To show agenda for the upcoming seven days set the variable show-week-agenda-p to t.

  ;; (setq show-week-agenda-p t)
  ;; Note that setting list-size for the agenda list is intentionally ignored; all agenda items for the current day will be displayed.

  (dashboard-setup-startup-hook)

  (setq initial-buffer-choice (lambda () (get-buffer "*dashboard*")))
  )

;; display ^L page breaks as tidy horizontal lines
(use-package page-break-lines
  :ensure t
  :config
  (setq global-page-break-lines-mode t)
  )

(provide 'init-dashboard)
;;; init-dashboard.el ends here
