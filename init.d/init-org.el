;;; init.org --- Stephen's emacs init.org.el file
;;; Commentary:
;; org-mode settings
;; 2016 12 16
;; 2017 01 06 change from req-package to use-package

;;; Code:

(use-package org
  :defines
  org-capture-bookmark
  org-capture-templates
  org-agenda-window-setup
  org-agenda-span
  org-agenda-skip-scheduled-if-deadline-is-shown
  org-agenda-todo-ignore-deadlines
  org-agenda-todo-ignore-scheduled
  org-agenda-sorting-strategy
  org-agenda-skip-deadline-prewarning-if-scheduled
  :mode ("\\.org$" . org-mode)
  :bind (("<f1>" . org-mode)
	 ("C-c l" . org-store-link)
         ("C-c c" . org-capture)
         ("C-c a" . org-agenda)
         ("C-'" . org-cycle-agenda-files)
         ("C-c b" . org-iswitchb))
  :config (progn (setq org-default-notes-file (concat org-directory "/notes.org"))
                 (setq org-tags-column -110)
		 (setq org-capture-bookmark t)
                 (setq org-refile-use-outline-path 'file)
                 (setq org-startup-folded 'showeverything)
                 (setq org-log-done 'note)
		 (setq org-tags-column 75)
		 (global-set-key (kbd "<f1>") 'org-mode)
		 (setq org-log-done t
		       org-todo-keywords '((sequence "TODO(t)" "INPROGRESS(i)" "|" "DONE(d)")
					   (sequence "DELIGATE(D)" "CHECK(C)" "|" "VERIFIED(V)")
					   (sequence "|" "CANCELED(x)"))
		       org-todo-keyword-faces '(("TODO" . org-warning)
						("INPROGRESS" . (:foreground "blue" :weight bold))
						("DONE" . (:foreground "green" :weight bold))
						("DELIGATE" . (:foreground "blue" :weight bold))
						("VERIFIED" . (:foreground "green" :weight bold))
						("CANCELED" .(:foreground "grey" :weight bold))))
		 (setq org-capture-templates
		       '(("t" "Todo" entry (file+headline (concat deft-directory "/gtd.org")  "Tasks") "* TODO [#A] %?\nSCHEDULED: %(org-insert-time-stamp (org-read-date nil t \"+0d\"))\n%a\n")
			 ("j" "Journal" entry (file+datetree (concat deft-directory "/journal.org"))
			  "* %?\nEntered on %U\n  %i\n  %a")))
		 (add-hook 'org-mode-hook
			   (lambda ()
			     (flyspell-mode)))
		 (add-hook 'org-mode-hook
			   (lambda ()
			     (writegood-mode)))
		 (define-key org-mode-map (kbd "C-M-\\") 'org-indent-region)
		 ;; org-mode agenda options
		 ;;open agenda in current window
		 (setq org-agenda-window-setup (quote current-window))
		 ;;warn me of any deadlines in next 7 days
		 (setq org-deadline-warning-days 7)
		 ;;show me tasks scheduled or due in next fortnight
		 (setq org-agenda-span (quote fortnight))
		 ;;don't show tasks as scheduled if they are already shown as a deadline
		 (setq org-agenda-skip-scheduled-if-deadline-is-shown t)
		 ;;don't give awarning colour to tasks with impending deadlines
		 ;;if they are scheduled to be done
		 (setq org-agenda-skip-deadline-prewarning-if-scheduled (quote pre-scheduled))
		 ;;don't show tasks that are scheduled or have deadlines in the
		 ;;normal todo list
		 (setq org-agenda-todo-ignore-deadlines (quote all))
		 (setq org-agenda-todo-ignore-scheduled (quote all))
		 ;;sort tasks in order of when they are due and then by priority
		 (setq org-agenda-sorting-strategy
		       (quote
			((agenda deadline-up priority-down)
			 (todo priority-down category-keep)
			 (tags priority-down category-keep)
			 (search category-keep))))
		 ))

(use-package org-bullets
  :ensure t
  :commands org-bullets-mode
  :config (org-bullets-mode 1))

(use-package org-cliplink
  :ensure t
  :bind ("C-M-y" . org-cliplink))

(use-package org-dashboard
  :ensure t
  :commands org-dashboard-display)

(provide 'init-org)
;;; init-org.el ends here
