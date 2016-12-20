;;; init.org --- Stephen's emacs init.org.el file
;;; Commentary:
					; org-mode settings
					; 2016 12 16

;;; Code:

(require 'req-package)

(req-package org
  :require writegood-mode
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
		 (global-set-key (kbd "<f1>") 'org-mode)
		 (setq org-log-done t
		       org-todo-keywords '((sequence "TODO" "INPROGRESS" "DONE"))
		       org-todo-keyword-faces '(("INPROGRESS" . (:foreground "blue" :weight bold))))
		 (setq org-capture-templates 
		       '(("t" "Todo" entry (file+headline (concat deft-directory "/gtd.org")  "Tasks") "* TODO %?\n  %i\n  %a")
			 ("j" "Journal" entry (file+datetree (concat deft-directory "/journal.org"))
			  "* %?\nEntered on %U\n  %i\n  %a")))
		 (add-hook 'org-mode-hook
			   (lambda ()
			     (flyspell-mode)))
		 (add-hook 'org-mode-hook
			   (lambda ()
			     (writegood-mode)))
                 (define-key org-mode-map (kbd "C-M-\\") 'org-indent-region)))

(req-package org-bullets
  :require org
  :commands org-bullets-mode
  :init (add-hook-exec 'org-mode (lambda () (org-bullets-mode 1))))

(req-package org-cliplink
  :require org
  :bind ("C-M-y" . org-cliplink))

(req-package org-dashboard
  :commands org-dashboard-display)

(provide 'init-org)
;;; init-org.el ends here
