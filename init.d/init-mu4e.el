;;; init-mu4e.el --- initiation for mu43 elmail package

;;; Commentary:
;; 2017 01 14 init sej
;; 2017 01 15 adding pragmatic Emacs hints
;; 2017 04 04 remove ensure went global ; defer not required for mode,bind,int

;;; Code:
(when (require 'mu4e nil t)
  (use-package mu4e
    :defer t
    :defines
    starttls-use-gnutls
    smtpmail-starttls-credentials
    smtpmail-auth-credentials
    :config
    (require 'smtpmail)
    (setq mu4e-get-mail-command "offlineimap")
    ;; don't save message to Sent Messages, Gmail/IMAP takes care of this
    (setq mu4e-sent-messages-behavior 'delete
	  message-send-mail-function 'smtpmail-send-it
	  starttls-use-gnutls t
	  message-kill-buffer-on-exit t)

    ;; something about ourselves
    (setq
     user-full-name  "Stephen E. Jenkins"
     mu4e-compose-signature
     (concat
      "Stephen E. Jenkins\n"
      "http://sej.dyndns.info\n"))
    (setq mu4e-maildir "~/Maildir")

    ;;default
    (setq mu4e-sent-folder "/Gmail/[Gmail].Sent Mail"
	  mu4e-drafts-folder "/Gmail/[Gmail].Drafts"
	  mu4e-trash-folder  "/Gmail/[Gmail].Trash"
	  user-mail-address "stephenearljenkins@gmail.com"
	  smtpmail-stream-type 'starttls
	  smtpmail-starttls-credentials '(("smtp.gmail.com" 587 nil nil))
	  smtpmail-auth-credentials
	  '(("smtp.gmail.com" 587 "stephenearljenkins@gmail.com" nil))
	  smtpmail-default-smtp-server "smtp.gmail.com"
	  smtpmail-smtp-server "smtp.gmail.com"
	  smtpmail-smtp-service 587
	  mu4e-maildir-shortcuts
	  '( ("/Gmail/INBOX"               . ?i)
	     ("/Gmail/INBOX"               . ?g)
	     ("/Hotmail/INBOX"             . ?h)
	     ("/Gmail/[Gmail].Sent Mail"   . ?s)
	     ("/Gmail/[Gmail].Trash"       . ?t)
	     ("/Gmail/archive"             . ?r)
	     ("/Gmail/[Gmail].All Mail"    . ?a)))

    (defvar my-mu4e-account-alist
      '(("Gmail"
	 mu4e-sent-folder "/Gmail/[Gmail].Sent Mail"
	 mu4e-drafts-folder "/Gmail/[Gmail].Drafts"
	 mu4e-trash-folder  "/Gmail/[Gmail].Trash"
	 user-mail-address "stephenearljenkins@gmail.com"
	 smtpmail-stream-type 'starttls
	 smtpmail-starttls-credentials '(("smtp.gmail.com" 587 nil nil))
	 smtpmail-auth-credentials
	 '(("smtp.gmail.com" 587 "stephenearljenkins@gmail.com" nil))
	 smtpmail-default-smtp-server "smtp.gmail.com"
	 smtpmail-smtp-server "smtp.gmail.com"
	 smtpmail-smtp-service 587
	 mu4e-maildir-shortcuts
	 '( ("/Gmail/INBOX"               . ?i)
	    ("/Gmail/INBOX"               . ?g)
	    ("/Hotmail/INBOX"             . ?h)
	    ("/Gmail/[Gmail].Sent Mail"   . ?s)
	    ("/Gmail/[Gmail].Trash"       . ?t)
	    ("/Gmail/archive"             . ?r)
	    ("/Gmail/[Gmail].All Mail"    . ?a)))
	("Hotmail"
	 mu4e-sent-folder "/Hotmail/[Hotmail].Sent Mail"
	 mu4e-drafts-folder "/Hotmail/[Hotmail].Drafts"
	 mu4e-trash-folder  "/Hotmail/[Hotmail].Trash"
	 user-mail-address "stephenearljenkins@hotmail.com"
	 smtpmail-stream-type 'starttls
	 smtpmail-starttls-credentials '(("smtp-mail.outlook.com" 587 nil nil))
	 smtpmail-auth-credentials
	 '(("smtp-mail.outlook.com" 587 "stephenearljenkins@hotmail.com" nil))
	 smtpmail-default-smtp-server "smtp-mail.outlook.com"
	 smtpmail-smtp-server "smtp-mail.outlook.com"
	 smtpmail-smtp-service 587
	 mu4e-maildir-shortcuts
	 '( ("/Hotmail/INBOX"                 . ?i)
	    ("/Gmail/INBOX"                   . ?g)
	    ("/Hotmail/INBOX"                 . ?h)
	    ("/Hotmail/[Hotmail].Sent Mail"   . ?s)
	    ("/Hotmail/[Hotmail].Trash"       . ?t)
	    ("/Hotmail/archive"               . ?r)
	    ("/Hotmail/[Hotmail].All Mail"    . ?a)))))


    (defun my-mu4e-set-account ()
      "Set the account for composing a message."
      (interactive)
      (let* ((account
	      (if mu4e-compose-parent-message
		  (let ((maildir (mu4e-message-field mu4e-compose-parent-message :maildir)))
		    (string-match "/\\(.*?\\)/" maildir)
		    (match-string 1 maildir))
		(completing-read (format "Compose with account: (%s) "
					 (mapconcat #'(lambda (var) (car var))
						    my-mu4e-account-alist "/"))
				 (mapcar #'(lambda (var) (car var))  my-mu4e-account-alist)
				 nil t nil nil (caar my-mu4e-account-alist))))
	     (account-vars (cdr (assoc account my-mu4e-account-alist))))
	(if account-vars
	    (mapc #'(lambda (var)
		      (set (car var) (cadr var)))
		  account-vars)
	  (error "No email account found"))))

    (add-hook 'mu4e-compose-pre-hook 'my-mu4e-set-account)


    (set-face-attribute 'font-lock-warning-face nil :foreground "red" :weight 'bold :background "yellow")
    (add-hook 'mu4e-compose-mode-hook
	      (defun bjm/mu4e-highlight-attachment ()
		"Flag attachment keywords"
		(font-lock-add-keywords nil
					'(("\\(attach\\|pdf\\|file\\)" 1 font-lock-warning-face t)))))

    ;; use 'fancy' non-ascii characters in various places in mu4e
    (setq mu4e-use-fancy-chars t)

    ;; save attachment to my desktop (this can also be a function)
    (setq mu4e-attachment-dir "~/Desktop")

    ;; attempt to show images when viewing messages
    (setq mu4e-view-show-images t)
    ;; use imagemagick, if available
    (when (fboundp 'imagemagick-register-types)
      (imagemagick-register-types))

    (use-package org-mu4e
      :defines
      org-mu4e-link-query-in-headers-mode
      :config (setq org-mu4e-link-query-in-headers-mode nil))

    ))


;;; init-mu4e.el ends here

