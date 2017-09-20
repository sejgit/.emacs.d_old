;;; init-git.el --- Git related configuration

;;; Commentary:
;; git related configuration for Emacs

;;; ChangeLog:
;; 2017 05 14 init SeJ from purcell/.emacs.d
;; 2017 06 12 add font-awesome git icon
;; 2017 08 29 map to sej-mode-map & documentation & defer/ensure
;; 2017 08 30 deleted a few packages & documented the rest
;; 2017 09 18 add full screen for magit-status and return to previous on quit
;; 2017 09 20 move init-gist.el to here & delete file

;;; Code:

;; gist client
(use-package gist
  :defines sej-mode-map
  :bind
  (:map sej-mode-map
	("C-M-g" . gist-list)
	("H-g" . gist-list)))

;; git on Emacs https://github.com/magit/magit
(use-package magit
  :defer t
  :ensure t
  :defines
  sej-mode-map
  *is-a-mac*
  :bind
  (:map sej-mode-map
	([(meta f12)] . magit-status)
	("C-x g" . magit-status)
	("C-x M-g" . magit-dispatch-popup)
	:map magit-status-mode-map
	("C-M-<up>" . magit-section-up)
	("q" . magit-quit-session))
  :config
  ;; full screen magit-status

  (defadvice magit-status (around magit-fullscreen activate)
    (window-configuration-to-register :magit-fullscreen)
    ad-do-it
    (delete-other-windows))

  (defun magit-quit-session ()
    "Restores the previous window configuration and kills the magit buffer"
    (interactive)
    (kill-buffer)
    (jump-to-register :magit-fullscreen))

  (setq-default magit-diff-refine-hunk t)
  (fullframe magit-status magit-mode-quit-window)
  (add-hook 'git-commit-mode-hook 'goto-address-mode)
  (when *is-a-mac*
    (add-hook 'magit-mode-hook (lambda () (local-unset-key [(meta h)])))))

;; M-x git-blamed-mode to turn on view with commits
(use-package git-blamed
  :defer t
  :ensure t)

;; for editing gitignore files
(use-package gitignore-mode
  :defer t
  :ensure t)

;; for editing gitconfig files
(use-package gitconfig-mode
  :defer t
  :ensure t)

;; see your file over time
;; - First do M-x git-timemachine
;; Use the following keys to navigate historic version of the file

;; p Visit previous historic version
;; n Visit next historic version
;; w Copy the abbreviated hash of the current historic version
;; W Copy the full hash of the current historic version
;; g Goto nth revision
;; q Exit the time machine.
;; b Run magit-blame on the currently visited revision (if magit available).
;;
;; Do NOT call git-timemachine-mode or git-timemachine-show-previous-revision
;; or other functions directly!
(use-package git-timemachine
  :defer t
  :ensure t
  )

;; force mgit to open in one window in the current frame when called
(use-package fullframe
  :defer t
  :ensure t
  :config
  (fullframe magit-status magit-mode-quit-window))

;; popup to show commit
(use-package git-messenger
  :defer t
  :ensure t
  :bind
  ;; Though see also vc-annotate's "n" & "p" bindings
  (:map sej-mode-map
	("C-c s p" . git-messenger:popup-message)
	("s-m" . git-messenger:popup-message)))

;; easy way to clone a github M-x github-clone (respository directory)
(use-package github-clone
  :defer t
  :ensure t)

;; listing & editing issues M-x github-issues (user repo)
(use-package github-issues
  :defer t
  :ensure t)

;; for listing & managing pull requests https://github.com/sigma/magit-gh-pulls
(use-package magit-gh-pulls
  :defer t
  :ensure t)

(defun my-vc-git-mode-line-string (orig-fn &rest args)
  "Replace Git in modeline with font-awesome git icon via ORIG-FN and ARGS."
  (let ((str (apply orig-fn args)))
    (concat [#xF1D3] ":" (substring-no-properties str 4))))

(advice-add #'vc-git-mode-line-string :around #'my-vc-git-mode-line-string)


(provide 'init-git)
;;;init-git.el ends here
