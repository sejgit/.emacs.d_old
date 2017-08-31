;;; init-git.el --- Git related configuration

;;; Commentary:
;; git related configuration for Emacs

;;; ChangeLog:
;; 2017 05 14 init SeJ from purcell/.emacs.d
;; 2017 06 12 add font-awesome git icon
;; 2017 08 29 map to sej-mode-map & documentation & defer/ensure

;;; Code:

;; git on Emacs https://github.com/magit/magit
(use-package magit
  :defer t
  :ensure t
  :defines sej-mode-map *is-a-mac*
  :bind
  (:map sej-mode-map
	([(meta f12)] . magit-status)
	("C-x g" . magit-status)
	("C-x M-g" . magit-dispatch-popup)
	:map magit-status-mode-map
	("C-M-<up>" . magit-section-up))
  :defer t
  :config
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
(use-package git-timemachine
  :defer t
  :ensure t
  ;; - First do M-x git-timemachine
  ;; - Then navigate versions by hitting p and n keys
  ;;
  ;; Do NOT call git-timemachine-mode or git-timemachine-show-previous-revision
  ;; or other functions directly!
  )
(use-package fullframe
  :defer t
  :ensure t)

(use-package git-commit
  :bind
  ;; Convenient binding for vc-git-grep
  ("C-x v f" . vc-git-grep)
  :defer t
  :ensure t)

(use-package git-messenger
  :defer t
  :ensure t
  :bind
  ;; Though see also vc-annotate's "n" & "p" bindings
  ("C-x v p" . git-messenger:popup-message))

(use-package yagist
  :defer t
  :ensure t)

(use-package bug-reference-github
  :defer t
  :ensure t
  :config
  (add-hook 'prog-mode-hook 'bug-reference-prog-mode))

(use-package github-clone
  :defer t
  :ensure t)

(use-package github-issues
  :defer t
  :ensure t)

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


