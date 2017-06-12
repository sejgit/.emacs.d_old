;;; init-git.el --- Company-mode configuration

;;; Commentary:
;;2017 05 14 init SeJ from purcell/.emacs.d
;;2017 06 12 add font-awesome git icon
;;; Code:



(use-package git-blamed)
(use-package gitignore-mode)
(use-package gitconfig-mode)
(use-package git-timemachine)


(use-package magit
  :bind
  ;; Hint: customize `magit-repo-dirs' so that you can use C-u M-F12 to
  ;; quickly open magit on any one of your projects.
  (([(meta f12)] . magit-status)
   ("C-x g" . magit-status)
   ("C-x M-g" . magit-dispatch-popup))
  :defer t
  :config
  (setq-default magit-diff-refine-hunk t)
  (define-key magit-status-mode-map (kbd "C-M-<up>") 'magit-section-up)
  (fullframe magit-status magit-mode-quit-window)
  (add-hook 'git-commit-mode-hook 'goto-address-mode)
  (when *is-a-mac*
    (add-hook 'magit-mode-hook (lambda () (local-unset-key [(meta h)]))))


  )

(use-package fullframe)

(use-package git-commit
  :bind
  ;; Convenient binding for vc-git-grep
  ("C-x v f" . vc-git-grep))

(use-package git-messenger
  :bind
  ;; Though see also vc-annotate's "n" & "p" bindings
  ("C-x v p" . git-messenger:popup-message))

(use-package yagist)
(use-package bug-reference-github
  :config
  (add-hook 'prog-mode-hook 'bug-reference-prog-mode))

(use-package github-clone)
(use-package github-issues)
(use-package magit-gh-pulls)

(defun my-vc-git-mode-line-string (orig-fn &rest args)
  "Replace Git in modeline with font-awesome git icon via ORIG-FN and ARGS."
  (let ((str (apply orig-fn args)))
    (concat [#xF1D3] ":" (substring-no-properties str 4))))

(advice-add #'vc-git-mode-line-string :around #'my-vc-git-mode-line-string)


(provide 'init-git)
;;;init-git.el ends here


