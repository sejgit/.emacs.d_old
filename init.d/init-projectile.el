;;; init-projectile.el --- Projectile init

;;; Commentary:
;; projectile settings

;;; ChangeLog
;; 2017 05 14 SeJ init from purcell/.emacs.d
;; 2017 06 01 simplified & added helm-projectile
;; 2017 08 25 add settings from EOS
;; 2017 08 30 cleanup

;;; Code:

(use-package projectile
  :ensure t
  :defer 10
  :defines sej-mode-map
  :commands projectile-mode
  :functions
  multi-compile-run
  helm-projectile-multi-compile-project
  :diminish projectile-mode
  :config
  (add-hook 'after-init-hook #'projectile-mode)
  (bind-key "C-c p b" #'projectile-switch-to-buffer #'projectile-command-map)
  (bind-key "C-c p K" #'projectile-kill-buffers #'projectile-command-map)

  ;; global ignores
  (add-to-list 'projectile-globally-ignored-files ".tern-port")
  (add-to-list 'projectile-globally-ignored-files "GTAGS")
  (add-to-list 'projectile-globally-ignored-files "GPATH")
  (add-to-list 'projectile-globally-ignored-files "GRTAGS")
  (add-to-list 'projectile-globally-ignored-files "GSYMS")
  (add-to-list 'projectile-globally-ignored-files ".DS_Store")
  ;; always ignore .class files
  (add-to-list 'projectile-globally-ignored-file-suffixes ".class")
  (use-package helm-projectile
    :ensure t
    :defer 10
    :init
    (use-package helm-ag
      :ensure t)
    (use-package grep) ;; required for helm-ag to work properly
    (setq projectile-completion-system 'helm)
    ;; no fuzziness for projectile-helm
    (setq helm-projectile-fuzzy-match nil)
    (helm-projectile-on)
    :config
    ;; Add multi-compile to the mix for projects
    (defun helm-projectile-multi-compile-project (dir)
      "A Helm action to invoke multi-compile on a project.
`dir' is the project root."
      (let ((default-directory dir))
	(multi-compile-run)))

    ;; Add new projectile binding for multi-compile
    (helm-projectile-define-key helm-projectile-projects-map
      (kbd "M-m")  #'helm-projectile-multi-compile-project)))

(provide 'init-projectile)
;;; init-projectile.el ends here
