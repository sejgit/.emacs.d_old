;;; init-custom.el ---  place for emacs to dump custom stuff

;;; Commentary:
;; 2017 05 08 init


;;; Code:
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(flycheck-error ((((class color)) (:underline "Red"))))
 '(flycheck-warning ((((class color)) (:underline "Orange"))))
 '(rainbow-delimiters-depth-1-face ((t (:foreground "red" :height 1.0))))
 '(rainbow-delimiters-depth-2-face ((t (:foreground "orange" :height 1.0))))
 '(rainbow-delimiters-depth-3-face ((t (:foreground "yellow" :height 1.0))))
 '(rainbow-delimiters-depth-4-face ((t (:foreground "green" :height 1.0))))
 '(rainbow-delimiters-depth-5-face ((t (:foreground "blue" :height 1.0))))
 '(rainbow-delimiters-depth-6-face ((t (:foreground "violet" :height 1.0))))
 '(rainbow-delimiters-depth-7-face ((t (:foreground "purple" :height 1.0))))
 '(rainbow-delimiters-depth-8-face ((t (:foreground "black" :height 1.0))))
 '(rainbow-delimiters-unmatched-face ((t (:background "cyan" :height 1.0)))))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(bmkp-last-as-first-bookmark-file "~/.emacs.d/bookmarks")
 '(midnight-mode t)
 '(package-selected-packages
   (quote
    (fic-mode adoc-mode markdown-mode whitespace-cleanup-mode indent-guide which-key pass synosaurus thesaurus eshell-prompt-extras keychain-environment with-editor exec-path-from-shell jedi pyenv-mode-auto pyvenv company-anaconda anaconda-mode pip-requirements projectile helm-projectile helm-ag org-dashboard org-bullets mode-icons whole-line-or-region helpful help-fns+ wgrep-ag ag wgrep rpn-calc rainbow-delimiters volatile-highlights crux google-this avy drag-stuff beacon beginend vlf expand-region undo-tree dtrt-indent highlight-numbers yaml-mode textile-mode php-mode smarty-mode nov csv-nav csv-mode crontab-mode batch-mode arduino-mode elisp-slime-nav paredit flx-ido goto-chg golden-ratio magit-gh-pulls github-issues github-clone git-messenger fullframe git-timemachine gitconfig-mode gitignore-mode git-blamed magit gist frame-cmds helm-flycheck flycheck-pos-tip flycheck-color-mode-line dired+ all-the-icons-dired all-the-icons quick-preview dired-narrow dired-sort dired-launch dired-open dired-rainbow browse-at-remote dired-collapse deft buffer-move framemove company-shell bash-completion auto-complete aggressive-indent el-get helm-descbinds helm-swoop smart-tab company-jedi company-statistics company-quickhelp uptimes use-package load-dir dashboard cyberpunk-theme browse-kill-ring auto-compile))))
