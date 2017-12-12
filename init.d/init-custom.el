;;; init-custom.el ---  place for emacs to dump custom stuff

;;; Commentary:
;; 2017 05 08 init


;;; Code:
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(bmkp-last-as-first-bookmark-file "~/.emacs.d/bookmarks")
 '(elfeed-feeds nil)
 '(midnight-mode t)
 '(neo-theme (quote nerd) t)
 '(neo-toggle-window-keep-p t t)
 '(neo-window-fixed-size t t)
 '(neo-window-width 25 t)
 '(org-agenda-files
   (quote
    ("~/gdrive/todo/journal.org" "~/gdrive/todo/inbox.org" "~/gdrive/todo/gtd.org" "~/gdrive/todo/tickler.org")))
 '(package-selected-packages
   (quote
    (abbrev esh-opt em-term em-prompt em-cmpl dired dired-aux company-dabbrev-code company-dabbrev diminish shell-pop smartparens elfeed-org elfeed-goodies elfeed neotree writegood-mode dired-subtree logview yaml-mode whole-line-or-region whitespace-cleanup-mode which-key wgrep-ag volatile-highlights vlf use-package uptimes undo-tree thesaurus textile-mode synosaurus smex smarty-mode smart-tab rpn-calc rainbow-delimiters quick-preview pyenv-mode-auto pip-requirements php-mode pass paredit org-dashboard org-bullets nov mode-icons markdown-mode magit-gh-pulls load-dir keychain-environment jedi indent-guide ido-completing-read+ highlight-numbers helpful help-fns+ helm-swoop helm-projectile helm-flycheck helm-descbinds helm-ag goto-chg google-this golden-ratio gitignore-mode github-issues github-clone gitconfig-mode git-timemachine git-messenger git-blamed gist fullframe framemove frame-cmds flycheck-pos-tip flycheck-color-mode-line flx-ido fic-mode expand-region exec-path-from-shell eshell-prompt-extras elpy elisp-slime-nav el-get dtrt-indent drag-stuff dired-sort dired-rainbow dired-open dired-narrow dired-launch dired-collapse dired+ deft dashboard cyberpunk-theme csv-nav csv-mode crux crontab-mode counsel company-statistics company-shell company-quickhelp company-jedi company-anaconda buffer-move browse-kill-ring browse-at-remote bookmark+ beginend beacon batch-mode bash-completion avy auto-compile arduino-mode all-the-icons-dired aggressive-indent ag adoc-mode))))
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
