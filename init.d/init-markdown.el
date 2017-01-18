;;; init-markdown.el --- Initialize emacs markdown-mode
;;; Commentary: for editing of markdown files
;; 2016 12 16 init SeJ
;; 2017 01 06 change from req-package to use-package

;;; Code:

(use-package markdown-mode
  :defer t
  :ensure t
  :mode
  "\\.md$"
  "\\.mdown$"
  :config
  (add-hook 'markdown-mode-hook
	    (lambda ()
	      (visual-line-mode t)
	      (writegood-mode t)
	      (flyspell-mode t)))
  (setq markdown-command "pandoc --smart -f markdown -t html"))

(provide 'init-markdown)
;;; init-markdown.el ends here


