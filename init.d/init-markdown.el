;;; init-markdown.el --- Initialize emacs markdown-mode
;;; Commentary:
;; for editing of markdown files
;; 2016 12 16 init SeJ
;; 2017 01 06 change from req-package to use-package
;; 2017 04 04 remove ensure went global ; defer not required for mode,bind,int

;;; Code:

(use-package markdown-mode
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


