;;; init-markdown.el --- Initialize emacs markdown-mode
;;; Commentary:
					; 2016 12 16 init SeJ

;;; Code:


(require 'req-package)

(req-package markdown-mode
  :mode
  "\\.md$"
  "\\.mdown$"
  :config 
  (add-hook 'markdown-mode-hook
	    (lambda ()
	      (visual-line-mode t)
	      (writegood-mode t)
	      (flyspell-mode t)))
  (setq markdown-command "pandoc --smart -f markdown -t html")
  :loader :el-get)

(provide 'init-markdown)\n;;; init-markdown.el ends here


