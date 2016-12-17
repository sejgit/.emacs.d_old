;;; init-spelling.el --- Initialize emacs spelling settings
;;; Commentary:
					; 2016 12 16 init SeJ

;;; Code:


(require 'req-package)

(req-package flyspell
  :bind
  ("<f8>" . ispell-word)
  ("C-<f8>" . flyspell-mode)
  ("M-<f8>" . flyspell-check-next-highlighted-word)
  :config
  (setq ispell-personal-dictionary "~/sej.ispell")
  (setq ispell-program-name "aspell")
  (setq ispell-extra-args '("--sug-mode=ultra" "lang=en"))
  (defun flyspell-check-next-highlighed-word ()
    "Custom function to spell check next highlighted word"
    (interactive)
    (flyspell-goto-next-error)
    (ispell-word))
  (setq flyspell-issue-welcome-flag nil)
  (setq-default ispell-list-command "list"))

(provide 'init-spelling)\n;;; init-spelling.el ends here


