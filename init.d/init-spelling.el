;;; init-spelling.el --- Initialize emacs spelling settings
;;; Commentary:
;; 2016 12 16 init SeJ
;; 2017 01 06 change from req-package to use-package

;;; Code:


(use-package flyspell
  :ensure t
  :bind
  (("<f8>" . ispell-word)
   ("C-<f8>" . flyspell-mode)
   ("M-<f8>" . flyspell-check-next-highlighted-word))
  :init
  (add-hook 'text-mode-hook 'flyspell-mode)
  :config
  (setq ispell-personal-dictionary "~/sej.ispell")
  (if (string-equal system-type "windows-nt")
      (setq ispell-program-name "hunspell.exe")
    (progn (setq ispell-program-name "aspell")
	   (setq ispell-extra-args '("--sug-mode=ultra" "lang=en"))))
  (setq ispell-local-dictionary "en_US")
  (defun flyspell-check-next-highlighed-word ()
    "Custom function to spell check next highlighted word"
    (interactive)
    (flyspell-goto-next-error)
    (ispell-word))
  (setq flyspell-issue-welcome-flag nil)
  (setq-default ispell-list-command "list"))

(provide 'init-spelling)
;;; init-spelling.el ends here
