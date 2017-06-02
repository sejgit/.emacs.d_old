;;; init-spelling.el --- Initialize emacs spelling settings
;;; Commentary:
;; 2016 12 16 init SeJ
;; 2017 01 06 change from req-package to use-package
;; 2017 01 15 add support for thesaurus.el
;; 2017 04 04 remove ensure went global ; defer not required for mode,bind,int

;;; Code:


(use-package flyspell
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
    (progn (setq ispell-program-name "/usr/local/bin/aspell")
	   ))
  ;;	   (setq ispell-extra-args '("--sug-mode=ultra" "lang=en_GB-ise"))))
  ;; (setq ispell-local-dictionary "en_GB-ise")
  (setq ispell-dictionary-alist '(("british" "[[:alpha:]]" "[^[:alpha:]]" "[']" nil  ("-d" "en_GB-ise") nil utf-8)
				  ("canadian" "[[:alpha:]]" "[^[:alpha:]]" "[']" nil  ("-d" "en_CA") nil utf-8)
				  ("american" "[[:alpha:]]" "[^[:alpha:]]" "[']" t ("-d" "en_US") nil utf-8)))

  (defun flyspell-check-next-highlighed-word ()
    "Custom function to spell check next highlighted word"
    (interactive)
    (flyspell-goto-next-error)
    (ispell-word))
  (setq flyspell-issue-welcome-flag nil)
  (setq-default ispell-list-command "list"))

(use-package thesaurus
  :bind ("C-x t" . thesaurus-choose-synonym-and-replace)
  :config
  (thesaurus-set-bhl-api-key-from-file "~/.ssh/BigHugeLabs.apikey.txt"))


(provide 'init-spelling)
;;; init-spelling.el ends here
