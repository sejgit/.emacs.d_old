;;; init-which-key.el --- Initialize emacs which-key

;;; Commentary:
;; pop-up which displays the keybindings following
;; your curently entered incomplete command

;;; ChangeLog
;; 2016 12 19 init SeJ
;; 2017 01 06 change from req-package to use-package
;; 2017 04 04 remove ensure went global ; defer not required for mode,bind,int
;; 2018 09 01 minor doc clean-up and ensure

;;; Code:

(use-package which-key
  :ensure t
  :defer 5
  :config (which-key-mode))

(provide 'init-which-key)
;;; init-which-key.el ends here
