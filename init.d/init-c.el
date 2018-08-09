;;; init-c.el --- c/c++ programming settings for Emacs

;;; Commentary:
;; settings from EOS

;;; Log
;; 2017 08 25 init SeJ
;; 2018 08 09 udates for Cedet

;;; Code:


(require 'cc-mode)

;; ;; Load CEDET.
;; ;; See cedet/common/cedet.info for configuration details.
;; ;; IMPORTANT: For Emacs >= 23.2, you must place this *before* any
;; ;; CEDET component (including EIEIO) gets activated by another
;; ;; package (Gnus, auth-source, ...).
;; (load-file "~/.emacs.d/vendor/cedet/cedet-devel-load.el")

;; ;; Add further minor-modes to be enabled by semantic-mode.
;; ;; See doc-string of `semantic-default-submodes' for other things
;; ;; you can use here.
(add-to-list 'semantic-default-submodes 'global-semantic-idle-summary-mode t)
(add-to-list 'semantic-default-submodes 'global-semantic-idle-completions-mode t)
(add-to-list 'semantic-default-submodes 'global-cedet-m3-minor-mode t)

;; ;; Enable Semantic
(semantic-mode 1)

;; ;; Enable EDE (Project Management) features
;;(global-ede-mode 1)


;; Flycheck supports C, so we switch it on
(add-hook 'c-mode-common-hook #'flycheck-mode)

;; always indent with 4 spaces ; in the Linuxx kernel style
(setq-default c-default-style "linux"
	      c-basic-offset 4)

;; hungry delete is useful in C ; remove up to the next non-whitespace
(setq-default c-hungry-delete-key t)

(provide 'init-c)
;;; init-c.el ends here
