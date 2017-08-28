;;; init-helm.el --- init helm
;;; Commentary:

;;; Log:
;; 2017 05 26 init SeJ
;; 2017 08 25 add helm items from EOS

;;; Code:

(use-package helm-config
  :ensure helm
  :demand t ;; demand it be loaded!
  :diminish helm-mode
  :bind
  (("C-M-z" . helm-resume)
   ("C-x C-f" . helm-find-files)
   ("C-x C-r" . helm-mini)
   ("C-x o" . helm-occur)
   ("M-y" . helm-show-kill-ring)
   ("C-h a" . helm-apropos)
   ;;   ("C-h m" . helm-man-woman)
   ("C-h SPC" . helm-all-mark-rings)
   ("C-x C-i" . helm-semantic-or-imenu)
   ("M-x" . helm-M-x)
   ("C-x C-b" . helm-buffers-list)
   ("C-x C-r" . helm-mini)
   ("C-x b" . helm-mini)
   ("C-h t" . helm-world-time)))

(use-package helm-swoop
  :ensure t
  :bind (("M-i" . helm-swoop)
         ("M-I" . helm-swoop-back-to-last-point)
         ("C-c M-i" . helm-multi-swoop))
  :config
  ;; When doing isearch, hand the word over to helm-swoop
  (define-key isearch-mode-map (kbd "M-i") 'helm-swoop-from-isearch)
  ;; From helm-swoop to helm-multi-swoop-all
  (define-key helm-swoop-map (kbd "M-i") 'helm-multi-swoop-all-from-helm-swoop)
  ;; Save buffer when helm-multi-swoop-edit complete
  (setq helm-multi-swoop-edit-save t
        ;; If this value is t, split window inside the current window
        helm-swoop-split-with-multiple-windows t
        ;; Split direcion. 'split-window-vertically or 'split-window-horizontally
        helm-swoop-split-direction 'split-window-vertically
        ;; don't auto select the thing at point
        helm-swoop-pre-input-function (lambda () "")
        ;; If nil, you can slightly boost invoke speed in exchange for text
        ;; color. If I want pretty I'll use helm-occur since it keeps colors
        helm-swoop-speed-or-color nil))

(use-package helm-descbinds
  :ensure t
  :bind ("C-h b" . helm-descbinds)
  :init (fset 'describe-bindings 'helm-descbinds))


(provide 'init-helm)
;;; init-helm.el ends here

