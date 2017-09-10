;;; init-helm.el --- init helm
;;; Commentary:
;; helm frameword settings for Emacs

;;; ChangeLog:
;; 2017 05 26 init SeJ
;; 2017 08 25 add helm items from EOS
;; 2017 08 30 map to sej-mode-map & trim bindings

;;; Code:

(use-package helm
  :ensure nil
  :demand
  :defines sej-mode-map
  :diminish helm-mode
  :bind
  (:map sej-mode-map
	("C-M-z" . helm-resume)
	("C-x C-f" . helm-find-files)
	("C-x C-r" . helm-mini)
	("C-x o" . helm-occur)
	("M-y" . helm-show-kill-ring)
	("C-h a" . helm-apropos)
	;;   ("C-h m" . helm-man-woman)
	("C-h SPC" . helm-all-mark-rings)
	;;("M-x" . helm-M-x)
	("C-x C-b" . helm-buffers-list)
	("C-x b" . helm-mini)))

(use-package helm-swoop
  :ensure t
  :defer t
  :defines sej-mode-map
  :bind (:map sej-mode-map
	      ("M-i" . helm-swoop)
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
  :defer t
  :defines sej-mode-map
  :bind (:map sej-mode-map
	      ("C-h b" . helm-descbinds))
  :init (fset 'describe-bindings 'helm-descbinds))


  (provide 'init-helm)
;;; init-helm.el ends here

