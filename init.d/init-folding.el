;;; init-folding.el --- Stephen's emacs init-folding.el
;;; Commentary:
;; uses origami to achieve folding

;; 2017 05 14 init SeJ

;;; Code:

(use-package origami
  :config
  (add-hook 'prog-mode-hook 'origami-mode)
  (define-key origami-mode-map (kbd "C-c f") 'origami-recursively-toggle-node)
  (define-key origami-mode-map (kbd "C-c F") 'origami-toggle-all-nodes))


(provide 'init-folding)

;;; init-folding.el ends here
