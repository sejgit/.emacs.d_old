;;; init-view.el --- init file for pdf and view packages

;;; Commentary:
;; some packages and settings for viewing files and pdf specifically

;;; ChangeLog
;; 2017 08 23 init SeJ

;;; Code:

(use-package view
  :ensure nil
  :config
  (progn
    (defun View-goto-line-last (&optional line)
      "goto last line"
      (interactive "P")
      (forward-line (line-number-at-pos (point-max))))

    (define-key view-mode-map (kbd "e") 'View-scroll-half-page-forward)
    (define-key view-mode-map (kbd "u") 'View-scroll-half-page-backward)

    ;; less like
    (define-key view-mode-map (kbd "N") 'View-search-last-regexp-backward)
    (define-key view-mode-map (kbd "?") 'View-search-regexp-backward?)
    (define-key view-mode-map (kbd "g") 'View-goto-line)
    (define-key view-mode-map (kbd "G") 'View-goto-line-last)
    ;; vi/w3m like
    (define-key view-mode-map (kbd "h") 'backward-char)
    (define-key view-mode-map (kbd "j") 'next-line)
    (define-key view-mode-map (kbd "k") 'previous-line)
    (define-key view-mode-map (kbd "l") 'forward-char)))

(use-package doc-view
  :ensure nil
  :config
  (define-key doc-view-mode-map (kbd "j")
    #'doc-view-next-line-or-next-page)
  (define-key doc-view-mode-map (kbd "k")
    #'doc-view-previous-line-or-previous-page)
  ;; use 'q' to kill the buffer, not just hide it
  (define-key doc-view-mode-map (kbd "q")
    #'kill-this-buffer))

(provide 'init-view)
;;; init-view.el ends here
