;;; init-python.el ---  Stephen's emacs init-python.el
;;; Commentary:
;; 2017 03 29 SeJ init
;; 2017 04 04 set python-interpreter
;; 2017 04 04 remove ensure went global ; defer not required for mode,bind,int
;; 2017 05 14 adds from purcell/.emacs.d
;; 2017 05 19 add mastering Emacs python debugging with compile

;;; Code:

(use-package pip-requirements)
(use-package python-environment)
(use-package ediff
  :defer t
  :config
  (setq ediff-shell (getenv "$SHELL"))
  (setq-default ediff-split-window-function
		(quote split-window-vertically)))

(use-package python
  :interpreter "python"
  :bind (("<kp-5>" . py-insert-debug)
         ("<f9>" . py-insert-debug))
  :mode (("\\.py$" . python-mode)
         ("\\.cpy$" . python-mode)
         ("\\.vpy$" . python-mode))
  :init
  (setq python-shell-interpreter "ipython3 --simple-prompt -i"
	python-shell-interpreter-args "--simple-prompt -i")
  :config
  (use-package anaconda-mode
    :config
    (add-hook 'python-mode-hook 'anaconda-mode)
    (add-hook 'python-mode-hook 'anaconda-eldoc-mode))
  (use-package company-anaconda)
  (declare-function py-insert-debug netsight nil)
  (setq fill-column 79)
  (setq-default flycheck-flake8rc "~/.config/flake8rc")
  (setq python-check-command "flake8")
  (setq tab-width 4))

(use-package pyvenv
  :defer t)
(use-package pyenv-mode-auto
  :defer t)

(use-package jedi
  :init
  (autoload 'jedi:setup "jedi" nil t)
  (add-hook 'python-mode-hook 'jedi:setup)
  (setq jedi:complete-on-dot t)
  :preface
  (declare-function jedi:goto-definition jedi nil)
  (declare-function jedi:related-names jedi nil)
  (declare-function jedi:show-doc jedi nil)
  :bind (("C-." . jedi:goto-definition)
	 ("C-c r" . jedi:related-names)
	 ("C-?" . jedi:show-doc)))

(require 'python)
(defun python--add-debug-highlight ()
  "Add a highlighter for use by `python--pdb-breakpoint-string'."
  (highlight-lines-matching-regexp "## DEBUG ##\\s-*$" 'hi-red-b))

(add-hook 'python-mode-hook 'python--add-debug-highlight)

(defvar python--pdb-breakpoint-string "import pdb; pdb.set_trace() ## DEBUG ##"
  "Python breakpoint string used by `python-insert-breakpoint'.")

(defun python-insert-breakpoint ()
  "Insert a python breakpoint using `pdb'."
  (interactive)
  (back-to-indentation)
  ;; this preserves the correct indentation in case the line above
  ;; point is a nested block
  (split-line)
  (insert python--pdb-breakpoint-string))
(define-key python-mode-map (kbd "<f6>") 'python-insert-breakpoint)

(defadvice compile (before ad-compile-smart activate)
  "Advises `compile' so it will set the argument COMINT to t if breakpoints are present in `python-mode' files."
  (when (derived-mode-p major-mode 'python-mode)
    (save-excursion
      (save-match-data
        (goto-char (point-min))
        (if (re-search-forward (concat "^\\s-*" python--pdb-breakpoint-string "$")
                               (point-max) t)
            ;; set COMINT argument to `t'.
            (ad-set-arg 1 t))))))

(defvar python-last-buffer nil
  "Name of the Python buffer that last invoked `toggle-between-python-buffers'.")

(make-variable-buffer-local 'python-last-buffer)

(defun toggle-between-python-buffers ()
  "Toggle between a `python-mode' buffer and its inferior Python process.
When invoked from a `python-mode' buffer it will switch the
active buffer to its associated Python process.  If the command is
invoked from a Python process, it will switch back to the `python-mode' buffer."
  (interactive)
  ;; check if `major-mode' is `python-mode' and if it is, we check if
  ;; the process referenced in `python-buffer' is running
  (if (and (eq major-mode 'python-mode)
           (processp (get-buffer-process python-shell-internal-buffer)))
      (progn
        ;; store a reference to the current *other* buffer; relying
        ;; on `other-buffer' alone wouldn't be wise as it would never work
        ;; if a user were to switch away from the inferior Python
        ;; process to a buffer that isn't our current one.
        (switch-to-buffer python-shell-internal-buffer)
        (setq python-last-buffer (other-buffer)))
    ;; switch back to the last `python-mode' buffer, but only if it
    ;; still exists.
    (when (eq major-mode 'inferior-python-mode)
      (if (buffer-live-p python-last-buffer)
	  (switch-to-buffer python-last-buffer)
        ;; buffer's dead; clear the variable.
        (setq python-last-buffer nil)))))

(define-key inferior-python-mode-map (kbd "<f12>") 'toggle-between-python-buffers)
(define-key python-mode-map (kbd "<f12>") 'toggle-between-python-buffers)


(provide 'init-python)
;;; init-python.el ends here
