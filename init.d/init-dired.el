;;; init-dired.el --- Initialize emacs dired mode
;;; Commentary:
;; dired mode related packages and settings for Emacs

;;; ChangeLog
;; 2016 12 16 init SeJ
;; 2017 01 06 change from req-package to use-package
;; 2017 01 11 add dired-narrow :: filter dired screen
;; 2017 04 04 remove ensure went global ; defer not required for mode,bind,int
;; 2017 05 12 updates from purcell/emacs.d
;; 2017 08 29 cleanup title block and use-package settings

;;; Code:

(use-package dired
  :ensure nil
  :defer t
  :functions sej/dired-truncate-lines sej/dired-rename-buffer-name
  :commands (dired-toggle-read-only ; to toggle read-only state of any buffer
             dired-get-filename) ; called by `dired-single'
  :config
  (setq dired-recursive-deletes 'always)
  (setq dired-recursive-copies  'always)
  ;; Set this variable to non-nil, Dired will try to guess a default
  ;; target directory. This means: if there is a dired buffer
  ;; displayed in the next window, use its current subdir, instead
  ;; of the current subdir of this dired buffer. The target is used
  ;; in the prompt for file copy, rename etc.
  (setq dired-dwim-target t)

  ;; Dired listing switches
  ;;  -a : Do not ignore entries starting with .
  ;;  -l : Use long listing format.
  ;;  -G : Do not print group names like 'users'
  ;;  -h : Human-readable sizes like 1K, 234M, ..
  ;;  -v : Do natural sort .. so the file names starting with . will show up first.
  ;;  -F : Classify filenames by appending '*' to executables,
  ;;       '/' to directories, etc.
  (setq dired-listing-switches "-alGhvF --group-directories-first") ; default: "-al"

  (defun sej/dired-rename-buffer-name ()
    "Rename the dired buffer name to distinguish it from file buffers.
It added extra strings at the front and back of the default dired buffer name."
    (let ((name (buffer-name)))
      (if (not (string-match "/$" name))
	  (rename-buffer (concat "*Dired* " name "/") t))))

  (defun sej/dired-truncate-lines ()
    (toggle-truncate-lines 1))

  (add-hook 'dired-mode-hook #'sej/dired-rename-buffer-name)
  (add-hook 'dired-mode-hook #'sej/dired-truncate-lines)

  (setq dired-omit-verbose nil)
  ;; hide backup, autosave, *.*~ files
  ;; omit mode can be toggled using `M-o' in dired buffer
  (add-hook 'dired-mode-hook #'dired-omit-mode)

  (use-package dired+
    :init
    ;; Details toggling is bound to "(" in `dired-mode' by default
    (setq diredp-hide-details-initially-flag nil)
    :config
    ;; Privilege indicator faces
    (defun sej/dired-update-privilege-faces ()
      (set-face-attribute 'diredp-dir-priv nil
			  :foreground "#7474FFFFFFFF"
			  :background (face-background 'default))
      (set-face-attribute 'diredp-exec-priv nil
			  :foreground "dodger blue"
			  :background (face-background 'default))
      (set-face-attribute 'diredp-other-priv nil
			  :background (face-background 'default))
      (set-face-attribute 'diredp-write-priv nil
			  :foreground "#25258F8F2929"
			  :background (face-background 'default))
      (set-face-attribute 'diredp-read-priv nil
			  :foreground "#999932325555"
			  :background (face-background 'default))
      (set-face-attribute 'diredp-no-priv nil
			  :foreground "#2C2C2C2C2C2C"
			  :background (face-background 'default))
      (set-face-attribute 'diredp-rare-priv nil
			  :foreground "Green"
			  :background (face-background 'default))
      (set-face-attribute 'diredp-link-priv nil
			  :foreground "#00007373FFFF"))
    (add-hook 'dired-mode-hook #'sej/dired-update-privilege-faces)

    ;; https://fuco1.github.io/2017-07-15-Collapse-unique-nested-paths-in-dired-with-dired-collapse-mode.html
    ;; https://github.com/Fuco1/dired-hacks/blob/master/dired-collapse.el
    (use-package dired-collapse
      :config
      (progn
	(add-hook 'dired-mode-hook #'dired-collapse-mode)))

    (use-package browse-at-remote)

    ;; dired rainbow
    (use-package dired-rainbow
      :ensure t)

    ;; dired open
    (use-package dired-open
      :ensure t)

    (use-package dired-launch
      :ensure t
      :config (dired-launch-enable))

    ;;narrow dired to match filter
    (use-package dired-narrow
      :ensure t 
      :bind (:map dired-mode-map
		  ("/" . dired-narrow)))

    (use-package dired-sort)
    ))


;; TIPS

;; (1) Jump to the dired of the current file
;;     C-x C-j - Calls `dired-jump' function.
;;     Jump to dired buffer corresponding to current buffer.
;;     If in a file, dired the current directory and move to file's line.
;;     If in Dired already, pop up a level and goto old directory's line.
;;     In case the proper dired file line cannot be found, refresh the dired
;;     buffer and try again.

;; https://peterreavy.wordpress.com/2011/05/04/emacs-dired-tips/
;; (2) To copy the name of the file at point, in order to make use of
;;     it elsewhere, use `dired-copy-filename-as-kill', which is bound to
;;     `w'. To make it copy the absolute path: `0 w'

;; (3) To copy the path to the folder you’re looking at in dired: `M-< w'

;; (4) Enable wdired mode in dired to edit the file names by hitting C-x C-q
;;     which is bound to `dired-toggle-read-only' by default. That's a wrapper
;;     function which calls `wdired-change-to-wdired-mode' in `dired-mode'.

;; http://truongtx.me/2013/04/24/dired-as-default-file-manager-1-introduction



;; Prefer g-prefixed coreutils version of standard utilities when available
(let ((gls (executable-find "gls")))
  (when gls (setq insert-directory-program gls)))

(provide 'init-dired)
;;; init-dired.el ends here






