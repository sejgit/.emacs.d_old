;;; init-misc-defuns.el --- Some Utility functions

;;; Commentary:
;; some functions to add value to my Emacs

;;; ChangeLog
;; 2017 05 17 init SeJ from purcell/.emacs.d
;; 2017 08 29 add copy-from-osx & paste-to-osx
;; 2017 09 08 fixed above for only mac
;; 2017 09 20 deleted unused defuns and renamed to init-misc-defuns.el
;;            move from init-bindings-settings.el

;;; Code:

;; from https://gist.github.com/the-kenny/267162
(defun copy-from-osx ()
  (shell-command-to-string "pbpaste"))

(defun paste-to-osx (text &optional push)
  (let ((process-connection-type nil))
    (let ((proc (start-process "pbcopy" "*Messages*" "pbcopy")))
      (process-send-string proc text)
      (process-send-eof proc))))

(when *is-a-mac*
(setq interprogram-cut-function 'paste-to-osx)
(setq interprogram-paste-function 'copy-from-osx))

;; as name suggests ; defined as C-c b in above keymappings
(defun create-scratch-buffer nil
  "create a new scratch buffer to work in. (could be *scratch* - *scratchX*)"
  (interactive)
  (let ((n 0)
	bufname)
    (while (progn
	     (setq bufname (concat "*scratch"
				   (if (= n 0) "" (int-to-string n))
				   "*"))
	     (setq n (1+ n))
	     (get-buffer bufname)))
    (switch-to-buffer (get-buffer-create bufname))
    (emacs-lisp-mode)
    ))


;; copy the current line ; mapped to C-c C-k above
(defun copy-line (&optional arg)
  "Do a 'kill-line' but copy rather than kill.  This function will directly call 'kill-line', so see documentation of 'kill-line' for how to use it including prefix argument ARG and relevant variables.  This function works by temporarily making the buffer read-only, so I suggest setting 'kill-read-only-ok' to t."
  (interactive "P")
  (setq buffer-read-only t)
  (kill-line arg)
  (setq buffer-read-only nil)
  (move-beginning-of-line 1))

;; duplicate the current line or region defined ; mapped to s-d above
(defun duplicate-current-line-or-region (arg)
  "Duplicates the current line or region ARG times.
If there's no region, the current line will be duplicated."
  (interactive "p")
  (if (region-active-p)
      (let ((beg (region-beginning))
	    (end (region-end)))
	(duplicate-region arg beg end)
	(one-shot-keybinding "d" (lambda() (interactive) (duplicate-region 1 beg end))))
    (duplicate-current-line arg)
    (one-shot-keybinding "d" 'duplicate-current-line)))

(defun one-shot-keybinding (key command)
  (set-transient-map
   (let ((map (make-sparse-keymap)))
     (define-key map (kbd key) command)
     map) t))

(defun replace-region-by (fn)
  (let* ((beg (region-beginning))
	 (end (region-end))
	 (contents (buffer-substring beg end)))
    (delete-region beg end)
    (insert (funcall fn contents))))

(defun duplicate-region (&optional num start end)
  "Duplicates the region bounded by START and END NUM times.
If no START and END is provided, the current region-beginning and
region-end is used."
  (interactive "p")
  (save-excursion
    (let* ((start (or start (region-beginning)))
	   (end (or end (region-end)))
	   (region (buffer-substring start end)))
      (goto-char end)
      (dotimes (i num)
	(insert region)))))

(defun duplicate-current-line (&optional num)
  "Duplicate the current line NUM times."
  (interactive "p")
  (if (bound-and-true-p paredit-mode)
      (paredit-duplicate-current-line)
    (save-excursion
      (when (eq (point-at-eol) (point-max))
	(goto-char (point-max))
	(newline)
	(forward-char -1))
      (duplicate-region num (point-at-bol) (1+ (point-at-eol))))))

;; macro saving
(defun save-macro (name)
  "Save a macro.  Take a NAME as argument and save the last defined macro under this name at the end of your init file."
  (interactive "SName of the macro :")
  (kmacro-name-last-macro name)
  (find-file user-init-file)
  (goto-char (point-max))
  (newline)
  (insert-kbd-macro name)
  (newline)
  (switch-to-buffer nil))

;; indentation and buffer cleanup
(defun untabify-buffer ()
  "Remove tabs from a buffer and replace with spaces."
  (interactive)
  (untabify (point-min) (point-max)))

(defun indent-buffer ()
  "Indent current buffer."
  (interactive)
  (indent-region (point-min) (point-max)))

(defun cleanup-buffer ()
  "Perform a bunch of operations on the whitespace content of a buffer."
  (interactive)
  (indent-buffer)
  (untabify-buffer)
  (delete-trailing-whitespace))

;; functions for push and jump to mark
(defun push-mark-no-activate ()
  "Pushes `point' to `mark-ring' and does not activate the region.  Equivalent to \\[set-mark-command] when \\[transient-mark-mode] is disabled."
  (interactive)
  (push-mark (point) t nil)
  (message "Pushed mark to ring"))

(defun jump-to-mark ()
  "Jumps to the local mark, respecting the `mark-ring' order.  This is the same as using \\[set-mark-command] with the prefix argument."
  (interactive)
  (set-mark-command 1))

;; function to edit the curent file as root
(defun sudo-edit (&optional arg)
  "Edit currently visited file as root.
With a prefix ARG prompt for a file to visit.
Will also prompt for a file to visit if current
buffer is not visiting a file."
  (interactive "P")
  (if (or arg (not buffer-file-name))
      (find-file (concat "/sudo:root@localhost:"
			 (ido-read-file-name "Find file(as root): ")))
    (find-alternate-file (concat "/sudo:root@localhost:" buffer-file-name))))

;; line numbers when using goto-line s-l or M-g M-g or M-g g
(defun goto-line-with-feedback ()
  "Show line numbers temporarily, while prompting for the line number input"
  (interactive)
  (unwind-protect
      (progn
	(linum-mode 1)
	(with-no-warnings (goto-line (read-number "Goto line: "))))
    (linum-mode -1)))



(provide 'init-misc-defuns)
;;; init-misc-defuns.el ends here
