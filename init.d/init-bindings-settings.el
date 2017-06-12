;;; init-bindings-settings.el --- bindings & settings
;;; Commentary:
;; Main file for keybindings.

;;; ChangeLog
;; 2016 12 16 init SeJ
;; 2016 12 21 add kill-this-buffer
;; 2017 01 06 cleanup by move of packages to init-misc-pkgs.el
;; 2017 01 06 change from req-package to use-package
;; 2017 01 11 add more pragmatic Emacs tips
;; 2017 01 12 add steve drunken tips
;; 2017 01 30 add sudo-edit function (C-x C-r) to edit file as sudo
;; 2017 03 29 add truncate lines setting
;; 2017 05 09 add copy-line C-c C-k
;; 2017 05 09 add some neat keybindings from emacs-starter-kit
;; 2017 05 09 rename file to init-bindings-settings.el
;; 2017 05 12 adds from purcell/emacs.d
;; 2017 05 21 add delete to trash can
;; 2017 05 25 add imenu binding
;; 2017 05 30 hippie-expand remap M-/ C-M-/
;; 2017 06 05 add sej-map keyboard mapping

;;; Code:

;; set up personal keymap
(dotimes (n 10)
  (global-unset-key (kbd (format "C-%d" n))))
(define-prefix-command 'sej-map)
(global-set-key (kbd "C-1") 'sej-map)

(define-key sej-map (kbd "a") 'counsel-ag)
(define-key sej-map (kbd "e") 'mu4e)
(define-key sej-map (kbd "s") 'shell)
(define-key sej-map (kbd "m") 'menu-bar-mode)
(define-key sej-map (kbd "o") 'org-mode)

(global-set-key (kbd "<f1>") 'org-mode)
(global-set-key (kbd "<f2>") 'shell)
(global-set-key (kbd "M-'") 'other-window)

;; general keybindings
(global-set-key (kbd "RET") 'newline-and-indent)
(global-set-key (kbd "C-;") 'comment-or-uncomment-region)
(global-set-key [remap dabbrev-expand] 'hippie-expand)
(setq hippie-expand-try-functions-list
      '(try-complete-file-name-partially
        try-complete-file-name
        try-expand-dabbrev
        try-expand-dabbrev-all-buffers
        try-expand-dabbrev-from-kill))

(global-set-key (kbd "C-+") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)
(global-set-key (kbd "C-x g") 'magit-status)

;;(global-set-key (kbd "M-3") 'delete-other-windows)
;;(global-set-key (kbd "M-4") 'split-window-vertically)
;;(global-set-key (kbd "M-2") 'delete-window)

;;added tips from pragmatic emacs
(global-set-key (kbd "C-x k") 'kill-this-buffer)
(global-set-key (kbd "C-x w") 'delete-frame)

;;scroll window up/down by one line
(global-set-key (kbd "M-n") (kbd "C-u 1 C-v"))
(global-set-key (kbd "M-p") (kbd "C-u 1 M-v"))
(global-set-key (kbd "M-SPC") 'cycle-spacing)

;;added tips from steve drunken blog 10 specific ways to improve productivity
(global-set-key (kbd "C-x C-m") 'execute-extended-command)
(global-set-key (kbd "C-c C-m") 'execute-extended-command)
(global-set-key (kbd "C-x C-k") 'kill-region)

;;added from emacs-starter-kit
;; You know, like Readline.
(global-set-key (kbd "C-M-d") 'backward-kill-word)

;; Align your code in a pretty way.
(global-set-key (kbd "C-x \\") 'align-regexp)

;; Perform general cleanup.
(global-set-key (kbd "C-c n") 'cleanup-buffer)

;; File finding
(global-set-key (kbd "C-x M-f") 'ido-find-file-other-window)
(global-set-key (kbd "C-x C-M-f") 'find-file-in-project)
(global-set-key (kbd "C-x f") 'recentf-ido-find-file)
(global-set-key (kbd "C-c y") 'bury-buffer)
(global-set-key (kbd "C-c r") 'revert-buffer)
(global-set-key (kbd "M-`") 'file-cache-minibuffer-complete)
;;(global-set-key (kbd "C-x C-b") 'ibuffer)

;; Fetch the contents at a URL, display it raw.
(global-set-key (kbd "C-x C-h") 'view-url)

;; Should be able to eval-and-replace anywhere.
(global-set-key (kbd "C-c e") 'eval-and-replace)

;; For debugging Emacs modes
(global-set-key (kbd "C-c p") 'message-point)
(global-set-key (kbd "C-c q") 'join-line)

;; wind move built in package (default bindins are S-<cursor>)
(when (fboundp 'windmove-default-keybindings)
  (windmove-default-keybindings))
(global-set-key (kbd "C-c <left>")  'windmove-left)
(global-set-key (kbd "C-c <right>") 'windmove-right)
(global-set-key (kbd "C-c <up>")    'windmove-up)
(global-set-key (kbd "C-c <down>")  'windmove-down)

;; framemove will move frames when at limits of current frame
(use-package framemove
  :config
  (setq framemove-hook-into-windmove t))

;; Some beginning settings
(when (display-graphic-p)
  (scroll-bar-mode -1)
  (tool-bar-mode -1))
(menu-bar-mode -1)
(column-number-mode nil)
(setq next-line-add-newlines t)

;;keep cursor at same position when scrolling
(setq scroll-preserve-screen-position 1)
(setq scroll-margin 3)

;; each line of text gets one line on the screen
(setq-default truncate-lines 1)
(setq truncate-partial-width-windows 1)

;; ignore case when searching
(setq-default case-fold-search 1)

;; require final newlines in files when they are saved
(setq require-final-newline 1)
;; add a new line when going to the next line
(setq next-line-add-newlines t)

;; marking text and clipboard settings
(delete-selection-mode t)
(transient-mark-mode t)
(setq select-enable-clipboard t)

;; empty line settings
(setq-default indicate-empty-lines nil)
(when (not indicate-empty-lines)
  (toggle-indicate-empty-lines))

;; indentation
(setq tab-width 2
      indent-tabs-mode nil)

;; yes and no settings
(defalias 'yes-or-no-p 'y-or-n-p)

;; echo keystrokes ; no dialog boxes ; visable bell ; highlight parens
(setq echo-keystrokes 0.1
      use-dialog-box nil
      visible-bell t)
(show-paren-mode t)

;; electric-pair-mode
(electric-pair-mode t)

;; Add proper word wrapping
(global-visual-line-mode t)

(setq backup-by-copying t      ; don't clobber symlinks
      backup-directory-alist
      '(("." . ".saves"))    ; don't litter my fs tree
      delete-old-versions t
      kept-new-versions 6
      kept-old-versions 2
      version-control t)       ; use versioned backups

;; delete to trash can
(setq delete-by-moving-to-trash t)

;; remove kill buffer with live process prompt
(setq kill-buffer-query-functions
      (remq 'process-kill-buffer-query-function
	    kill-buffer-query-functions))

(setq-default kill-read-only-ok t)

(global-set-key "\C-c\C-k" 'copy-line)

(defun copy-line (&optional arg)
  "Do a kill-line but copy rather than kill.  This function directly calls
kill-line, so see documentation of kill-line for how to use it including prefix
argument and relevant variables.  This function works by temporarily making the
buffer read-only, so I suggest setting kill-read-only-ok to t."
  (interactive "P")
  (setq buffer-read-only t)
  (kill-line arg)
  (setq buffer-read-only nil)
  (move-beginning-of-line 1))

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

(global-set-key (kbd "M-c") 'cleanup-buffer)

(setq-default show-trailing-whitespace t)

(defun push-mark-no-activate ()
  "Pushes `point' to `mark-ring' and does not activate the region.  Equivalent to \\[set-mark-command] when \\[transient-mark-mode] is disabled."
  (interactive)
  (push-mark (point) t nil)
  (message "Pushed mark to ring"))

(defun jump-to-mark ()
  "Jumps to the local mark, respecting the `mark-ring' order.  This is the same as using \\[set-mark-command] with the prefix argument."
  (interactive)
  (set-mark-command 1))

(global-set-key (kbd "C-`") 'push-mark-no-activate)
(global-set-key (kbd "M-`") 'jump-to-mark)

(use-package visible-mark
  :config
  (global-visible-mark-mode 1) ;; or add (visible-mark-mode) to specific hooks
  (setq visible-mark-max 3)
  (setq visible-mark-faces `(visible-mark-face1 visible-mark-face2))
  )

;; color codes
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)
(add-to-list 'comint-output-filter-functions 'ansi-color-process-output)


;; Save whatever’s in the current (system) clipboard before
;; replacing it with the Emacs’ text.
;; https://github.com/dakrone/eos/blob/master/eos.org
(setq save-interprogram-paste-before-kill t)

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

(global-set-key (kbd "C-x C-r") 'sudo-edit)

;; uniquify settings
(setq uniquify-buffer-name-style 'reverse)
(setq uniquify-separator " • ")
(setq uniquify-after-kill-buffer-p t)
(setq uniquify-ignore-buffers-re "^\\*")

(provide 'init-bindings-settings)
;;; init-bindings-settings.el ends here
