;;; init-look-and-feel.el --- miscilaneous settings and a few small packages
;;; Commentary:
;; 2016 12 16 init SeJ
;; 2016 12 21 add kill-this-buffer
;; 2017 01 06 cleanup by move of packages to init-misc-pkgs.el
;; 2017 01 06 change from req-package to use-package
;; 2017 01 11 add scroll-margin 3
;; 2017 01 12 add steve drunken tips

;;; Code:

;; keybindings
(global-set-key (kbd "RET") 'newline-and-indent)
(global-set-key (kbd "C-;") 'comment-or-uncomment-region)
(global-set-key (kbd "M-/") 'hippie-expand)
(global-set-key (kbd "C-+") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)
(global-set-key (kbd "C-c C-k") 'compile)
(global-set-key (kbd "C-x g") 'magit-status)

(global-set-key (kbd "M-3") 'delete-other-windows)
(global-set-key (kbd "M-4") 'split-window-vertically)
(global-set-key (kbd "M-2") 'delete-window)
(global-set-key (kbd "M-s") 'other-window)
(global-set-key (kbd "<f1>") 'org-mode)

 ;added tips from pragmatic emacs
(global-set-key (kbd "C-x k") 'kill-this-buffer)
(global-set-key (kbd "C-x w") 'delete-frame)
(setq scroll-margin 3)


;;added tips from steve drunken blog 10 specific ways to improve productivity
(global-set-key "\C-x\C-m" 'execute-extended-command)
(global-set-key "\C-c\C-m" 'execute-extended-command)
;;(global-set-key "\C-w" 'backward-kill-word) ; not using to avoid confussion
(global-set-key "\C-x\C-k" 'kill-region)
(global-set-key "\C-c\C-k" 'kill-region)


;; Some beginning settings
(scroll-bar-mode -1)
(tool-bar-mode -1)
(menu-bar-mode -1)
(column-number-mode nil)

;; wind move built in package
(when (fboundp 'windmove-default-keybindings)
  (windmove-default-keybindings))
(global-set-key (kbd "C-c <left>")  'windmove-left)
(global-set-key (kbd "C-c <right>") 'windmove-right)
(global-set-key (kbd "C-c <up>")    'windmove-up)
(global-set-key (kbd "C-c <down>")  'windmove-down)

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

;; macro saving
(defun save-macro (name)
  "Save a macro.  Take a name as argument and save the last defined macro under this name at the end of your init file."
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
  (interactive)
  (untabify (point-min) (point-max)))

(defun indent-buffer ()
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

;; color codes
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)
(add-to-list 'comint-output-filter-functions 'ansi-color-process-output)


;; Save whatever’s in the current (system) clipboard before
;; replacing it with the Emacs’ text.
;; https://github.com/dakrone/eos/blob/master/eos.org
(setq save-interprogram-paste-before-kill t)


(provide 'init-look-and-feel)
;;; init-look-and-feel ends here
