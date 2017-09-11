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
;; 2017 08 07 add Hyper & Super keys for osx and win remove sej-map
;; 2017 08 21 add some Lisp stuff from Getting Started with Emacs Lisp
;; 2017 08 29 take another run at sej-map
;; 2017 09 08 add code to unset C- M- digit keys

;;; Code:

;; set keys for Apple keyboard, for emacs in OS X
;; make cmd key do Meta
;; make opt key do Super
;; make Control key do Control
;; make Fn key do Hyper

;; make PC keyboard's Win key or other to type Super or Hyper, for emacs running on Windows.
;; Left Windows key
;; Right Windows key
;; Menu/App key

(defconst *is-a-mac* (eq system-type 'darwin))

(if *is-a-mac*
    (progn
      (setq mac-command-modifier 'meta)
      (setq mac-option-modifier 'super)
      (setq mac-control-modifier 'control)
      (setq ns-function-modifier 'hyper)
      ))
;; (progn
;;   (setq w32-pass-lwindow-to-system nil)
;;   (setq w32-lwindow-modifier 'super)
;;   (setq w32-pass-rwindow-to-system nil)
;;   (setq w32-rwindow-modifier 'super)
;;   (setq w32-pass-apps-to-system nil)
;;   (setq w32-apps-modifier 'hyper)
;;   ))

;; Below is taken from stackexchange (Emacs)
;; Main use is to have my key bindings have the highest priority
;; https://github.com/kaushalmodi/.emacs.d/blob/master/elisp/modi-mode.el

;; Instead of below going to use the prefix where appropriate but use Hyper & Super a lot
;; (defvar sej-keymap-prefix (kbd "C-c s")
;;   "'sej-mode' keymap prefix.
;; Overrides the default binding.")

;; (defvar sej-mode-map (make-sparse-keymap)
;;   "Keymap for `sej-mode' whose bindings begin with 'sej-keymap-prefix'.")
;; (fset 'sej-mode-map sej-mode-map)

;; (defvar sej-mode-map (let ((map (make-sparse-keymap)))
;; 		       (define-key map sej-keymap-prefix 'sej-mode-map)
;; 		       map)
;;   "Keymap for 'sej-mode'.")

(defvar sej-mode-map (make-sparse-keymap)
  "Keymap for 'sej-mode'.")

;;;###autoload
(define-minor-mode sej-mode
  "A minor mode so that my key settings override annoying major modes."
  ;; If init-value is not set to t, this mode does not get enabled in
  ;; `fundamental-mode' buffers even after doing \"(global-my-mode 1)\".
  ;; More info: http://emacs.stackexchange.com/q/16693/115
  :init-value t
  :lighter " sej"
  :keymap sej-mode-map)

;;;###autoload
(define-globalized-minor-mode global-sej-mode sej-mode sej-mode)

;; https://github.com/jwiegley/use-package/blob/master/bind-key.el
;; The keymaps in `emulation-mode-map-alists' take precedence over
;; `minor-mode-map-alist'
(add-to-list 'emulation-mode-map-alists `((sej-mode . ,sej-mode-map)))

;; Turn off the minor mode in the minibuffer
(defun turn-off-sej-mode ()
  "Turn off sej-mode."
  (sej-mode -1))
(add-hook 'minibuffer-setup-hook #'turn-off-sej-mode)

(defmacro bind-to-sej-map (key fn)
  "Bind to KEY a function to the `sej-mode-map'.
USAGE: (bind-to-sej-map \"f\" #'full-screen-center)."
  `(define-key sej-mode-map (kbd ,key) ,fn))

;; http://emacs.stackexchange.com/a/12906/115
(defun unbind-from-sej-map (key)
  "Unbind from KEY the function from the 'sej-mode-map'.
USAGE: (unbind-from-modi-map \"key f\")."
  (interactive "kUnset key from sej-mode-map: ")
  (define-key sej-mode-map (kbd (key-description key)) nil)
  (message "%s" (format "Unbound %s key from the %s."
			(propertize (key-description key)
				    'face 'font-lock-function-name-face)
			(propertize "sej-mode-map"
				    'face 'font-lock-function-name-face))))
;; Minor mode tutorial: http://nullprogram.com/blog/2013/02/06/

;; unset C- and M- digit keys
(dotimes (n 10)
  (global-unset-key (kbd (format "C-%d" n)))
  (global-unset-key (kbd (format "M-%d" n)))
  )

;; use hyper (alt on osx) for mode type bindings
(define-key sej-mode-map (kbd "H-a") 'counsel-ag)
(define-key sej-mode-map (kbd "H-o") 'org-mode)
(define-key sej-mode-map (kbd "<f1>") 'org-mode)
(define-key sej-mode-map (kbd "H-s") 'shell)
(define-key sej-mode-map (kbd "<f2>") 'shell)
(define-key sej-mode-map (kbd "H-m") 'menu-bar-mode)
(define-key sej-mode-map (kbd "H-h") 'ns-do-hide-emacs)
(define-key sej-mode-map (kbd "H-H") 'ns-do-hide-others)
;;(global-set-key (kbd "H-e") 'mu4e) ; not used for the moment
;;(global-set-key (kbd "M-`") 'ns-next-frame)

;; use super for action type stuff
;; some lisp stuff from Getting Started with Emacs Lisp
(define-key sej-mode-map (kbd "<s-return>") 'eval-last-sexp)
(define-key sej-mode-map (kbd "<M-return>") 'eval-buffer)
(define-key sej-mode-map (kbd "s-i") 'emacs-init-time)
(define-key sej-mode-map (kbd "s-s") 'save-buffer) ;; defined just here for ref
(define-key sej-mode-map (kbd "s-q") 'save-buffers-kill-emacs) ;; defined just here for ref

;; general keybindings
(define-key global-map (kbd "C-h C-h") nil)
(define-key sej-mode-map (kbd "C-h C-h") nil)
(define-key sej-mode-map (kbd "M-'") 'other-window)
(define-key sej-mode-map (kbd "C-j") 'newline-and-indent)
(define-key sej-mode-map (kbd "C-;") 'comment-dwim)
(define-key sej-mode-map (kbd "M-/") 'hippie-expand)
(setq hippie-expand-try-functions-list
      '(hippie-expand-try-functions-list
	try-complete-file-name-partially
        try-expand-dabbrev
        try-expand-dabbrev-all-buffers
        try-expand-dabbrev-from-kill
	try-complete-file-name-partially
        try-complete-file-name
	try-expand-all-abbrevs
	try-expand-list
	try-expand-line
	try-expand-line-all-buffers
	try-complete-lisp-symbol-partially
	try-compelete-lisp-symbol))

(define-key sej-mode-map (kbd "C-+") 'text-scale-increase)
(define-key sej-mode-map (kbd "C--") 'text-scale-decrease)
(define-key sej-mode-map (kbd "C-x g") 'magit-status)
(define-key sej-mode-map (kbd "s-b") 'helm-mini)

(define-key sej-mode-map (kbd "s-0") 'delete-window)
(define-key sej-mode-map (kbd "s-1") 'delete-other-windows)
(define-key sej-mode-map (kbd "s-2") 'split-window-vertically)
(define-key sej-mode-map (kbd "s-3") 'split-window-right)

;;added tips from pragmatic emacs
(define-key sej-mode-map (kbd "C-x k") 'kill-this-buffer)
(define-key sej-mode-map (kbd "C-x w") 'delete-frame)

;;scroll window up/down by one line
(define-key sej-mode-map (kbd "s-n") (kbd "C-u 1 C-v"))
(define-key sej-mode-map (kbd "s-p") (kbd "C-u 1 M-v"))
(define-key sej-mode-map (kbd "M-SPC") 'cycle-spacing)

;;added tips from steve drunken blog 10 specific ways to improve productivity
(define-key sej-mode-map (kbd "C-x C-m") 'execute-extended-command)
(define-key sej-mode-map (kbd "C-c C-m") 'execute-extended-command)

;;added from emacs-starter-kit
;; You know, like Readline.
(define-key sej-mode-map (kbd "C-M-d") 'backward-kill-word)

;; Align your code in a pretty way.
(define-key sej-mode-map (kbd "C-x \\") 'align-regexp)

;; Perform general cleanup.
(define-key sej-mode-map (kbd "C-c n") 'cleanup-buffer)

;; File & buffer finding
(define-key sej-mode-map (kbd "C-x M-f") 'ido-find-file-other-window)
(define-key sej-mode-map (kbd "C-x C-M-f") 'find-file-in-project)
(define-key sej-mode-map (kbd "C-x f") 'recentf-ido-find-file)
(define-key sej-mode-map (kbd "C-c y") 'bury-buffer)
(define-key sej-mode-map (kbd "s-y") 'bury-buffer)
(define-key sej-mode-map (kbd "C-c r") 'revert-buffer)
(define-key sej-mode-map (kbd "M-`") 'file-cache-minibuffer-complete)
(define-key sej-mode-map (kbd "M-n") 'bs-cycle-next)
(define-key sej-mode-map (kbd "M-p") 'bs-cycle-previous)

;; wind move built in package (default bindins are S-<cursor>)
(when (fboundp 'windmove-default-keybindings)
  (windmove-default-keybindings))
(define-key sej-mode-map (kbd "C-c <left>")  'windmove-left)
(define-key sej-mode-map (kbd "C-c <right>") 'windmove-right)
(define-key sej-mode-map (kbd "C-c <up>")    'windmove-up)
(define-key sej-mode-map (kbd "C-c <down>")  'windmove-down)

;; framemove will move frames when at limits of current frame
(use-package framemove
  :ensure t
  :config
  (setq framemove-hook-into-windmove t))

;; buffer-move to swap buffers between windows
(use-package buffer-move
  :ensure t
  :defer t
  :bind (:map sej-mode-map
	      ("<s-up>" . buf-move-up)
	      ("<s-down>" . buf-move-down)
	      ("<s-left>" . buf-move-left)
	      ("<s-right>" . buf-move-right)))

;; Some beginning settings
(when (display-graphic-p)
  (scroll-bar-mode -1)
  (tool-bar-mode -1))
(menu-bar-mode t)
(column-number-mode nil)
(setq next-line-add-newlines t)

;; indentation & fill column
(setq tab-width 2
      indent-tabs-mode nil
      fill-column 80)

;; yes and no settings
(defalias 'yes-or-no-p 'y-or-n-p)

;; don't indicate empty or end of a buffer
(setq-default indicate-empty-lines nil)
(setq-default indicate-buffer-boundaries nil)

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
(delete-selection-mode nil)
(transient-mark-mode t)
(setq select-enable-clipboard t)

;; empty line settings
(setq-default indicate-empty-lines nil)

;; echo keystrokes ; no dialog boxes ; visable bell ; highlight parens
(setq echo-keystrokes 0.1)
(setq use-dialog-box nil
      visible-bell t)
(show-paren-mode t)

;; electric-pair-mode
(electric-pair-mode t)
(electric-layout-mode t)
(electric-indent-mode t)
;; Ignore electric indentation for python and yaml
(defun electric-indent-ignore-mode (char)
  "Ignore electric indentation for 'python-mode'.  CHAR is input character."
  (if (or (equal major-mode 'python-mode)
          (equal major-mode 'yaml-mode))
      'no-indent
    nil))
(add-hook 'electric-indent-functions 'electric-indent-ignore-mode)

;; Add proper word wrapping
(global-visual-line-mode t)
(setq line-move-visual t)

(setq-default backup-directory-alist
	      '(("." . ".saves")))    ; don't litter my fs tree

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

;; hide mouse while typing
(setq make-pointer-invisible t)


(define-key sej-mode-map "\C-c\C-k" 'copy-line)

(defun copy-line (&optional arg)
  "Do a 'kill-line' but copy rather than kill.  This function will directly call 'kill-line', so see documentation of 'kill-line' for how to use it including prefix argument ARG and relevant variables.  This function works by temporarily making the buffer read-only, so I suggest setting 'kill-read-only-ok' to t."
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

(define-key sej-mode-map (kbd "M-c") 'cleanup-buffer)

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

(define-key sej-mode-map (kbd "C-`") 'push-mark-no-activate)
(define-key sej-mode-map (kbd "M-`") 'jump-to-mark)

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

(define-key sej-mode-map (kbd "C-x C-r") 'sudo-edit)

;; uniquify settings
(setq uniquify-buffer-name-style 'reverse)
(setq uniquify-separator " • ")
(setq uniquify-after-kill-buffer-p t)
(setq uniquify-ignore-buffers-re "^\\*")

(defadvice kill-buffer (around kill-buffer-around-advice activate)
  "Bury the *scratch* buffer, but never kill it."
  (let ((buffer-to-kill (ad-get-arg 0)))
    (if (equal buffer-to-kill "*scratch*")
        (bury-buffer)
      ad-do-it)))


(provide 'init-bindings-settings)
;;; init-bindings-settings.el ends here






