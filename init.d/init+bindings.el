;;; init+bindings.el --- Emacs bindings
;;; Commentary:
;; Main file for keybindings.

;;; ChangeLog:
;; 2016 12 16 init SeJ
;; 2016 12 21 add kill-this-buffer
;; 2017 01 06 cleanup by move of packages to init-misc-pkgs.el
;; 2017 01 06 change from req-package to use-package
;; 2017 01 11 add more pragmatic Emacs tips
;; 2017 01 12 add steve drunken tips
;; 2017 01 30 add sudo-edit function (C-x C-r) to edit file as sudo
;; 2017 03 29 add truncate lines setting
;; 2017 05 09 add copy-line C-c C-k
;; 	      add some neat keybindings from emacs-starter-kit
;; 	      rename file to init-bindings-settings.el
;; 2017 05 12 adds from purcell/emacs.d
;; 2017 05 21 add delete to trash can
;; 2017 05 25 add imenu binding
;; 2017 05 30 hippie-expand remap M-/ C-M-/
;; 2017 06 05 add sej-map keyboard mapping
;; 2017 08 07 add Hyper & Super keys for osx and win remove sej-map
;; 2017 08 21 add some Lisp stuff from Getting Started with Emacs Lisp
;; 2017 08 29 take another run at sej-map
;; 2017 09 08 add code to unset C- M- digit keys
;; 2017 09 18 add goto-line with temp line numbers
;; 2017 09 19 add transpose keybindings & others from magnar
;; 2017 09 20 make more pure keybindings & move others stuff out

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
    (with-no-warnings
      (progn
        (setq mac-command-modifier 'meta)
        (setq mac-option-modifier 'super)
        (setq mac-control-modifier 'control)
        (setq ns-function-modifier 'hyper)
        ;; keybinding to toggle full screen mode
        (global-set-key (kbd "M-<f11>") 'toggle-frame-fullscreen)
        )
      (progn
        (setq w32-pass-lwindow-to-system nil)
        (setq w32-lwindow-modifier 'super)
        (setq w32-pass-rwindow-to-system nil)
        (setq w32-rwindow-modifier 'super)
        (setq w32-pass-apps-to-system nil)
        (setq w32-apps-modifier 'hyper)
        )))

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
;;           (define-key map sej-keymap-prefix 'sej-mode-map)
;;           map)
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

;; shorthand for interactive lambdas
(defmacro λ (&rest body)
  `(lambda ()
     (interactive)
     ,@body))

(global-set-key (kbd "H-l") (λ (insert "\u03bb")))
(global-set-key (kbd "C-x 8 l") (λ (insert "\u03bb")))
;; More neat bindings for C-x 8
(global-set-key (kbd "C-x 8 t m") (λ (insert "™")))
(global-set-key (kbd "C-x 8 C") (λ (insert "©")))
(global-set-key (kbd "C-x 8 - >") (λ (insert "→")))
(global-set-key (kbd "C-x 8 8") (λ (insert "∞")))
(global-set-key (kbd "C-x 8 v") (λ (insert "✓")))


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
(define-key sej-mode-map (kbd "<M-return>") 'eval-last-sexp)
(define-key sej-mode-map (kbd "<s-return>") 'eval-buffer)
(define-key sej-mode-map (kbd "s-b") 'helm-mini)
(define-key sej-mode-map (kbd "s-d") 'duplicate-current-line-or-region)  ;;defined below
(define-key sej-mode-map (kbd "s-i") 'emacs-init-time)
(define-key sej-mode-map (kbd "s-s") 'save-buffer) ;; defined just here for ref
(define-key sej-mode-map (kbd "s-q") 'save-buffers-kill-emacs) ;; defined just here for ref
;; toggle two most recent buffers
(fset 'quick-switch-buffer [?\C-x ?b return])
(define-key sej-mode-map (kbd "s-o") 'quick-switch-buffer)

;; general keybindings
(define-key global-map (kbd "C-h C-h") nil)
(define-key sej-mode-map (kbd "C-h C-h") nil)
(define-key sej-mode-map (kbd "M-'") 'other-window)
(define-key sej-mode-map (kbd "C-j") 'newline-and-indent)
(define-key sej-mode-map (kbd "C-;") 'comment-dwim)
(define-key sej-mode-map (kbd "M-/") 'hippie-expand)
(define-key sej-mode-map (kbd "M-j") (lambda () (interactive) (join-line -1)))

(define-key sej-mode-map (kbd "C-+") 'text-scale-increase)
(define-key sej-mode-map (kbd "C--") 'text-scale-decrease)
(define-key sej-mode-map (kbd "C-x g") 'magit-status)

(define-key sej-mode-map (kbd "s-0") 'delete-window)
(define-key sej-mode-map (kbd "s-1") 'delete-other-windows)
(define-key sej-mode-map (kbd "s-2") 'split-window-vertically)
(define-key sej-mode-map (kbd "s-3") 'split-window-right)
(define-key sej-mode-map (kbd "s-4") 'dired-other-frame)
(define-key sej-mode-map (kbd "s-5") 'make-frame-command)
(define-key sej-mode-map (kbd "s-6") 'delete-other-frames)

;;added tips from pragmatic emacs
(define-key sej-mode-map (kbd "C-x k") 'kill-this-buffer)
(define-key sej-mode-map (kbd "C-x w") 'delete-frame)

;; Zap to char
(define-key sej-mode-map (kbd "M-z") 'zap-to-char)
(define-key sej-mode-map (kbd "s-z") (lambda (char) (interactive "cZap to char backwards: ") (zap-to-char -1 char)))
(define-key sej-mode-map (kbd "C-M-d") 'backward-kill-word)
(define-key sej-mode-map (kbd "C-c C-k") 'copy-line) ;; defined below

;;scroll window up/down by one line
(define-key sej-mode-map (kbd "s-n") (kbd "C-u 1 C-v"))
(define-key sej-mode-map (kbd "s-p") (kbd "C-u 1 M-v"))
(define-key sej-mode-map (kbd "M-SPC") 'cycle-spacing)

;;added tips from steve drunken blog 10 specific ways to improve productivity
(define-key sej-mode-map (kbd "C-x C-m") 'execute-extended-command)
(define-key sej-mode-map (kbd "C-c C-m") 'execute-extended-command)

;; Align your code in a pretty way.
(define-key sej-mode-map (kbd "C-x \\") 'align-regexp)

;; Perform general cleanup.
(define-key sej-mode-map (kbd "C-c n") 'cleanup-buffer)

;; File & buffer finding
(define-key sej-mode-map (kbd "C-x C-M-f") 'find-file-in-project)
(define-key sej-mode-map (kbd "C-c y") 'bury-buffer)
(define-key sej-mode-map (kbd "s-y") 'bury-buffer)
(define-key sej-mode-map (kbd "C-c r") 'revert-buffer)
(define-key sej-mode-map (kbd "M-`") 'file-cache-minibuffer-complete)
(define-key sej-mode-map (kbd "M-n") 'bs-cycle-next)
(define-key sej-mode-map (kbd "M-p") 'bs-cycle-previous)
(define-key sej-mode-map (kbd "C-c b") 'create-scratch-buffer) ; defined below

;; Transpose stuff with M-t
(global-unset-key (kbd "M-t")) ;; which used to be transpose-words
(global-set-key (kbd "M-t l") 'transpose-lines)
(global-set-key (kbd "M-t w") 'transpose-words)
(global-set-key (kbd "M-t s") 'transpose-sexps)
(global-set-key (kbd "M-t p") 'transpose-params)

;; wind move built in package (default bindins are S-<cursor>)
(when (fboundp 'windmove-default-keybindings)
  (windmove-default-keybindings)) ;; Shift + direction
(winner-mode t)
;;(define-key sej-mode-map (kbd "C-c <left>")  'windmove-left)
;;(define-key sej-mode-map (kbd "C-c <right>") 'windmove-right)
;;(define-key sej-mode-map (kbd "C-c <up>")    'windmove-up)
;;(define-key sej-mode-map (kbd "C-c <down>")  'windmove-down)

;; buffer-move package to swap buffers between windows
;; (defined in init-movement.el)
(define-key sej-mode-map (kbd "<s-up>") 'buf-move-up)
(define-key sej-mode-map (kbd "<s-down>") 'buf-move-down)
(define-key sej-mode-map (kbd "<s-left>") 'buf-move-left)
(define-key sej-mode-map (kbd "<s-right>") 'buf-move-right)

;; goto-chg package
;; (defined in init-movement.el)
(define-key sej-mode-map (kbd "M-.") 'goto-last-change)
(define-key sej-mode-map (kbd "C-M-.") 'goto-last-change)
(define-key sej-mode-map (kbd "C-,") 'goto-last-change-reverse)

;; avy efficient movement through beginning letters
;; (defined in init-movement.el)
(define-key sej-mode-map (kbd "C-<return>") 'avy-goto-word-1)

;; crux package smart beginning of line movement
;; (defined in init-movement.el)
(define-key sej-mode-map (kbd "C-a") 'crux-move-beginning-of-line)

;; drag-stuff package
;; (defined in init-movement.el)
(define-key sej-mode-map (kbd "M-<down>") 'drag-stuff-down)
(define-key sej-mode-map (kbd "M-<up>") 'drag-stuff-up)

;; push and jump to mark functions
;; (defined in init-misc-defuns.el)
(define-key sej-mode-map (kbd "C-`") 'push-mark-no-activate)
(define-key sej-mode-map (kbd "M-`") 'jump-to-mark)

;; cleanup-buffer function
;; (defined in init-misc-defuns.el)
(define-key sej-mode-map (kbd "M-c") 'cleanup-buffer)

;; function to edit the curent file as root
;; (defined in init-misc-defuns.el)
(define-key sej-mode-map (kbd "C-x C-r") 'sudo-edit)

;; line numbers when using goto-line s-l or M-g M-g or M-g g
;; (defined in init-misc-defuns.el)
(global-set-key [remap goto-line] 'goto-line-with-feedback)


(provide 'init+bindings)
;;; init+bindings.el ends here
