;; Stephen's emacs init-real.el file
;; 2016-02-24 init
;; 2016 03 17 good ideas from aaron bedra's emacs configuration
;; 2016 11 29 integrate win-nt version & virtualbox
;; 2016 11 30 cleanup & concat with linux versions
;; 2016 12 12 transfer updates from test-version
;; 2016 12 15 updates due to win move to wsys2/ming64
;; 2016 12 15 split init.el & init-real.el adding init.d

;; el-get
(add-to-list 'load-path "~/.emacs.d/el-get/el-get")
(unless (require 'el-get nil 'noerror)
  (with-current-buffer
      (url-retrieve-synchronously
       "https://raw.githubusercontent.com/dimitri/el-get/master/el-get-install.el")
    (goto-char (point-max))
    (eval-print-last-sexp)))
(add-to-list 'el-get-recipe-path "~/.emacs.d/el-get-user/recipes")
(el-get 'sync)

;; recompile configs
(add-hook 'kill-emacs-hook (lambda () (byte-recompile-directory my-init-dir 0 t)))

;; elpa
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives '("elpa" . "http://tromey.com/elpa/"))
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/"))
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/"))
(add-to-list 'package-archives '("sunrise" . "http://joseito.republika.pl/sunrise-commander/"))

(defconst my-init-dir "~/.emacs.d/init.d")

;; set up package manager
(eval-when-compile (package-initialize))

(defun require-package (package)
  "refresh package archives, check package presence and install if it's not installed"
  (if (null (require package nil t))
      (progn (let* ((ARCHIVES (if (null package-archive-contents)
                                  (progn (package-refresh-contents)
                                         package-archive-contents)
                                package-archive-contents))
                    (AVAIL (assoc package ARCHIVES)))
               (if AVAIL
                   (package-install package)))
             (require package))))

;; use package
(require-package 'use-package)
(require 'use-package)

;; req-package
(require-package 'req-package)
(require 'req-package)
(req-package--log-set-level 'trace)

;; init.d
(random t)
(req-package load-dir
  :force true
  :init
  (setq force-load-messages t)
  (setq load-dir-debug nil)
  (setq load-dir-recursive nil)
  :config
  (load-dir-one my-init-dir)
  (req-package-finish))


(switch-to-buffer "*dashboard*")
(dashboard-insert-startupify-lists)
