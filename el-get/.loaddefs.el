;;; .loaddefs.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads nil "aggressive-indent-mode/aggressive-indent"
;;;;;;  "aggressive-indent-mode/aggressive-indent.el" (22613 59048
;;;;;;  0 0))
;;; Generated autoloads from aggressive-indent-mode/aggressive-indent.el

(autoload 'aggressive-indent-indent-defun "aggressive-indent-mode/aggressive-indent" "\
Indent current defun.
Throw an error if parentheses are unbalanced.
If L and R are provided, use them for finding the start and end of defun.

\(fn &optional L R)" t nil)

(autoload 'aggressive-indent-indent-region-and-on "aggressive-indent-mode/aggressive-indent" "\
Indent region between L and R, and then some.
Call `indent-region' between L and R, and then keep indenting
until nothing more happens.

\(fn L R)" t nil)

(autoload 'aggressive-indent-mode "aggressive-indent-mode/aggressive-indent" "\
Toggle Aggressive-Indent mode on or off.
With a prefix argument ARG, enable Aggressive-Indent mode if ARG is
positive, and disable it otherwise.  If called from Lisp, enable
the mode if ARG is omitted or nil, and toggle it if ARG is `toggle'.
\\{aggressive-indent-mode-map}

\(fn &optional ARG)" t nil)

(defvar global-aggressive-indent-mode nil "\
Non-nil if Global Aggressive-Indent mode is enabled.
See the `global-aggressive-indent-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `global-aggressive-indent-mode'.")

(custom-autoload 'global-aggressive-indent-mode "aggressive-indent-mode/aggressive-indent" nil)

(autoload 'global-aggressive-indent-mode "aggressive-indent-mode/aggressive-indent" "\
Toggle Aggressive-Indent mode in all buffers.
With prefix ARG, enable Global Aggressive-Indent mode if ARG is positive;
otherwise, disable it.  If called from Lisp, enable the mode if
ARG is omitted or nil.

Aggressive-Indent mode is enabled in all buffers where
`aggressive-indent-mode' would do it.
See `aggressive-indent-mode' for more information on Aggressive-Indent mode.

\(fn &optional ARG)" t nil)

(defalias 'aggressive-indent-global-mode #'global-aggressive-indent-mode)

;;;***

;;;### (autoloads nil "el-get/el-get" "el-get/el-get.el" (22613 58974
;;;;;;  0 0))
;;; Generated autoloads from el-get/el-get.el

(autoload 'el-get-version "el-get/el-get" "\
Message the current el-get version

\(fn)" t nil)

(autoload 'el-get-install "el-get/el-get" "\
Cause the named PACKAGE to be installed after all of its
dependencies (if any).

PACKAGE may be either a string or the corresponding symbol.

\(fn PACKAGE)" t nil)

(autoload 'el-get-update "el-get/el-get" "\
Update PACKAGE.

\(fn PACKAGE)" t nil)

(autoload 'el-get-update-all "el-get/el-get" "\
Performs update of all installed packages.

\(fn &optional NO-PROMPT)" t nil)

(autoload 'el-get-update-packages-of-type "el-get/el-get" "\
Update all installed packages of type TYPE.

\(fn TYPE)" t nil)

(autoload 'el-get-self-update "el-get/el-get" "\
Update el-get itself.  The standard recipe takes care of reloading the code.

\(fn)" t nil)

(autoload 'el-get-remove "el-get/el-get" "\
Remove any PACKAGE that is know to be installed or required.

\(fn PACKAGE)" t nil)

(autoload 'el-get-reinstall "el-get/el-get" "\
Remove PACKAGE and then install it again.

\(fn PACKAGE)" t nil)

(autoload 'el-get-cd "el-get/el-get" "\
Open dired in the package directory.

\(fn PACKAGE)" t nil)

(autoload 'el-get-make-recipes "el-get/el-get" "\
Loop over `el-get-sources' and write a recipe file for each
entry which is not a symbol and is not already a known recipe.

\(fn &optional DIR)" t nil)

(autoload 'el-get-checksum "el-get/el-get" "\
Compute the checksum of the given package, and put it in the kill-ring

\(fn PACKAGE)" t nil)

(autoload 'el-get-self-checksum "el-get/el-get" "\
Compute the checksum of the running version of el-get itself.

Also put the checksum in the kill-ring.

\(fn)" t nil)

(autoload 'el-get "el-get/el-get" "\
Ensure that packages have been downloaded once and init them as needed.

This will not update the sources by using `apt-get install' or
`git pull', but it will ensure that:

* the packages have been installed
* load-path is set so their elisp files can be found
* Info-directory-list is set so their info files can be found
* Autoloads have been prepared and evaluated for each package
* Any post-installation setup (e.g. `(require 'feature)') happens

When SYNC is nil (the default), all installations run
concurrently, in the background.

When SYNC is 'sync, each package will be installed synchronously,
and any error will stop it all.

Please note that the `el-get-init' part of `el-get' is always
done synchronously. There's `byte-compile' support though, and
the packages you use are welcome to use `autoload' too.

PACKAGES is expected to be a list of packages you want to install
or init.  When PACKAGES is omited (the default), the list of
already installed packages is considered.

\(fn &optional SYNC &rest PACKAGES)" nil nil)

;;;***

;;;### (autoloads nil "el-get/el-get-bundle" "el-get/el-get-bundle.el"
;;;;;;  (22613 58974 0 0))
;;; Generated autoloads from el-get/el-get-bundle.el

(autoload 'el-get-bundle-el-get "el-get/el-get-bundle" "\


\(fn SRC SYNC)" nil nil)

(autoload 'el-get-bundle "el-get/el-get-bundle" "\
Install PACKAGE and run initialization FORM.

PACKAGE can be either a simple package name or a package name
with a modifier before the name to specify local recipe source
information:

* `<owner>/' : specifies a Github owner name
* `gist:<id>' : specifies a Gist ID
* `<type>:' : specifies a type of the package source

If `FEATURE in PACKAGE' form is used instead of PACKAGE, then
that FEATURE is `require'd after installing PACKAGE.  You can
also use `el-get-bundle!' macro if FEATURE and PACKAGE are the
same.  If you wish to `require' more than one feature, then use
`:features' property in FORM.

The initialization FORM may start with a property list that
describes a local recipe.  The property list may include the keyword
`:bundle-sync' with a value of either `t' or `nil' to request that
`el-get-bundle' invoke `el-get' synchronously (respectively asynchronously).
The keyword `:bundle-async' is the inverse of `:bundle-sync'.
\(Note that the request to run el-get synchronously may not be respected in all
circumstances: see the definition of `el-get-bundle-el-get' for details.)
The FORM after the property list is treated as initialization code,
which is actually an `:after' property of the local recipe.

A copy of the initialization code is stored in a directory
specified by `el-get-bundle-init-directory' and its byte-compiled
version is used if `el-get-bundle-byte-compile' is non-nil.

\(fn PACKAGE &rest FORM)" nil t)

(function-put 'el-get-bundle 'lisp-indent-function 'defun)

(autoload 'el-get-bundle! "el-get/el-get-bundle" "\
Install PACKAGE and run initialization form.
It is the same as `el-get-bundle' except that PACKAGE is explicitly
required.

\(fn PACKAGE &rest ARGS)" nil t)

(function-put 'el-get-bundle! 'lisp-indent-function 'defun)

;;;***

;;;### (autoloads nil "el-get/el-get-check" "el-get/el-get-check.el"
;;;;;;  (22613 58974 0 0))
;;; Generated autoloads from el-get/el-get-check.el

(autoload 'el-get-check-recipe "el-get/el-get-check" "\
Check the format of the recipe.
Please run this command before sending a pull request.
Usage: M-x el-get-check-recipe RET

You can run this function from checker script like this:
    test/check-recipe.el PATH/TO/RECIPE.rcp

When used as a lisp function, FILE-OR-BUFFER must be a buffer
object or a file path.

\(fn FILE-OR-BUFFER)" t nil)

;;;***

;;;### (autoloads nil "el-get/el-get-list-packages" "el-get/el-get-list-packages.el"
;;;;;;  (22613 58974 0 0))
;;; Generated autoloads from el-get/el-get-list-packages.el

(autoload 'el-get-list-packages "el-get/el-get-list-packages" "\
Display a list of packages.

\(fn)" t nil)

;;;***

;;;### (autoloads nil "names/names" "names/names.el" (22613 60333
;;;;;;  0 0))
;;; Generated autoloads from names/names.el

(defvar names--inside-make-autoload nil "\
Used in `make-autoload' to indicate we're making autoloads.")

(autoload 'define-namespace "names/names" "\
Inside the namespace NAME, execute BODY.
NAME can be any symbol (not quoted), but it's highly recommended
to use some form of separator (such as :, /, or -). For a
complete description of this macro, please visit the frontpage
with \\[names-view-manual].

In summary, this macro has two main effects:

1. Any definitions inside BODY will have NAME prepended to the
symbol given. Ex:

    (define-namespace foo-
    (defvar bar 1 \"docs\")
    )

expands to

    (defvar foo-bar 1 \"docs\")


2. Any function calls and variable names get NAME prepended to
them if such a variable or function exists. Ex:

    (define-namespace foo:
    (defun message (x y) nil)
    (message \"%s\" my-var)
    )

expands to

    (defun foo:message (x y) nil)
    (foo:message \"%s\" my-var)

Note how `message' is expanded to `foo:message' in the second
form, because that function exists. Meanwhile, `bar' is left
untouched because `foo:bar' is not a known variable name.

===============================

AUTOLOAD

In order for `define-namespace' to work with \";;;###autoload\"
comments must replace all instances of \";;;###autoload\" inside
your `define-namespace' with `:autoload'.
Afterwards, add an \";;;###autoload\" comment just above your
`define-namespace'.

===============================

KEYWORDS

Immediately after NAME you may add keywords which customize the
behaviour of `define-namespace'. For a list of possible keywords
and a description of their effects, see the variable
`names--keyword-list'.

\(fn NAME [KEYWORD ...] BODY)" nil t)

(function-put 'define-namespace 'lisp-indent-function '(lambda (&rest x) 0))

(eval-after-load 'find-func '(defadvice find-function-search-for-symbol (around names-around-find-function-search-for-symbol-advice (symbol type library) activate) "Make sure `find-function-search-for-symbol' understands namespaces." ad-do-it (ignore-errors (unless (cdr ad-return-value) (with-current-buffer (car ad-return-value) (search-forward-regexp "^(define-namespace\\_>") (skip-chars-forward "\n[:blank:]") (let* ((names--regexp (concat "\\`" (regexp-quote (symbol-name (read (current-buffer)))))) (short-symbol (let ((name (symbol-name symbol))) (when (string-match names--regexp name) (intern (replace-match "" nil nil name)))))) (when short-symbol (ad-set-arg 0 short-symbol) ad-do-it)))))))

(defadvice make-autoload (around names-before-make-autoload-advice (form file &optional expansion) activate) "\
Make sure `make-autoload' understands `define-namespace'.
Use the `names--inside-make-autoload' variable to indicate to
`define-namespace' that we're generating autoloads." (require (quote names)) (if (null (eq (car-safe form) (quote define-namespace))) ad-do-it (setq names--inside-make-autoload t) (setq form (macroexpand form)) (setq names--inside-make-autoload nil) (if (version< emacs-version "24.3") (setq ad-return-value (cons (quote progn) (mapcar (lambda (x) (names--make-autoload-compat x file)) (cdr form)))) (ad-set-arg 2 (quote expansion)) (ad-set-arg 0 form) ad-do-it)))

;;;***

;;;### (autoloads nil nil ("breadcrumb/breadcrumb.el" "el-get/el-get-autoloading.el"
;;;;;;  "el-get/el-get-build.el" "el-get/el-get-byte-compile.el"
;;;;;;  "el-get/el-get-core.el" "el-get/el-get-custom.el" "el-get/el-get-dependencies.el"
;;;;;;  "el-get/el-get-install.el" "el-get/el-get-methods.el" "el-get/el-get-notify.el"
;;;;;;  "el-get/el-get-recipes.el" "el-get/el-get-status.el" "names/names-dev.el")
;;;;;;  (22613 60333 0 0))

;;;***

(provide '.loaddefs)
;; Local Variables:
;; version-control: never
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; .loaddefs.el ends here
