((aggressive-indent-mode status "installed" recipe
			 (:name aggressive-indent-mode :description "Emacs minor mode that keeps your code always indented. More reliable than electric-indent-mode" :website "https://github.com/Malabarba/aggressive-indent-mode" :type github :depends
				(names cl-lib)
				:pkgname "Malabarba/aggressive-indent-mode"))
 (breadcrumb status "installed" recipe
	     (:name breadcrumb :website "http://breadcrumbemacs.sourceforge.net/" :description "Breadcrumb is an add-on module for Emacs that allows you to set a series of quick bookmarks in the file buffers, and jump back to them quickly." :type http-zip :url "http://downloads.sourceforge.net/project/breadcrumbemacs/Breadcrumb%20for%20Emacs/1.1.3/breadcrumb-1.1.3.zip"))
 (cl-lib status "installed" recipe
	 (:name cl-lib :builtin "24.3" :type elpa :description "Properly prefixed CL functions and macros" :url "http://elpa.gnu.org/packages/cl-lib.html"))
 (el-get status "installed" recipe
	 (:name el-get :website "https://github.com/dimitri/el-get#readme" :description "Manage the external elisp bits and pieces you depend upon." :type github :branch "master" :pkgname "dimitri/el-get" :info "." :compile
		("el-get.*\\.el$" "methods/")
		:features el-get :post-init
		(when
		    (memq 'el-get
			  (bound-and-true-p package-activated-list))
		  (message "Deleting melpa bootstrap el-get")
		  (unless package--initialized
		    (package-initialize t))
		  (when
		      (package-installed-p 'el-get)
		    (let
			((feats
			  (delete-dups
			   (el-get-package-features
			    (el-get-elpa-package-directory 'el-get)))))
		      (el-get-elpa-delete-package 'el-get)
		      (dolist
			  (feat feats)
			(unload-feature feat t))))
		  (require 'el-get))))
 (names status "installed" recipe
	(:name names :description "A Namespace implementation for Emacs-Lisp" :website "https://github.com/Bruce-Connor/names" :type github :depends cl-lib :pkgname "Bruce-Connor/names")))
