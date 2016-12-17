((aggressive-indent-mode status "installed" recipe
			 (:name aggressive-indent-mode :description "Emacs minor mode that keeps your code always indented. More reliable than electric-indent-mode" :website "https://github.com/Malabarba/aggressive-indent-mode" :type github :depends
				(names cl-lib)
				:pkgname "Malabarba/aggressive-indent-mode"))
 (arduino-mode status "installed" recipe
	       (:name arduino-mode :website "https://github.com/bookest/arduino-mode" :description "Emacs major mode for Arduino development." :type github :pkgname "bookest/arduino-mode"))
 (auto-complete status "installed" recipe
		(:name auto-complete :website "https://github.com/auto-complete/auto-complete" :description "The most intelligent auto-completion extension." :type github :pkgname "auto-complete/auto-complete" :depends
		       (popup fuzzy)
		       :features auto-complete-config :post-init
		       (progn
			 (add-to-list 'ac-dictionary-directories
				      (expand-file-name "dict" default-directory))
			 (ac-config-default))))
 (breadcrumb status "installed" recipe
	     (:name breadcrumb :website "http://breadcrumbemacs.sourceforge.net/" :description "Breadcrumb is an add-on module for Emacs that allows you to set a series of quick bookmarks in the file buffers, and jump back to them quickly." :type http-zip :url "http://downloads.sourceforge.net/project/breadcrumbemacs/Breadcrumb%20for%20Emacs/1.1.3/breadcrumb-1.1.3.zip"))
 (cl-lib status "installed" recipe
	 (:name cl-lib :builtin "24.3" :type elpa :description "Properly prefixed CL functions and macros" :url "http://elpa.gnu.org/packages/cl-lib.html"))
 (csharp-mode status "installed" recipe
	      (:name csharp-mode :description "C# mode derived mode." :type github :pkgname "josteink/csharp-mode"))
 (dash status "installed" recipe
       (:name dash :description "A modern list api for Emacs. No 'cl required." :type github :pkgname "magnars/dash.el"))
 (deferred status "installed" recipe
   (:name deferred :description "Simple asynchronous functions for emacs lisp." :type github :pkgname "kiwanami/emacs-deferred"))
 (deft status "installed" recipe
   (:name deft :description "Quickly browse, filter, and edit plain text notes." :type git :url "https://github.com/jrblevin/deft.git"))
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
 (emacs-async status "installed" recipe
	      (:name emacs-async :description "Simple library for asynchronous processing in Emacs" :type github :pkgname "jwiegley/emacs-async"))
 (epl status "installed" recipe
      (:name epl :description "EPL provides a convenient high-level API for various package.el versions, and aims to overcome its most striking idiocies." :type github :pkgname "cask/epl"))
 (flycheck status "installed" recipe
	   (:name flycheck :type github :pkgname "flycheck/flycheck" :minimum-emacs-version "24.3" :description "On-the-fly syntax checking extension" :depends
		  (dash pkg-info let-alist seq)))
 (flycheck-pos-tip status "installed" recipe
		   (:name flycheck-pos-tip :type github :pkgname "flycheck/flycheck-pos-tip" :description "Flycheck errors display in tooltip" :depends
			  (pos-tip flycheck)
			  :post-init
			  (setq-default flycheck-display-errors-function 'flycheck-pos-tip-error-messages)))
 (flyspell status "installed" recipe
	   (:name flyspell :website "http://www-sop.inria.fr/members/Manuel.Serrano/flyspell/flyspell.html" :description "On-the-fly spell checker." :type http :url "http://www-sop.inria.fr/members/Manuel.Serrano/flyspell/flyspell-1.7q.el"))
 (fuzzy status "installed" recipe
	(:name fuzzy :website "https://github.com/auto-complete/fuzzy-el" :description "Fuzzy matching utilities for GNU Emacs" :type github :pkgname "auto-complete/fuzzy-el"))
 (gh status "installed" recipe
     (:name gh :description "Github API client libraries" :type github :pkgname "sigma/gh.el" :depends
	    (pcache logito request marshal)
	    :autoloads nil))
 (gist status "installed" recipe
       (:name gist :type github :pkgname "defunkt/gist.el" :depends
	      (gh tabulated-list)
	      :description "Emacs integration for gist.github.com" :website "http://github.com/defunkt/gist.el"))
 (goto-chg status "installed" recipe
	   (:name goto-chg :description "Goto the point of the most recent edit in the buffer." :type emacswiki))
 (ht status "installed" recipe
     (:name ht :website "https://github.com/Wilfred/ht.el" :description "The missing hash table utility library for Emacs." :type github :pkgname "Wilfred/ht.el"))
 (ido-ubiquitous status "installed" recipe
		 (:name ido-ubiquitous :description "Use ido (nearly) everywhere" :website "https://github.com/DarwinAwardWinner/ido-ubiquitous" :type github :depends
			(cl-lib s)
			:pkgname "DarwinAwardWinner/ido-ubiquitous"))
 (let-alist status "installed" recipe
	    (:name let-alist :description "Easily let-bind values of an assoc-list by their names." :builtin "25.0.50" :type elpa :url "https://elpa.gnu.org/packages/let-alist.html"))
 (log4e status "installed" recipe
	(:name log4e :website "https://github.com/aki2o/log4e" :description "provide logging framework for elisp." :type github :pkgname "aki2o/log4e"))
 (logito status "installed" recipe
	 (:name logito :type github :pkgname "sigma/logito" :description "logging library for Emacs" :website "http://github.com/sigma/logito"))
 (magit status "installed" recipe
	(:name magit :website "https://github.com/magit/magit#readme" :description "It's Magit! An Emacs mode for Git." :type github :pkgname "magit/magit" :branch "master" :minimum-emacs-version "24.4" :depends
	       (dash with-editor emacs-async)
	       :info "Documentation" :load-path "lisp/" :compile "lisp/" :build
	       `(("make" ,(format "EMACSBIN=%s" el-get-emacs)
		  "docs")
		 ("touch" "lisp/magit-autoloads.el"))
	       :build/berkeley-unix
	       `(("gmake" ,(format "EMACSBIN=%s" el-get-emacs)
		  "docs")
		 ("touch" "lisp/magit-autoloads.el"))
	       :build/windows-nt
	       (with-temp-file "lisp/magit-autoloads.el" nil)))
 (markdown-mode status "installed" recipe
		(:name markdown-mode :description "Major mode to edit Markdown files in Emacs" :website "http://jblevins.org/projects/markdown-mode/" :type github :pkgname "jrblevin/markdown-mode" :prepare
		       (add-to-list 'auto-mode-alist
				    '("\\.\\(md\\|mdown\\|markdown\\)\\'" . markdown-mode))))
 (marshal status "installed" recipe
	  (:name marshal :description "EIEIO marshalling, inspired by Go tagged structs." :type github :pkgname "sigma/marshal.el" :depends
		 (ht)))
 (names status "installed" recipe
	(:name names :description "A Namespace implementation for Emacs-Lisp" :website "https://github.com/Bruce-Connor/names" :type github :depends cl-lib :pkgname "Bruce-Connor/names"))
 (paredit status "installed" recipe
	  (:name paredit :description "Minor mode for editing parentheses" :type github :prepare
		 (progn
		   (autoload 'enable-paredit-mode "paredit")
		   (autoload 'disable-paredit-mode "paredit"))
		 :pkgname "emacsmirror/paredit"))
 (pcache status "installed" recipe
	 (:name pcache :description "persistent caching for Emacs" :type github :pkgname "sigma/pcache"))
 (pkg-info status "installed" recipe
	   (:name pkg-info :description "Provide information about Emacs packages." :type github :pkgname "lunaryorn/pkg-info.el" :depends
		  (dash epl)))
 (popup status "installed" recipe
	(:name popup :website "https://github.com/auto-complete/popup-el" :description "Visual Popup Interface Library for Emacs" :type github :submodule nil :depends cl-lib :pkgname "auto-complete/popup-el"))
 (pos-tip status "installed" recipe
	  (:name pos-tip :description "Show tooltip at point" :type emacswiki))
 (rainbow-delimiters status "installed" recipe
		     (:name rainbow-delimiters :website "https://github.com/Fanael/rainbow-delimiters#readme" :description "Color nested parentheses, brackets, and braces according to their depth." :type github :pkgname "Fanael/rainbow-delimiters"))
 (req-package status "installed" recipe
	      (:name req-package :description "req-package is a macro wrapper on top of use-package. It's goal is to simplify package dependencies management, when using use-package for your .emacs." :type github :features
		     (req-package)
		     :depends
		     (use-package dash log4e)
		     :pkgname "edvorg/req-package"))
 (request status "installed" recipe
	  (:name request :description "Easy HTTP request for Emacs Lisp" :type github :submodule nil :pkgname "abingham/emacs-request" :depends
		 (deferred)
		 :provide
		 (request-deferred)))
 (restclient status "installed" recipe
	     (:name restclient :description "HTTP REST client tool for emacs" :type github :pkgname "pashky/restclient.el"))
 (s status "installed" recipe
    (:name s :description "The long lost Emacs string manipulation library." :type github :pkgname "magnars/s.el"))
 (seq status "installed" recipe
      (:name seq :description "Sequence manipulation library for Emacs" :builtin "25" :type github :pkgname "NicolasPetton/seq.el"))
 (slime status "installed" recipe
	(:name slime :description "Superior Lisp Interaction Mode for Emacs" :type github :autoloads "slime-autoloads" :info "doc" :pkgname "slime/slime" :depends cl-lib :load-path
	       ("." "contrib")
	       :build
	       '(("sed" "-i" "s/@itemx INIT-FUNCTION/@item INIT-FUNCTION/" "doc/slime.texi")
		 ("make" "-C" "doc" "slime.info"))
	       :build/darwin
	       '(("make" "-C" "doc" "slime.info"))
	       :build/berkeley-unix
	       '(("gmake" "-C" "doc" "slime.info"))
	       :post-init
	       (slime-setup)))
 (smex status "installed" recipe
       (:name smex :description "M-x interface with Ido-style fuzzy matching." :type github :pkgname "nonsequitur/smex" :features smex :post-init
	      (smex-initialize)))
 (tabulated-list status "installed" recipe
		 (:name tabulated-list :type github :pkgname "sigma/tabulated-list.el" :description "generic major mode for tabulated lists." :website "http://github.com/sigma/tabulated-list.el"))
 (use-package status "installed" recipe
	      (:name use-package :type github :description "A use-package declaration for simplifying your .emacs" :pkgname "jwiegley/use-package"))
 (web-mode status "installed" recipe
	   (:name web-mode :description "emacs major mode for editing PHP/JSP/ASP HTML templates (with embedded CSS and JS blocks)" :type github :pkgname "fxbois/web-mode"))
 (with-editor status "installed" recipe
	      (:name with-editor :description "Use the Emacsclient as $EDITOR" :type github :pkgname "magit/with-editor"))
 (writegood status "installed" recipe
	    (:name writegood :type github :description "Polish up poor writing on the fly." :pkgname "bnbeckwith/writegood-mode"))
 (yaml-mode status "installed" recipe
	    (:name yaml-mode :description "Simple major mode to edit YAML file for emacs" :type github :pkgname "yoshiki/yaml-mode")))
