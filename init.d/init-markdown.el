;;; init-writing.el --- Initialize emacs markdown-mode

;;; Commentary:
;; for editing of markdown files & writing helpers

;;; ChangeLog:
;; 2016 12 16 init SeJ
;; 2017 01 06 change from req-package to use-package
;; 2017 04 04 remove ensure went global ; defer not required for mode,bind,int
;; 2017 08 25 add auto-fill-mode from EOS
;; 2017 09 04 change to init-writing.el & add yaml, elasticsearch, skeleton, abbrev, thesaurus
;;            numbering rectangles, writing/viewing helpers, highlighting indentation


;;; Code:

;; insert more often than you think
(defun lod ()
  "Well. This is disappointing."
  (interactive)
  (insert "ಠ_ಠ"))

(define-key sej-mode-map (kbd "s-l") #'lod)

;; markdown-mode used a lot on Github
(use-package markdown-mode
  :ensure t
  :defer t
  :mode
  (("\\`README\\.md\\'" . gfm-mode)
   ("github\\.com.*\\.txt\\'" . gfm-mode)
   ("\\.md\\'"          . markdown-mode)
   ("\\.markdown\\'"    . markdown-mode))
  :config
  (setq markdown-enable-wiki-links t
        markdown-italic-underscore t
        markdown-make-gfm-checkboxes-buttons t
        markdown-gfm-additional-languages '("sh"))
  (add-hook 'markdown-mode-hook
	    (lambda ()
	      (visual-line-mode t)
	      (writegood-mode t)
	      (flyspell-mode t)
	      (auto-fill-mode t)))
  (setq markdown-command "pandoc --smart -f markdown -t html"))

;; YAML support
(use-package yaml-mode
  :defer t
  :ensure t)

;; Elasticsearch uses asciidoc everywhere for documentation
(use-package adoc-mode
  :defer t
  :ensure t
  )

;; skeletons are a kind of yasnippet but they don't mess with keybindings
(use-package skeleton
  :ensure nil
  :config
  (define-skeleton sej/org-header
    "Insert a standard header for org-mode files"
    "Title: "
    "#+TITLE: " str \n
    "#+AUTHOR: " (user-full-name) \n
    "#+EMAIL: " user-mail-address \n
    "#+SETUPFILE: ~/eos/setupfiles/default.setup

| *Author* | {{{author}}} ({{{email}}})    |
| *Date*   | {{{time(%Y-%m-%d %H:%M:%S)}}} |

* Introduction
" \n)

  (define-skeleton eos/org-wrap-elisp
    "Wrap text with #+BEGIN_SRC / #+END_SRC for the emacs-lisp code"
    nil
    > "#+BEGIN_SRC emacs-lisp" \n
    > _ \n
    > "#+END_SRC" \n)

  (define-skeleton eos/org-wrap-source
    "Wrap text with #+BEGIN_SRC / #+END_SRC for a code type"
    "Language: "
    > "#+BEGIN_SRC " str \n
    > _ \n
    > "#+END_SRC" \n)

  (define-skeleton eos/es-make-index
    "Insert boilerplate to create an index with `es-mode' syntax"
    "Index name: "
    "POST /" str \n
    "{" \n
    > "\"settings\": {" \n
    > "\"index\": {" \n
    > "\"number_of_shards\": 1," \n
    > "\"number_of_replicas\": 0" \n
    > "}" \n ;; index
    > "}," \n ;; settings
    > "\"mappings\": {" \n
    > "\"" (skeleton-read "Type name: ") "\": {" \n
    > "\"properties\": {" \n
    > "\"body\": {" \n
    > "\"type\": \"string\"" \n
    > "}" \n ;; body
    > "}" \n ;; properties
    > "}" \n ;; type
    > "}" \n ;; mappings
    > "}" \n))

;; for inserting abbreviations
(use-package abbrev
  :ensure nil
  :init (add-hook 'after-init-hook 'abbrev-mode)
  :diminish abbrev-mode
  :config
  (define-abbrev-table
    'global-abbrev-table
    '(("ooc" "out of curiosity" nil 0)))
  (define-abbrev-table
    'org-mode-abbrev-table
    '(("<el" "" 'eos/org-wrap-elisp 0))))

;; synaurus is bound to C-c s but that is my keymap so adding H-t
(use-package synosaurus
  :ensure t
  :defer 15
  :defines sej-mode-map
  :bind (:map sej-mode-map
	      ("H-t" . synosaurus-lookup))
  :init
  (setq-default synosaurus-backend 'synosaurus-backend-wordnet)
  (add-hook 'after-init-hook #'synosaurus-mode))

(provide 'init-markdown)
;;; init-markdown.el ends here


