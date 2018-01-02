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
;; 2017 09 04 move indent-guide, page-break-lines, whitespace-cleanup-mode from init-misc-pkgs.el
;; 2017 09 07 move YAML mode to init-misc-filetypes.el

;;; Code:

(use-package writegood-mode
  :ensure t)

;; show vertical lines to guide indentation
(use-package indent-guide
  :ensure t
  :hook (prog-mode . indent-guide-mode)
  :diminish
  indent-guide-mode)

;; display ^L page breaks as tidy horizontal lines
(use-package page-break-lines
  :ensure t
  :config
  (setq global-page-break-lines-mode t)
  :diminish
  psge-break-lines-mode)

;; intelligently call whitespace-cleanup on save
(use-package whitespace-cleanup-mode
  :ensure t
  ;; intelligently call whitespace-cleanup on save
  :config
  (global-whitespace-cleanup-mode t))

;; markdown-mode used a lot on Github
(use-package markdown-mode
  :ensure t
  :functions writegood-mode
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

;; Elasticsearch uses asciidoc everywhere for documentation
(use-package adoc-mode
  :defer t
  :ensure t
  )

;; skeletons are a kind of yasnippet but they don't mess with keybindings
(use-package skeleton
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
  :hook (after-init . abbrev-mode)
  :diminish abbrev-mode
  :config
  (define-abbrev-table
    'global-abbrev-table
    '(("ooc" "out of curiosity" nil 0)))
  (define-abbrev-table
    'org-mode-abbrev-table
    '(("<el" "" 'eos/org-wrap-elisp 0))))


;; Let's say you have a list like:
;; First Item
;; Second Item
;; Third Item
;; Fourth Item
;; And you want to number it to look like:
;; 1. First Item
;; 2. Second Item
;; 3. Third Item
;; 4. Fourth Item
;; This function allows you to hit C-x r N and specify the pattern and starting offset to number lines in rectangular-selection mode:
(defun number-rectangle (start end format-string from)
  "Delete text in the region-rectangle, then number it."
  (interactive
   (list (region-beginning) (region-end)
         (read-string "Number rectangle: "
                      (if (looking-back "^ *" nil nil) "%d. " "%d"))
         (read-number "From: " 1)))
  (save-excursion
    (goto-char start)
    (setq start (point-marker))
    (goto-char end)
    (setq end (point-marker))
    (delete-rectangle start end)
    (goto-char start)
    (loop with column = (current-column)
          while (and (<= (point) end) (not (eobp)))
          for i from from   do
          (move-to-column column t)
          (insert (format format-string i))
          (forward-line 1)))
  (goto-char start))

(define-key sej-mode-map (kbd "C-x r N") 'number-rectangle)


(provide 'init-writing)
;;; init-writing.el ends here


