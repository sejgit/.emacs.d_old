;;; init-swiper.el --- init swiper and add functions
;;; Commentary:

;;; Log:
;; 2017 01 11 init & move swiper from init-misc-pkgs.el

;;; Code:

;; swiper
(use-package swiper
  :ensure t
  :functions ivy--regex
  :bind
  (("C-s" . swiper)
   ("C-x C-d" . sej/ivy-dired-recent-dirs))
  :config
  ;; open recent directory, requires ivy (part of swiper)
  ;; borrows from http://stackoverflow.com/questions/23328037/in-emacs-how-to-maintain-a-list-of-recent-directories
  (defun sej/ivy-dired-recent-dirs ()
    "Present a list of recently used directories and open the selected one in dired"
    (interactive)
    (let ((recent-dirs
	   (delete-dups
	    (mapcar (lambda (file)
		      (if (file-directory-p file) file (file-name-directory file)))
		    recentf-list))))
      (let ((dir (ivy-read "Directory: "
			   recent-dirs
			   :re-builder #'ivy--regex
			   :sort nil
			   :initial-input nil)))
	(dired dir)))))

(provide 'init-swiper)
;;; init-swiper.el ends here

