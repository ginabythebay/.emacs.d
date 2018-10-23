;;; ile-link.el --- Integrated Legal Environment                      -*- lexical-binding: t; -*-

;; Copyright (C) 2018  Gina White

;; Author: Gina White <ginabythebay@gmail.com>
;; Keywords: lisp
;; Version: 0.0.1

;;; Commentary:

;; Code for org links.  PDFs only for now

;;; Code:

;; code goes here

(require 'org)
(require 'pdf-view)


(defun ile-link-store-pdf ()
  "Store a link to a pdf."
  (when (equal major-mode 'pdf-view-mode)
    (let* ((page (number-to-string (pdf-view-current-page)))
           (file (buffer-file-name))
           (link (concat "pdf:" page ":" file))
           (title (format "%s, page %s" (file-name-nondirectory file) page)))
      (org-store-link-props
       :type "pdf"
       :link link
       :description title))))

(defun ile-link-follow-pdf (link)
  "Follow pdf LINK."
  (unless (string-match "\\([0-9]*\\):\\(.*\\)" link)
    (user-error "Could not interpret %S.  We expect to see <pageno>:<filename>" link))
  (let ((page (string-to-number (match-string 1 link)))
        (file (match-string 2 link)))
    (find-file file)
    (pdf-view-goto-page page)))

(org-link-set-parameters "pdf"
                         :store #'ile-link-store-pdf
                         :follow #'ile-link-follow-pdf)


(provide 'ile-link)
;;; ile-link.el ends here
