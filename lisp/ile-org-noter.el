;;; ile-org-noter.el --- Integrated Legal Environment

;; Copyright (C) 2018  Gina White

;; Author: Gina White <ginabythebay@gmail.com>
;; Keywords: lisp
;; Version: 0.0.1

;;; Commentary:

;; extending/overriding org-noter

;;; Code:

;; code goes here

(require 'org-noter)

(require 'let-alist)

(defun ile--find-one-match (text)
  "Find first span within TEXT with match face or blank string if nothing found."
  (let ((start (next-single-property-change 0 'face text))
        (end))
    (when start
      (setq end (next-single-property-change start 'face text)))
    (unless start
      (setq start 0))
    (substring text start end)))

(defun ile-org-noter-dates ()
  "Find dates in the current document page and prompts the user to pick one, \
then insert it in the notes buffer."
  (interactive)
  (org-noter--with-valid-session
   (let ((doc-buffer (org-noter--session-doc-buffer session)))
     (with-current-buffer doc-buffer
       (let* ((page (car (org-noter--doc-approx-location)))
              (all-results (pdf-info-search-regexp
                            "\\b([a-zA-Z]+\\s+\\d{1,2},?\\s*\\d{4})|(\\d{1,2}/\\d{1,2}/\\d{4})\\b"
                            ;;"[a-zA-Z]+\\s+\\d{1,2},?\\s*\\d{4}"
                            page
                            'invalid-regexp
                            doc-buffer))
              (all-date-texts
               (mapcar
                (lambda (x)
                  (ile--find-one-match (let-alist x .text)))
                all-results)))

         (let ((choice
                (completing-read
                 (format "date for page %d" page)
                 (push "undated" all-date-texts))))
           (with-current-buffer (org-noter--session-notes-buffer session)
             (insert (if (string= choice "undated")
                        "undated"
                      (org-read-date nil nil choice))))))))))


(provide 'ile-org-noter)
;;; ile-org-noter.el ends here
