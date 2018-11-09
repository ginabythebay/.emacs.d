;;; ile-org-noter.el --- Integrated Legal Environment -*- lexical-binding: t -*-

;; Copyright (C) 2018  Gina White

;; Author: Gina White <ginabythebay@gmail.com>
;; Keywords: lisp
;; Version: 0.0.1

;;; Commentary:

;; extending/overriding org-noter

;;; Code:

;; code goes here

(require 'bates)
(require 'cl-lib)
(require 'let-alist)
(require 'org-noter)

(defconst ile-pdf-date-regexp
  (eval-when-compile
    (concat
     "\\b"
     ;; e.g. August 13, 1984
     "([a-zA-Z]+\\s+\\d{1,2}(,|\\s)\\s*\\d{4})"
     ;; e.g. August 06 , 1984
     "|"
     "([a-zA-Z]+\\s+\\d{1,2}\\s*,\\s+\\d{4})"
     ;; e.g. 03/04/2014
     "|"
     "(\\d{1,2}/\\d{1,2}/\\d{4})"
     ;; e.g. 04 Jan 16
     "|"
     "(\\d{1,2}\\s+[a-zA-Z]{3,}\\s+(\\d{2}|\\d{4}))"
     "\\b"
     )))

(defun ile--find-one-match (text)
  "Find first span within TEXT with match face or blank string if nothing found."
  (let ((start (next-single-property-change 0 'face text))
        (end))
    (when start
      (setq end (next-single-property-change start 'face text)))
    (unless start
      (setq start 0))
    (substring text start end)))

;;;###autoload
(defun ile-org-noter-next-page ()
  "Move the doc buffer to the next page."
  (interactive)
  (org-noter--with-valid-session
   (let ((location-cons (org-noter--doc-approx-location)))
     (setf (car location-cons) (1+ (car location-cons)))
     (org-noter--doc-goto-location location-cons))))

;;;###autoload
(defun ile-org-noter-prev-page ()
  "Move the doc buffer to the previous page."
  (interactive)
  (org-noter--with-valid-session
   (let ((location-cons (org-noter--doc-approx-location)))
     (setf (car location-cons) (1- (car location-cons)))
     (org-noter--doc-goto-location location-cons))))

;;;###autoload
(defun ile-org-noter-dates ()
  "Find dates in the current document page and prompts the user to pick one, \
then insert it in the notes buffer."
  (interactive)
  (org-noter--with-valid-session
   (let ((doc-buffer (org-noter--session-doc-buffer session)))
     (with-current-buffer doc-buffer
       (let* ((page (car (org-noter--doc-approx-location)))
              (all-results (pdf-info-search-regexp
                            ile-pdf-date-regexp
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


;;;###autoload
(defun ile-duplicate ()
  "Prompt the user for an entry and treat the current entry as a duplicate of that."
  (interactive)
  (org-noter--with-valid-session
   (with-current-buffer (org-noter--session-notes-buffer session)
     (let ((chosen))
       (save-excursion
         (org-back-to-heading 1)

         (let ((choices
                (cl-loop with page-no = 0
                         with description = ""
                         while t do
                           (org-previous-visible-heading 1)
                           (setq page-no (org-entry-get nil "NOTER_PAGE"))
                           (setq description (org-entry-get nil "DESCRIPTION"))
                         if (not page-no)
                           return temp
                         else
                          if (not (string-prefix-p "dup of" description t))
                            collect (format
                                     "Dup of %s %s|%s"
                                     (org-entry-get nil "BATES_START")
                                     description
                                     (org-entry-get nil "DATE")) into temp)))
           (setq chosen (completing-read "Duplicate of" choices))))

       (if (not (string-match "^\\(.*\\)|\\(.*\\)$" chosen))
           (error "Internal error parsing %s" chosen)
         (let ((description (match-string 1 chosen))
               (date (match-string 2 chosen)))
         (org-entry-put nil "DESCRIPTION" description)
         (org-entry-put nil "DATE" date)))))))

;;;###autoload
(defun ile-insert-and-dup ()
  "Run bates-insert-note and then ile-duplicate."
  (interactive)
  (org-noter--with-valid-session
   (save-excursion
     (bates-insert-note)
     (ile-duplicate)
     (pop-to-buffer (org-noter--session-doc-buffer session)))))

(provide 'ile-org-noter)
;;; ile-org-noter.el ends here
