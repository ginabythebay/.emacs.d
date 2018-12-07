;;; ile-org-noter.el --- Integrated Legal Environment  -*- lexical-binding: t -*-

;; Copyright (C) 2018  Gina White

;; Author: Gina White <ginabythebay@gmail.com>
;; Keywords: lisp
;; Version: 0.0.1

;;; Commentary:

;; General stuff for ile

;;; Code:

;; code goes here

(require 'ile-navigation)

(defconst ile--exclude-bates-names
  '("CA"  ;; our fair state
    "SBN" ;; state bar number
    ;; todo keywords
    "TODO" "NEXT" "DONE" "WAITING" "HOLD" "CANCELLED" "PHONE" "MEETING"))

(defun ile--bates-matcher (limit)
  "Decide if we match a potential bates value, for font locking.

LIMIT is the limit of the search."
  (when (search-forward-regexp ile-org--bates-re limit t)
    (let ((name (match-string 1)))
      (unless (member name ile--exclude-bates-names)
        t))))

(defmacro ile--highlights (matcher)
  "Create an entry for 'font-lock-keywords based on the regular expression MATCHER."
  `(list ,matcher 0 font-lock-comment-face 'prepend))

(defconst ile--keywords
  (list
   (list 'ile--bates-matcher 0 font-lock-comment-face 'prepend)
   ;; avoid highlighting dates that are part of clock entries or
   ;; scheduling entries
   (ile--highlights (concat "[^[<]" ile-org--date-re))
   (ile--highlights ile-org--fed-rule-re)))

;;;###autoload
(define-minor-mode ile-mode
  "Integrated Legal Environment."
  nil " ile" nil

  (if ile-mode
      (font-lock-add-keywords nil ile--keywords t)
    (font-lock-remove-keywords nil ile--keywords))
  (if (fboundp #'font-lock-flush)
      (font-lock-flush)
    (when font-lock-mode
      (with-no-warnings (font-lock-fontify-buffer)))))


;;;###autoload
(defun ile-tbl-to-clipboard ()
  "Copy data in the clipboard to OS clipboard and kill ring.

Similar `ORG-TABLE-EXPORT', but instead of exporting to a file,
copy the data in the table to the OS clipboard (and to the kill
ring) formatted according to user's choice, where the format
choices are the same as org-table-export."
  (interactive)
  (unless (org-at-table-p) (user-error "No table at point"))
  (let* ((format
          (completing-read "Transform table function: "
                           '("orgtbl-to-tsv"
                             "orgtbl-to-csv" "orgtbl-to-latex"
                             "orgtbl-to-html" "orgtbl-to-generic"
                             "orgtbl-to-texinfo" "orgtbl-to-orgtbl"
                             "orgtbl-to-unicode"))))
    (if (string-match "\\([^ \t\r\n]+\\)\\( +.*\\)?" format)
        (progn
          (message "Copying data...")
          (let ((transform (intern (match-string 1 format)))
                (params (and (match-end 2)
                             (read (concat "(" (match-string 2 format) ")"))))
                (table (org-table-to-lisp
                        (buffer-substring-no-properties
                         (org-table-begin) (org-table-end)))))
            (unless (fboundp transform)
              (user-error "No such transformation function %s" transform))
            (kill-new (funcall transform table params))
            (message "Copying data...done")))
      (user-error "Table export format invalid"))))

(defun ile-entry-when-sort-key ()
  "Return a key we can use for sorting the current entry.

We look at the timestamp for the special SCHEDULED property or
the special DEADLINE property.  If only one is found, we use
that.  If neither is found, we return a blank string.  It is not
possible for both to be found because org mode only recognizes
the first entry after the heading."
  (or (org-entry-get (point) "DEADLINE")
      (org-entry-get (point) "SCHEDULED")
      ""))

;;;###autoload
(defun ile-sort-entries-by-date ()
  "Sort entries by SCHEDULED or DEADLINE special property.
See `org-sort-entries'."
  (interactive)
  (org-sort-entries nil ?f #'ile-entry-when-sort-key #'string<))

(provide 'ile)
;;; ile.el ends here


