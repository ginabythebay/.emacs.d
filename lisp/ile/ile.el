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
(require 'org)
(require 'org-agenda)

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

(defconst ile-case-planner-buffer-name "*ile case planner*")

;;;###autoload
(defun ile-case-planner (&optional case-file)
  "Build a case plan for CASE-FILE.
CASE-FILE can be a single file or a list of files.  If not
specified, `org-agenda-files' will be used."
  (message "building case plan...")
  (let ((org-agenda-span 700)
        (org-agenda-files (if case-file
                              (if (listp case-file)
                                  case-file
                                (list case-file))
                            org-agenda-files)))
    (org-agenda nil "a"))
  (set-buffer org-agenda-buffer-name)
  (let* ((all-events (mapcar
                      (lambda (l) (org-fix-agenda-info (text-properties-at 0 l)))
                      (seq-filter
                       (lambda (l) (get-text-property 0 'org-category l))
                       (org-split-string (buffer-string) "\n"))))
	 cases case-names)
    (dolist (e all-events)
      (let* ((category (plist-get e 'org-category))
             (events (assoc-default category cases 'string= '())))
        (setq cases (cons (cons category (append events (list e))) cases))))
    (cl-delete-duplicates cases :key #'car :test #'string= :from-end t)
    (setq case-names (sort (mapcar 'car cases) #'string<))
    (switch-to-buffer ile-case-planner-buffer-name nil t)
    (erase-buffer)
    ;; TODO (gina) make buffer read only;  inhibit it when we need to right
    ;; TODO (gina) allow 'g' to refresh the buffer
    (org-mode)
    (insert "#+OPTIONS: toc:nil\n")
    (insert "#+HTML_HEAD: <link href='http://fonts.googleapis.com/css?family=PT+Serif' rel='stylesheet' type='text/css'>\n")
    (insert "#+HTML_HEAD: <style type=\"text/css\">p { font-family: 'PT Serif; }</style>\n")
    (insert "* Case Planner\n")
    (insert "Generated " (format-time-string "%B %d, %Y at %I:%M:%S %p") "\n")
    (dolist (name case-names)
      (insert "** " name "\n")
      (let ((rows (seq-filter
                   #'identity
                   (mapcar #'ile--case-event
                           (assoc-default name cases 'string= '())))))
        (if (not rows)
            (insert "No upcoming events found\n")
          (insert )
          (setq rows (cons 'hline rows))
          (setq rows (cons (ile--case-event nil) rows))
          (insert (orgtbl-to-orgtbl rows '(:raw t)))
          (org-return)
          ))))
  (message "building case plan...done"))

(defconst ile--case-planner-types
  (list "past-scheduled"
        "scheduled"
        "deadline"
        "timestamp"                     ; TODO-gina- remove
        "block"))

(defun ile--case-event (event)
  "Convert EVENT to a list suitable for use in a table.
If EVENT is nil, return a header.  If non-nil:
return nil if EVENT doesn't apply or a list where the elements are:
timespec,type,head,todo,tags"
  (if (not event)
      (list "*Date*" "*Time*" "*Type*" "*Event*" "*Todo*" "*Tags*")
    (when-let* ((date (plist-get event 'date))
                (type (plist-get event 'type))
                (head (plist-get event 'txt)))
      (let ((tokens (split-string head " :" nil "[ \f\t\n\r\v]+")))
        (when (> (length tokens) 1)
          (setq head (car tokens))))
      (when (member type ile--case-planner-types)
        (list (ile--pad-date date)
              (or (plist-get event 'time) "")
              type
              head
              (or (plist-get event 'todo) "")
              (plist-get event 'tags))))))

(defconst ile--case-date-re
  "\\([[:digit:]]\\{4,4\\}\\)-\\([[:digit:]]\\{1,2\\}\\)-\\([[:digit:]]\\{1,2\\}\\)")

(defun ile--pad-date (date)
  "Return DATE padded so it is always of the form YYYY-MM-DD.
If DATE is in an unexpected format, it is returned unchanged."
  (if (not (string-match ile--case-date-re date))
      date
    (let ((year (match-string 1 date))
          (month (match-string 2 date))
          (day (match-string 3 date)))
      (when (= 1 (length month)) (setq month (concat "0" month)))
      (when (= 1 (length day)) (setq day (concat "0" day)))
      (concat year "-" month "-" day))))

;; TODO(gina) make this generic.  Currently I keep rewriting the same thing
(defun ile-next-wit-file ()
  "Yes."
  (interactive)

  (org-next-visible-heading 1)
  (right-char 3)
  (let ((f (buffer-substring-no-properties (point) (line-end-position))))
    (next-line 2)
    (end-of-line)
    (insert " ")
    (other-window 1)
    (find-file f)
    (other-window 1)))


(provide 'ile)
;;; ile.el ends here


