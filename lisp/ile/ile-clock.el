;;; ile-clock.el --- Integrated Legal Environment -*- lexical-binding: t -*-

;; Copyright (C) 2018  Gina White

;; Author: Gina White <ginabythebay@gmail.com>
;; Keywords: lisp
;; Version: 0.0.1

;;; Commentary:

;; Code to extract clock information in a court-friendly format

;;; Code:

;; code goes here

(require 'org)
(require 'org-clock)
(require 'seq)
(require 'subr-x)

(defun org-dblock-write:clockview (params)
  "Collect the column specification from the #+cols line according to PARAMS."
  (interactive)
  (save-restriction
    (let* ((scope (plist-get params :scope))
           (linkify (if (plist-member params :linkify)
                        (plist-get params :linkify)
                      t))
           (files (pcase scope
		    (`agenda
		     (org-agenda-files t))
		    (`agenda-with-archives
		     (org-add-archive-files (org-agenda-files t)))
		    (`file-with-archives
		     (and buffer-file-name
			  (org-add-archive-files (list buffer-file-name))))
		    ((or `nil `file `subtree `tree
			 (and (pred symbolp)
			      (guard (string-match "\\`tree\\([0-9]+\\)\\'"
						   (symbol-name scope)))))
		     (or (buffer-file-name (buffer-base-buffer))
			 (current-buffer)))
		    (_ (user-error "Unknown scope: %S" scope))))
           (block (plist-get params :block))
           (range (org-clock-special-range block))
	   table pos)
      (save-excursion
        ;; match restriction to scope
        (cond
         ((not scope))	     ;use the restriction as it is now
         ((eq scope 'file) (widen))
         ((eq scope 'subtree) (org-narrow-to-subtree))
         ((eq scope 'tree)
          (while (org-up-heading-safe))
          (org-narrow-to-subtree))
         ((and (symbolp scope)
               (string-match "\\`tree\\([0-9]+\\)\\'"
                             (symbol-name scope)))
          (let ((level (string-to-number
                        (match-string 1 (symbol-name scope)))))
            (catch 'exit
              (while (org-up-heading-safe)
                (looking-at org-outline-regexp)
                (when (<= (org-reduced-level (funcall outline-level))
                          level)
                  (throw 'exit nil))))
            (org-narrow-to-subtree))))
        (setq table (ile-clock--clockview-to-table files range linkify))
	(widen))
	(setq pos (point))
	(insert table) (org-cycle) (move-end-of-line 1)
	(goto-char pos)
	(org-table-recalculate 'all))))

(defun ile-clock--clockview-to-table (files range linkify)
  "Return a table as a string based on FILES and RANGE.

FILES can be a list of file names or a single file name.  If it
is not specified, the current file will be used.

RANGE specifies the start time and end time to include in float
time.  We expect a list where the car is start time and the cadr
is the end time, as returned by `org-clock-special-range'.  If not
specified, we will include all entries.

LINKIFY means we create links for tasks if t."
  (string-join
   (apply #'list
          (format "|%s|" (string-join (ile-clock--listify) "|"))
          "|---"
          (cl-loop for e in (ile-clock-entries files range linkify)
                   collect (format "|%s|" (string-join (ile-clock--listify e) "|"))))
   "\n"))

(defun ile-clock-entries (&optional files range linkify)
  "Get clock entries for the current buffer.

FILES can be a list of file names or a single file name.  If it
is not specified, the current file will be used.

RANGE specifies the start time and end time to include in float
time.  We expect a list where the car is start time and the cadr
is the end time, as returned by `org-clock-special-range'.  If not
specified, we will include all entries.

LINKIFY means we create links for tasks if t."
  (setq files (cond
               ((stringp files) (list files))
               ((not files) nil)
               ((listp files) files)
               (t (user-error "Unexpected files arg %s" files))))

  (sort
   (cl-loop for bf in (cond
                       ((stringp files) (list (find-file-noselect files)))
                       ((not files) (list (current-buffer)))
                       ((listp files) (mapcar #'find-file-noselect files))
                       (t (user-error "Unexpected files arg %s" files)))
            append
            (with-current-buffer bf
              (let* ((rstart (if range
                                 (float-time (car range))
                               0))
                     (rend (if range
                               (float-time (cadr range))
                             (float-time (current-time))))
                     (parser (lambda (e) (ile-clock--parse-clock rstart rend e linkify))))
                ;; list with one entry per task/date, with times summed up
                (cl-loop for e in
                         ;; alist where entries are grouped by task and date
                         (seq-group-by
                          (lambda (e) (list (plist-get e :task) (plist-get e :date)))
                          (org-element-map (org-element-parse-buffer) 'clock
                            parser nil nil))
                         collect (ile-clock--combine-entries (cdr e))))))
   (lambda (a b)
     (let ((a-date (plist-get a :date))
           (b-date (plist-get b :date))
           (a-tags (plist-get a :filetags))
           (b-tags (plist-get b :filetags)))
       (cond
        ((string< a-date b-date) 1)
        ((not (string= a-date b-date)) nil)
        ((string< a-tags b-tags) 1)
        ((not (string= a-tags b-tags)) nil)
        (t (string< (plist-get a :task) (plist-get b :task))))))))

(defun ile-clock--in-rangep (rstart rend timestamp)
  "Decides if TIMESTAMP overlaps with a range.

TIMESTAMP is a string like
\"[2019-03-12 Wed 12:02]--[2019-03-12 Wed 12:09]\",
and RSTART and REND are float times."
  (let* ((tokens (split-string timestamp "--"))
         (tstart (float-time
                  (apply #'encode-time (org-parse-time-string (car tokens)))))
         (tend (float-time
                (apply #'encode-time (org-parse-time-string (cadr tokens))))))
    (and (<= rstart tstart) (<= tend rend))))

(defun ile-clock--parse-clock (rstart rend element linkify)
  "Ingest clock ELEMENT and produces a plist of its relevant properties.

RSTART and REND represent a range of times and we only return clock entries
that overlap with that range.

LINKIFY means we create links for tasks if t."
  (when (and (equal (org-element-type element) 'clock)
             ;; Only ingest closed, inactive clock elements.
             (equal (org-element-property :status element) 'closed)
             (equal (org-element-property
                     :type (org-element-property :value element))
                    'inactive-range))
    (let* ((timestamp (org-element-property :value element))
           (duration (org-element-property :duration element))
           (start (list (org-element-property :year-start timestamp)
                        (org-element-property :month-start timestamp)
                        (org-element-property :day-start timestamp)))
           (end (list (org-element-property :year-end timestamp)
                      (org-element-property :month-end timestamp)
                      (org-element-property :day-end timestamp)))
           (task (org-element-property :raw-value (org-element-lineage element '(headline))))
           (task (if linkify
                     (let ((link (org-link-escape (concat "file:" (buffer-file-name) "::*" task))))
                       (concat "\[\[" link "\]\[" task "\]\]"))
                   task)))

      (unless (equal start end)
        (user-error
         "Task \"%s\" starts on %s and ends on %s.  We don't support tasks that span dates"
         task
         (apply #'format "%d-%02d-%02d" start)
         (apply #'format "%d-%02d-%02d" end)))
      (when (ile-clock--in-rangep rstart
                                  rend
                                  (org-element-property :raw-value timestamp))
        (list
         :filetags (format ":%s:" (string-join org-file-tags ":"))
         :task task
         :date (apply #'format "%d-%02d-%02d" start)
         :minutes (ile-clock--duration-to-minutes duration))))))

(defun ile-clock--duration-to-minutes (d)
  "Convert a duration D like \"1:23\" to 83."
  (let* ((hm (seq-map #'string-to-number (split-string d ":"))))
    (+ (* 60 (nth 0 hm))
       (nth 1 hm))))

(defun ile-clock--minutes-to-duration (m)
  "Convert minutes M like 83 to a duration like \"1:23\"."
  (let* ((hours (/ m 60))
         (minutes (% m 60))
         (hours (number-to-string hours))
         (minutes (format "%02d" minutes)))
    (string-join (list hours minutes) ":")))

(defun ile-clock--combine-entries (entries)
  "Takes a list of ENTRIES and combines them by adding their minutes together.
Assumes that all ENTRIES have the same task and start."
  (when (> (length entries) 0)
    (let ((first (car entries)))
      (list
       :filetags (plist-get first :filetags)
       :task (plist-get first :task)
       :date (plist-get first :date)
       :minutes (seq-reduce (lambda (v e) (+ v (plist-get e :minutes))) entries 0)))))

(defconst ile-clock--csv-keys (list :filetags :date :task :minutes))
(defconst ile-clock--csv-headers (list "filetags" "date" "task" "minutes"))

(defun ile-clock--listify (&optional entry)
  "Convert an ENTRY to a list for easy csv export.

If ENTRY is nil, we return a list of headers instead."
  (if entry
      (mapcar (lambda (k)
                (org-quote-csv-field
                 (format "%s" (plist-get entry k))))
              ile-clock--csv-keys)
    ile-clock--csv-headers)
  )

(defun ile-clock-export-entries (outfile &optional files range)
  "Exports clock entries in csv format.

OUTFILE is the name of the file to output to.

FILES can be a list of file names or a single file name.  If it
is not specified, the current file will be used.

RANGE specifies the start time and end time to include in float
time.  We expect a list where the car is start time and the cadr
is the end time, as returned by `org-clock-special-range'.  If not
specified, we will include all entries."
  (let ((entries (ile-clock-entries files range))
        buf)
    (with-current-buffer (find-file-noselect outfile)
      (setq buf (current-buffer))
      (erase-buffer)
      (fundamental-mode)
      (insert (string-join (ile-clock--listify) ",") "\n")
      (cl-loop for e in entries
               do (insert (string-join (ile-clock--listify e) ",") "\n"))
      (save-buffer))
    (kill-buffer buf)))

(provide 'ile-clock)
;;; ile-clock.el ends here
