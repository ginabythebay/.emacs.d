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
(require 'seq)
(require 'subr-x)

(defun ile-clock--parse-clock (element)
  "Ingest clock ELEMENT and produces a plist of its relevant properties."
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
           (task (org-element-property :raw-value (org-element-lineage element '(headline)))))
      (unless (equal start end)
        (user-error
         "Task \"%s\" starts on %s and ends on %s.  We don't support tasks that span dates"
         task
         (apply #'format "%d-%02d-%02d" start)
         (apply #'format "%d-%02d-%02d" end)))
      (list
       :task task
       :date (apply #'format "%d-%02d-%02d" start)
       :minutes (ile-clock--duration-to-minutes duration)))))

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
       :task (plist-get first :task)
       :date (plist-get first :date)
       :minutes (seq-reduce (lambda (v e) (+ v (plist-get e :minutes))) entries 0)))))

;; TODO(gina)
;;   - expand this to optionally take a list of files to draw from.
;;   - Also allow it to accept a value like 'lastweek to narrow the results.  See
;;      org-clock-special-range
(defun ile-clock-entries ()
  "Get clock entries for the current buffer."
  (sort
   ;; list with one entry per task/date, with times summed up
   (cl-loop for e in
            ;; alist where entries are grouped by task and date
            (seq-group-by
             (lambda (e) (list (plist-get e :task) (plist-get e :date)))
             (org-element-map (org-element-parse-buffer) 'clock
               #'ile-clock--parse-clock nil nil))
            collect (ile-clock--combine-entries (cdr e)))
   (lambda (a b)
     (let ((a-date (plist-get a :date))
           (b-date (plist-get b :date)))
       (or (string< a-date b-date)
           (and (string= a-date b-date)
                (string< (plist-get a :task) (plist-get b :task))))))))

(defun ile-clock-test-current-file ()
  "Test of clock parser."
  (interactive)
  (cl-loop for e in
           (ile-clock-entries)
           do (message "%s %s %s"
                       (plist-get e :date)
                       (plist-get e :task)
                       (ile-clock--minutes-to-duration (plist-get e :minutes)))))

(provide 'ile-clock)
;;; ile-clock.el ends here
