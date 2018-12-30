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

(defconst ile-case-planner-buffer-name "*ile case planner*")

;;;###autoload
(defun ile-case-planner (cmd-key)
  "Run an agenda command in batch mode and send the result to STDOUT.
If CMD-KEY is a string of length 1, it is used as a key in
`org-agenda-custom-commands' and triggers this command.  If it is a
longer string it is used as a tags/todo match string.
Parameters are alternating variable names and values that will be bound
before running the agenda command.

The output gives a line for each selected agenda item.  Each
item is a list of comma-separated values, like this:

category,head,type,todo,tags,date,time,extra,priority-l,priority-n

category     The category of the item
head         The headline, without TODO kwd, TAGS and PRIORITY
type         The type of the agenda entry, can be
                todo               selected in TODO match
                tagsmatch          selected in tags match
                diary              imported from diary
                deadline           a deadline on given date
                scheduled          scheduled on given date
                timestamp          entry has timestamp on given date
                closed             entry was closed on given date
                upcoming-deadline  warning about deadline
                past-scheduled     forwarded scheduled item
                block              entry has date block including g. date
todo         The todo keyword, if any
tags         All tags including inherited ones, separated by colons
date         The relevant date, like 2007-2-14
time         The time, like 15:00-16:50
extra        Sting with extra planning info
priority-l   The priority letter if any was given
priority-n   The computed numerical priority
agenda-day   The day in the agenda where this is listed"
  (interactive "MOrg Cmd: ")
  ;; (if (> (length cmd-key) 2)
  ;;     (org-tags-view nil cmd-key)
  ;;   (let ((org-agenda-span 700))
  ;;     (org-agenda nil cmd-key)))
  (set-buffer org-agenda-buffer-name)
  (let* ((all-events (mapcar
                      (lambda (l) (org-fix-agenda-info (text-properties-at 0 l)))
                      (seq-filter
                       (lambda (l) (get-text-property 0 'org-category l))
                       (org-split-string (buffer-string) "\n"))))
	 line cases case-names)
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
    (insert "* Case Planner\n")
    (insert "Generated " (format-time-string "%B %d, %Y at %I:%M:%S %p") "\n")
    (let (cat-set (mapcar )
                  (last-cat))
      (dolist (name case-names)
        (insert "** " name "\n")
        )
      )))


(provide 'ile)
;;; ile.el ends here


