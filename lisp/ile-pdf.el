;;; ile-pdf.el --- Integrated Legal Environment

;; Copyright (C) 2018  Gina White

;; Author: Gina White <ginabythebay@gmail.com>
;; Keywords: lisp
;; Version: 0.0.1

;;; Commentary:

;; Code to manipulate pdfs

;;; Code:

;; code goes here

(require 'cl-lib)

(defgroup ile-pdf nil
  "Separate PDF pages."
  :group 'convenience)

(defcustom ile-pdf-pdftk (executable-find "pdftk")
  "Path to pdftk executable."
  :group 'ile-pdf-separate
  :type 'string)

(defconst ile-pdf--output-buffer "*ile-pdf*")

(defun ile-pdf--extract-pages-args (in-file page-ranges out-file)
  "Build the arguments portion of the command to extract one or more pages.
from IN-FILE and put them into OUT-FILE.  OUT-FILE will be
overwritten if it already exists.  PAGE-RANGES is expected to be
a list and defines which pages to extract.  Each entry of
PAGE-RANGES should be a cons cell with a start page and an end
page."
  (let ((range-entries
          (cl-loop
           for entry in page-ranges
           collect (format "%s-%s" (car entry) (cdr entry))
           ))
        )
    (append (list in-file "cat") range-entries (list "output" out-file))))

(ert-deftest ile-pdf--extract-pages-args ()
  "Test argument creation."
  (should (equal
           '("foo" "cat" "1-13" "15-20" "output" "bar")
           (ile-pdf--extract-pages-args
            "foo"
            '( (1 . 13) (15 . 20))
            "bar"))))

(defun ile-pdf--extract-pages (in-file page-ranges out-file)
  "Extract one or more pages from IN-FILE and put them into OUT-FILE.
OUT-FILE will be overwritten if it already exists.
PAGE-RANGES is expected to be a list and defines which pages to extract.
Each entry of PAGE-RANGES should be a cons cell with a start page and
an end page."
  (when (get-buffer ile-pdf--output-buffer)
    (kill-buffer ile-pdf--output-buffer))

  (let* ((args (ile-pdf--extract-pages-args in-file page-ranges out-file))
         (status (apply
                  #'call-process
                  ile-pdf-pdftk
                  nil
                  ile-pdf--output-buffer
                  nil
                  args)))
    (unless (equal status 0)
      (error "Failed extracting pages.  See buffer %s for more detail "
             ile-pdf--output-buffer)))
  (message "Extracted pages to %s" (file-name-nondirectory out-file)))

(defun ile-pdf--parse-page-ranges (page-ranges)
  "Parse PAGE-RANGES.
PAGE-RANGES will be converted to the form expected by
'ile-pdf--extract-pages.  If PAGE-RANGES is already a list,
it will be returned as-is."
  (if (listp page-ranges)
      page-ranges
    (let ((tokens (split-string page-ranges ";" t "[[:space:]]+")))
      (cl-loop
       for tks in tokens
       collect
       (if (string-match-p "^[[:digit:]]+$" tks)
           (cons (string-to-number tks) (string-to-number tks))
         (let ((pages (split-string tks "-" t "[[:space:]]+")))
           (unless (equal (length pages) 2)
             (user-error "Unable to parse [%s] as a range of pages in [%s].  Expected 2 numbers separated by a - character" pages page-ranges))
           (cons
            (string-to-number (nth 0 pages))
            (string-to-number (nth 1 pages)))))))))

(ert-deftest ile-pdf--parse-page-ranges ()
  "Test page range parsing."
  (should (equal
           '((7 . 9) (12 . 44))
           (ile-pdf--parse-page-ranges "7-9; 12- 44")))
  (should (equal
           '((40 . 40))
           (ile-pdf--parse-page-ranges "40"))))


(defun ile-pdf-extract-pages (out-file page-ranges)
  "Extract pages after prompting the user for what to extract.
OUT-FILE is the name of the file to write to.
PAGE-RANGES is one or more page ranges, separated by a semicolon.
Each page range can be a single page number of two pages
separated by a dash (-)."
  (interactive
   (list
    (read-file-name "Output file: ")
    (read-string (format "Page ranges e.g. 1-9; 13; 45-70.  Default %d: " (pdf-view-current-page))
                 nil
                 nil
                 (number-to-string (pdf-view-current-page)))))
  ;;(interactive "FOutput file: \nMPage ranges (e.g. 1-9; 13; 45-70) ")
  (let ((page-ranges (ile-pdf--parse-page-ranges page-ranges))
        (in-file (buffer-file-name)))
    (ile-pdf--extract-pages in-file page-ranges out-file)))

;; The stuff below is just some working code I can hack on later

(defun my-extractor-junk (dir in-file page-ranges out-prefix)
  "Extract stuff."
  (let ((in-file (concat dir in-file))
        (out-file (concat dir out-prefix in-file)))
    (ile-pdf--extract-pages in-file page-ranges out-file)
  ))

;; (progn
;;   (my-extractor-junk
;;    "c:/Users/gina/Documents/Gina/Gantt/Discovery/Defendants Production/"
;;    "14-0980 PITCHESS 12391-12799.pdf"
;;    '( (2 . 30) (275 . 282)  (301 . 400))
;;    "eyes-only ")

;;   (my-extractor-junk
;;    "c:/Users/gina/Documents/Gina/Gantt/Discovery/Defendants Production/"
;;    "16-0433 PITCHESS 10229-10736.pdf"
;;    '( (11 . 14) (129 . 134)  (262 . 265) (386 . 389) (412 . 420))
;;    "eyes-only ")

;;   (my-extractor-junk
;;    "c:/Users/gina/Documents/Gina/Gantt/Discovery/Defendants Production/"
;;    "16-0501 PITCHESS 10737-11382.pdf"
;;    '( (18 . 31) (335 . 348))
;;    "eyes-only ")

;;   (my-extractor-junk
;;    "c:/Users/gina/Documents/Gina/Gantt/Discovery/Defendants Production/"
;;    "16-0833 PITCHESS 11622-12390.pdf"
;;    '( (25 . 26) (345 . 346) (657 . 658) (715 . 716))
;;    "eyes-only ")
;; )


(provide 'ile-pdf)
;;; ile-pdf.el ends here
