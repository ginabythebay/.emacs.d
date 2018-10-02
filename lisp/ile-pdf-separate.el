;;; ile-pdf-separate.el --- Integrated Legal Environment

;; Copyright (C) 2018  Gina White

;; Author: Gina White <ginabythebay@gmail.com>
;; Keywords: lisp
;; Version: 0.0.1

;;; Commentary:

;; Code to manipulate pdfs

;;; Code:

;; code goes here

(require 'cl-lib)

(defgroup ile-pdf-separate nil
  "Separate PDF pages."
  :group 'convenience)

(defcustom ile-pdf-separate-pdftk (executable-find "pdftk")
  "Path to pdftk executable."
  :group 'ile-pdf-separate
  :type 'string)

(defconst ile-pdf-separate--output-buffer "*ile-pdf-separate*")

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
  (kill-buffer ile-pdf-separate--output-buffer)
  (let* ((args (ile-pdf--extract-pages-args in-file page-ranges out-file))
         (status (apply
                  #'call-process
                  ile-pdf-separate-pdftk
                  nil
                  ile-pdf-separate--output-buffer
                  nil
                  args)))
    (unless (equal status 0)
      (error "Failed extracting pages.  See buffer %s for more detail "
             ile-pdf-separate--output-buffer)))
  (message "Extracted pages to %s" (file-name-nondirectory out-file)))


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


(provide 'ile-pdf-separate)
;;; ile-pdf-separate.el ends here
