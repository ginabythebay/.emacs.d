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
  :group 'ile-pdf
  :type 'string)

(defcustom ile-pdf-wkhtmltopdf (executable-find "wkhtmltopdf")
  "Path to wkhtmltopdf executable."
  :group 'ile-pdf
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

(defun ile-pdf-from-html (in-files out-file &optional footer-left footer-right)
  "Convert one or more html files to a pdf file.

IN-FILES is either a single html file or a list of html files.
OUT-FILE is the pdf file to output to.  if FOOTER-LEFT or
FOOTER-RIGHT is set, we will pass --footer-line to the
executable."
  (when (get-buffer ile-pdf--output-buffer)
    (kill-buffer ile-pdf--output-buffer))

  (let ((args
         '("--orientation" "Landscape"
           "--no-background"
           "--no-outline")))
    (when footer-left
      (setq args (append args `("--footer-left" ,footer-left))))
    (when footer-right
      (setq args (append args `("--footer-right" ,footer-right))))
    (when (or footer-left footer-right)
      (setq args (append args '("footer-line"))))
    (when (not (listp in-files))
      (setq in-files (list in-files)))
    (setq args (append args in-files (list out-file)))

    (let ((status (apply
                   #'call-process
                   ile-pdf-wkhtmltopdf
                   nil
                   ile-pdf--output-buffer
                   nil
                   args)))
      (unless (equal status 0)
        (error "Failed convert to pdf.  See buffer %s for more detail "
               ile-pdf--output-buffer)))))

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
    (let ((tokens (split-string page-ranges ";\\|," t "[[:space:]]+")))
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
           '((33 . 33) (36 . 36) (44 . 44) (83 . 83) (84 . 84) (85 . 85)
             (86 . 86) (87 . 87) (88 . 88) (89 . 89))
           (ile-pdf--parse-page-ranges "33,36,44,83,84,85,86,87,88,89")))
  (should (equal
           '((7 . 9) (12 . 44))
           (ile-pdf--parse-page-ranges "7-9; 12- 44")))
  (should (equal
           '((40 . 40))
           (ile-pdf--parse-page-ranges "40"))))


;;;###autoload
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
  (let ((page-ranges (ile-pdf--parse-page-ranges page-ranges))
        (in-file (buffer-file-name)))
    (ile-pdf--extract-pages in-file page-ranges out-file)))

(defun ile-pdf-unite (out-file in-files)
  "Concatenate all files in IN-FILES to produce OUT-FILE."
  (when (get-buffer ile-pdf--output-buffer)
    (kill-buffer ile-pdf--output-buffer))

  (let* ((args (append in-files (list "cat" "output" out-file)))
         (status (apply
                  #'call-process
                  ile-pdf-pdftk
                  nil
                  ile-pdf--output-buffer
                  nil
                  args)))
    (unless (equal status 0)
      (error "Failed uniting files.  See buffer %s for more detail "
             ile-pdf--output-buffer))))

(provide 'ile-pdf)
;;; ile-pdf.el ends here
