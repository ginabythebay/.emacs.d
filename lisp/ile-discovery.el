;;; ile-pdf-discovery.el --- Integrated Legal Environment -*- lexical-binding: t -*-

;; Copyright (C) 2018  Gina White

;; Author: Gina White <ginabythebay@gmail.com>
;; Keywords: lisp
;; Version: 0.0.1

;;; Commentary:

;; Code to find productions, unite them etc.

;;; Code:

;; code goes here

(require 'cl-lib)
(require 'projectile)


(defconst ile--ignored-discovery-dirs '("./" "../" "united/" "what is this/")
  "Set of child directories of the Discovery directory that we ignore.")

(defsubst ile--f-eq (s1 s2)
  "Return t if S1 and S2 are equal when ignoring case."
  (string-collate-equalp s1 s2 nil t))

(defun ile--file-name-sort-p (f1 f2)
  "Return non-nil if F1 should sort before F2.

F1 and F2 are both treated a file names and the comparison will
be based on the numeric value of their starting bates numbers."
  (let ((range1
         (bates-parse-filename-to-range (file-name-nondirectory f1)))
        (range2
         (bates-parse-filename-to-range (file-name-nondirectory f2))))
    (unless range1
      (user-error "Unable to parse [%s] as into a bates range" f1))
    (unless range2
      (user-error "Unable to parse [%s] as into a bates range" f2))
    (< (bates-page-no (car range1)) (bates-page-no (car range2)))))

(ert-deftest ile--file-name-sort-p ()
  "Tests sorting"
  (should (equal (ile--file-name-sort-p "OCA 51-562.pdf" "OCA 1-50.pdf") nil))
  (should (equal (ile--file-name-sort-p "OCA 1-50.pdf" "OCA 51-562.pdf") t))
  (should (equal (ile--file-name-sort-p "OCA 51-562.pdf" "OCA 51-562.pdf") nil)))

(defun ile--group-productions (files)
  "Convert a list of file names in FILES to an alist.

Each element of the alist will be a list where the car is the
bates prefix and cdr is a list of file names for that bates
number.

If an element of FILES does not match our expected bates pattern,
it will be dropped."
  (let ((result ()))
    (cl-loop
     for f in files
     if (ile--f-eq "pdf" (file-name-extension f))
     do
     (let ((bates-pair (bates-parse-filename-to-range (file-name-nondirectory f))))
       (when bates-pair
         (let ((key (bates-page-prefix (car bates-pair))))
         (map-put result key (cons f (map-elt result key () #'equal)) #'equal)
       )))
     end)
    result))

(ert-deftest ile--group-productions ()
  "Tests grouping"
  (should (equal
           (ile--group-productions
            '("OCA 563-894.pdf" "PITCHESS 1-50.pdf" "PITCHESS 51-51.pdf" "OCA 1-50.pdf" "OCA 51-562.pdf"))
           '(("PITCHESS" "PITCHESS 51-51.pdf" "PITCHESS 1-50.pdf")
             ("OCA" "OCA 51-562.pdf" "OCA 1-50.pdf" "OCA 563-894.pdf")))))

(defun ile--merge-discovery-lists (m1 m2)
  "Merge M1 and M2.
We assume M1 and M2 are a-lists, as produced by
‘ile--group-productions’.  Where keys match, we append the two
lists"
  (let ((keys (delete-dups (append (map-keys m1) (map-keys m2))))
        (result ()))
    (dolist (k keys result)
      (map-put result k
               (sort
                (append
                 (map-elt m1 k () #'equal)
                 (map-elt m2 k () #'equal))
                #'ile--file-name-sort-p))
      )
    )
  )

(ert-deftest ile--merge-discovery-lists ()
  "Tests grouping"
  (should (equal
           (ile--merge-discovery-lists
            '(("PITCHESS" "PITCHESS 51-51.pdf" "PITCHESS 1-50.pdf")
               ("OCA" "OCA 51-562.pdf" "OCA 1-50.pdf" "OCA 563-894.pdf"))
            '(("GANTT" "GANTT 51-51.pdf" "GANTT 1-50.pdf")
               ("OCA" "OCA 895-895.pdf")))
           '(("GANTT" "GANTT 51-51.pdf" "GANTT 1-50.pdf")
             ("OCA" "OCA 51-562.pdf" "OCA 1-50.pdf" "OCA 563-894.pdf" "OCA 895-895.pdf")
             ("PITCHESS" "PITCHESS 51-51.pdf" "PITCHESS 1-50.pdf")))))

(defun ile--find-case-productions (&optional project-dir)
  "Look for any pdf files that appear to be document productions in PROJECT-DIR.
If PROJECT-DIR is not specified, the project directory will be
determined by projectile.

To find pdf files, we will look in [PROJECT-DIR]/Discovery/ for
all subdirectories, skipping 'united' if that exists.  Within
each such subdirectory, if there is a 'produced' subdirectory, we
will look there, otherwise we will look in the subdirectory
itself.  If a file matches what ‘bates-parse-filename-to-range’ expects.

All directory names will be compared in a case-insensitive way."
  (unless project-dir
    (setq project-dir (projectile-project-root)))

  (let ((discovery-dir (file-name-as-directory (concat project-dir "Discovery"))))
    (unless (file-directory-p discovery-dir)
      (user-error "Could not find %s to look for production files" discovery-dir))

    ;; Find all directories where we expect to find production files
    (let ((dirs
           (cl-loop
            for c in (directory-files discovery-dir)
            do (setq c (file-name-as-directory c))
            unless (member c ile--ignored-discovery-dirs)
              if (file-directory-p (file-name-as-directory (concat discovery-dir c "produced")))
                collect (file-name-as-directory (concat c "produced"))
              else
                collect c
              end)))

      (let ((result ()))
        (dolist (d dirs)
          (setq result
                (ile--merge-discovery-lists
                 result
                 (ile--group-productions
                  (directory-files (concat discovery-dir d) t)))))
        result))))

(ile--find-case-productions "~/syb/Gantt/")

(provide 'ile-discovery)
;;; ile-discovery.el ends here
