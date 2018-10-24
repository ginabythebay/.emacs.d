;;; ile-discovery.el --- Integrated Legal Environment -*- lexical-binding: t -*-

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

(defsubst ile-skip-production-file-p (base-name)
  "Return non-nil if we should skip BASE-NAME (e.g. it is a cover letter)."
  (let ((case-fold-search t))
    (string-match-p "cover letter" base-name)))

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
     (let* ((base-name (file-name-nondirectory f))
            (bates-pair (bates-parse-filename-to-range base-name)))
       (when (and bates-pair (not (ile-skip-production-file-p base-name)))
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
                #'ile--file-name-sort-p)))))

(ert-deftest ile--merge-discovery-lists ()
  "Tests grouping"
  (should (equal
           (ile--merge-discovery-lists
            '(("PITCHESS" "PITCHESS 51-51.pdf" "PITCHESS 1-50.pdf")
               ("OCA" "OCA 51-562.pdf" "OCA 1-50.pdf" "OCA 563-894.pdf"))
            '(("GANTT" "GANTT 51-51.pdf" "GANTT 1-50.pdf")
               ("OCA" "OCA 895-895.pdf")))
           '(("GANTT" "GANTT 1-50.pdf" "GANTT 51-51.pdf")
             ("OCA" "OCA 1-50.pdf" "OCA 51-562.pdf" "OCA 563-894.pdf" "OCA 895-895.pdf")
             ("PITCHESS" "PITCHESS 1-50.pdf" "PITCHESS 51-51.pdf")))))

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

(defun ile--united-name (files)
  "Calculate the base name (without the directory) of the united file.

FILES is expected to a be a list of file names in sorted order,
with each being compatible with ‘bates-parse-filename-to-range’.
If there are gaps in the file ranges, that is considered an
error."

  (let (prefix prev-end-no first-no prev-file)
    (dolist (f files)
      (let* ((range (bates-parse-filename-to-range f))
             (start (nth 0 range))
             (end (nth 1 range)))
        (unless prefix
          (setq prefix (bates-page-prefix start)))
        (unless first-no
          (setq first-no (bates-page-no start)))
        (when prev-end-no
          (unless (equal (1+ prev-end-no) (bates-page-no start))
            (user-error "There is apparently a bates number gap before [%s].  Expected it to start with %s.  Previous file is [%s]" f (1+ prev-end-no)  prev-file)))

        (setq prev-end-no (bates-page-no end))
        (setq prev-file f)))
    (format "united %s %d-%d.pdf" prefix first-no prev-end-no)))

(ert-deftest ile--united-name ()
  "Tests ile--united-name"
  (should (equal
           (ile--united-name '("OCA 17-24.pdf" "OCA 25-30.pdf" "OCA 31-50.pdf"))
           "united OCA 17-50.pdf")))

(defun ile--file-older-p (target dependencies)
  "Return t if TARGET is older than any file in the DEPENDENCIES list."
  (let ((target-time '(0 0 0 0))
        (attributes (file-attributes target))
        (result nil))
    (when attributes
      (setq target-time (nth 5 attributes)))

    (cl-loop
     for dep in dependencies
     do (when (time-less-p target-time (nth 5 (file-attributes dep)))
          (setq result t)))
    result))

(defun ile-make-united-discovery (&optional project-dir)
  "Generate/Regenerate all united discovery files as needed.

If a united discover file exists timestamps will be compared to
see if it needs to be regenerated.  All files are placed in
PROJECT-DIR/Discovery/united/.  If PROJECT-DIR is not
specified, projectile will be consulted to determine it."
  (interactive)
  (unless project-dir
    (setq project-dir (projectile-project-root)))
  (let* ((all-productions (ile--find-case-productions project-dir))
         (keys (map-keys all-productions))
         (united-dir (concat project-dir "Discovery/united/"))
         regenerated)
    (dolist (k keys)
      (let* ((dependencies (map-elt all-productions k () #'equal))
             (united-file (ile--united-name dependencies)))
        (when (ile--file-older-p (concat united-dir united-file) dependencies)
          (push united-file regenerated)
          (message "Discovery unification...building %s..." united-file)
          (ile-pdf-unite (concat united-dir united-file) dependencies))))
    (if regenerated
        (message "Discovery unification...done building %s" regenerated)
      (message "Discovery unification...nothing to do"))))

(require 'pdf-outline)
(require 'let-alist)


(defun ile-discovery--get-outline-link (pos dummy-page)
  "Return info for the link at POS.

The return value will be a cons cell where the car is a title and
the cdr is a page.  The page will be nil if, e.g. the type of
the link is not 'goto-dest.

DUMMY-PAGE will be used if there is no link at the POS."
  (let ((link (pdf-outline-link-at-pos pos)))
    (if link
        (let-alist link
          `(, .title . ,(if (eq .type 'goto-dest) .page nil)))
      `("dummy entry" . , dummy-page)
        )
    )
  )

(defun ile-discovery-copy-jury-instructions ()
  "Put a table in the kill ring based on the selected pdf outline entries.

There must be an active region and it must select one or more pdf
outline entries."
  (interactive)
  (unless (equal major-mode 'pdf-outline-buffer-mode)
    (user-error "You must run this command in a pdf outline buffer"))
  (unless (use-region-p)
    (user-error "No region found.  You must select one or more pdf outline entries"))

  (save-window-excursion
    (save-excursion
      (let* ((start (region-beginning))
             (end (region-end))
             (lastpage (pdf-info-number-of-pages
                        (window-buffer (pdf-outline-get-pdf-window))))
             (links (cl-loop initially (goto-char start)
                             collect (ile-discovery--get-outline-link
                                      (point) lastpage)
                             do (forward-line 1)
                             until (> (point) end)))
             (ranges (cl-loop with prev-page = nil
                          with prev-title = nil
                          for l in links
                          if prev-page
                          collect (list prev-page (1- (cdr l)) prev-title)
                          do (setq prev-page (cdr l)
                                   prev-title (car l)))))

        (with-temp-buffer
          (org-mode)
          (dolist (r ranges)
            (insert (format "%s-%s\t%s\n" (nth 0 r) (nth 1 r) (nth 2 r))))
          (org-table-convert-region (point-min) (point-max))
          (kill-ring-save (point-min) (point-max)))))))

(require 'company)

(defconst sample-completions
  '("alan" "john" "ada" "don"))


(defun company-sample-backend (command &optional arg &rest ignored)
  (interactive (list 'interactive))

  (cl-case command
    (interactive (company-begin-backend 'company-sample-backend))
    (prefix (and (eq major-mode 'fundamental-mode)
                 (company-grab-symbol)))
    (candidates
     (cl-remove-if-not
      (lambda (c) (string-prefix-p arg c))
      sample-completions))))

(add-to-list 'company-backends 'company-sample-backend)

(provide 'ile-discovery)
;;; ile-discovery.el ends here
