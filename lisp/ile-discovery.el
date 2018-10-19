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

(defun ile--file-united-file (prefix no project-dir)
  "Find a united file.

We look for files in PROJECT-DIR/Discovery/united that contains PREFIX and NO."
  (let ((files (directory-files (concat project-dir "Discovery/united/") t)))
    (cl-some (lambda (f) (bates-file-contains-p f prefix no)) files)))

(defun ile-jump-bates-number (bates-no)
  "Jump to the bates number specified by BATES-NO in the current pdf."
  (interactive "nBates number: ")
  (pdf-view-goto-page (bates-find-page (buffer-file-name) bates-no)))

(defun ile-org-table-column-next-duplicate ()
  "Move the cursor down to the cells in the current column until we leave the current table or we find the next duplicate entry."
  (interactive)
  (unless (org-table-p)
    (user-error "Must be in a table"))
  (save-excursion (org-table-align))
  (let ((last-field (org-trim (save-excursion (org-table-get-field)))))
    (next-line)
    (cl-loop
     for current-field = (org-trim (save-excursion (org-table-get-field)))
     then (progn (next-line) (org-trim (save-excursion (org-table-get-field))))
     until (or (not (org-table-p)) (string= current-field last-field))
     do (setq last-field current-field)
     )
    )
  )

(defun ile-org--derive-case (&optional case)
  "Return CASE if specified, else try to figure it out."
  (if case
      case
    (downcase (file-name-nondirectory (directory-file-name (projectile-project-root))))))

(defun ile-org--make-case-id (suffix &optional case)
  "Make an id composed of the CASE and the SUFFIX.
We derive CASE using the current directory if not specified."
  (format "%s-%s" (ile-org--derive-case case) suffix))


(defconst ile-org--bates-re "[A-Z]+ [0-9]+")
(defconst ile-org--date-re "[0-9]\\{4,4\\}-[0-9]\\{2,2\\}-[0-9]\\{2,2\\}")

(defun ile-org-bates-at-point ()
  "Return the bates number (e.g. OCA 400) at point, or nil if none is found."
  (let ((case-fold-search nil))
    (when (thing-at-point-looking-at ile-org--bates-re 10)
      (buffer-substring (match-beginning 0) (match-end 0)))))

(defun ile-org-date-at-point ()
  "Return the date at point, or nil if none is found."
  (when (thing-at-point-looking-at ile-org--date-re 10)
     (buffer-substring (match-beginning 0) (match-end 0))))


(defun ile-org-lookup-date (target &optional case)
  "Look up a TARGET date and show related CASE information for it.
Case is just our client name, e.g. 'gantt'.  We will attempt to
derive it using projectile if not specified."
  (interactive (list (let* ((def (ile-org-date-at-point))
                            (def (and def (substring-no-properties def)))
                            (prompt (if def
                                        (format "Date to look for (%s): " def)
                                      "Date to look for: ")))
                       (read-string prompt nil nil def))))
  (org-id-goto (ile-org--make-case-id "timeline" case))
  (org-narrow-to-subtree)
  (let ((ast (org-element-parse-buffer))
        (row-no 0)
        (col-no 0))
    (let ((pos (org-element-map ast '(table-cell table-row)
                 (lambda (el)
                   (let ((el-type (org-element-type el)))
                     (cond ((eq el-type 'table-row)
                            (setq row-no (1+ row-no) col-no 0)
                            nil)
                           ((eq el-type 'table-cell)
                            (setq col-no (1+ col-no))
                            (when (equal col-no 1)
                              (let ((contents (substring-no-properties (car (org-element-contents el)))))
                                (when (/= 0 (string-to-number contents))
                                  (when (or (string= contents target) (string< target contents))
                                    (org-element-property :contents-begin el)))))))))
                 nil t)))
          (when pos
            (goto-char pos))))
  (widen))

(defun ile-org--parse-current-discovery-table (noun-hdr def-hdr)
  "Parse the first table in the current element.
NOUN-HDR is the name of the column we will use as the thing we are defining.
DEF-HDR is the name of the column we will use as the definition.

Return value will be hash table mapping from noun to definition.

We expect that the first line of the table is a header line."
  (let ((result (make-hash-table :test #'equal)))
    (save-restriction
      (org-narrow-to-subtree)
      (let ((ast (org-element-parse-buffer))
            (row-no 0)
            (col-no 0)
            (table-no 0)
            (checked-header nil)
            checked-header cur-noun cur-def noun-col def-col)
        (org-element-map ast '(table-row table-cell table)
          (lambda (el)
            (cl-case (org-element-type el)
              ('table
               (setq table-no (1+ table-no))
               (when (> table-no 1)
                 t)
               nil)
              ('table-row
               (setq col-no 0
                     row-no (1+ row-no))
               nil)
              ('table-cell
               (setq col-no (1+ col-no))
               (let ((cell-contents (substring-no-properties
                                     (car (org-element-contents el)))))
                 (if (= row-no 1)
                     (cond ((string= cell-contents noun-hdr)
                            (setq noun-col col-no))
                           ((string= cell-contents def-hdr)
                            (setq def-col col-no)))
                   (when (and (not checked-header) (>= row-no 2))
                     (unless noun-col
                       (user-error "Could not find noun header %s" noun-hdr))
                     (unless def-col
                       (user-error "Could not find definition header %s" def-col))
                     (setq checked-header t))

                   (cond ((= noun-col col-no)
                          (setq cur-noun cell-contents))
                         ((= def-col col-no)
                          (setq cur-def cell-contents)))

                   (when (and cur-noun cur-def)
                     (puthash cur-noun cur-def result)
                     (setq cur-noun nil
                           cur-def nil))
                   nil))))
            nil t)))
      result)))

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


(defun ile-jump-bates (bates &optional project-dir)
  "Open united file matching BATES, jump to the correct page.

If PROJECT-DIR is not specified, projectile will be consulted to
determine it."
  (interactive (list (let* ((def (ile-org-bates-at-point))
                            (def (and def (substring-no-properties def)))
                            (prompt (if def
                                        (format "Bates number (%s): " def)
                                      "Bates number: ")))
                     (read-string prompt nil nil def))))
  (unless project-dir
    (setq project-dir (projectile-project-root)))

  (let ((tokens (split-string bates)))
    (unless (equal 2 (length tokens))
      (user-error "Unexpected bates value [%s].  Should be something like [OCA 739]" bates))

    (let ((prefix (nth 0 tokens))
          (no (string-to-number (nth 1 tokens))))
      (when (equal 0 no)
        (user-error "Unexpected bates value [%s].  Should be something like [OCA 739]" bates))

      (let ((united-file (ile--file-united-file prefix no project-dir)))
        (unless united-file
          (user-error "Unable to find united file for %s" bates))
        (find-file united-file)
        (pdf-view-goto-page (bates-find-page united-file no))))))

(defun ile-jump-discovery (target)
  "Jump to TARGET where target can be a date or a bates number."
  (interactive (list (let* ((def (or (ile-org-bates-at-point) (ile-org-date-at-point)))
                            (def (and def (substring-no-properties def)))
                            (prompt (if def
                                        (format "Date or bates number (%s): " def)
                                      "Date or bates number: ")))
                       (read-string prompt nil nil def))))
  (cond ((string-match ile-org--bates-re target)
         (ile-jump-bates target))
        ((string-match ile-org--date-re target)
         (ile-org-lookup-date target))))

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
