;;; ile-navigation.el --- Integrated Legal Environment -*- lexical-binding: t -*-

;; Copyright (C) 2018  Gina White

;; Author: Gina White <ginabythebay@gmail.com>
;; Keywords: lisp
;; Version: 0.0.1

;;; Commentary:

;; Code to navigate around a case

;;; Code:

;; code goes here

(require 'bates)
(require 'cl-lib)
(require 'dash)
(require 'org-element)
(require 'org-id)
(require 'projectile)


(defconst ile-org--bates-re "[A-Z]+ [0-9]+")
(defconst ile-org--date-re "[0-9]\\{4,4\\}-[0-9]\\{2,2\\}-[0-9]\\{2,2\\}")
(defconst ile-org--fed-rule-re "FRE\\|FRCP [0-9]+\\(([0-9a-zA-Z]+)\\)*")

(defun ile-org--derive-case (&optional case)
  "Return CASE if specified, else try to figure it out."
  (if case
      case
    (downcase (file-name-nondirectory (directory-file-name (projectile-project-root))))))

(defun ile-org--make-case-id (suffix &optional case)
  "Make an id composed of the CASE and the SUFFIX.
We derive CASE using the current directory if not specified."
  (format "%s-%s" (ile-org--derive-case case) suffix))

(defun ile--file-united-file (prefix no project-dir)
  "Find a united file.

We look for files in PROJECT-DIR/Discovery/united that contains PREFIX and NO."
  (let ((files (directory-files (concat project-dir "Discovery/united/") t)))
    (cl-some (lambda (f) (bates-file-contains-p f prefix no)) files)))

;;;###autoload
(defun ile-jump-bates-number (bates-no)
  "Jump to the bates number specified by BATES-NO in the current pdf."
  (interactive "nBates number: ")
  (pdf-view-goto-page (bates-find-page (buffer-file-name) bates-no)))

;;;###autoload
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
     do (setq last-field current-field))))

(defun ile-org-bates-at-point ()
  "Return the bates number (e.g. OCA 400) at point, or nil if none is found."
  (let ((case-fold-search nil))
    (when (thing-at-point-looking-at ile-org--bates-re 10)
      (buffer-substring (match-beginning 0) (match-end 0)))))

(defun ile-org-date-at-point ()
  "Return the date at point, or nil if none is found."
  (when (thing-at-point-looking-at ile-org--date-re 10)
     (buffer-substring (match-beginning 0) (match-end 0))))

(defun ile-org-fed-rule-at-point ()
  "Return the federal rule at point, or nil if none if found."
  (when (thing-at-point-looking-at ile-org--fed-rule-re 20)
    (buffer-substring (match-beginning 0) (match-end 0))))

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

;;;###autoload
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
  (unless project-dir (setq project-dir (projectile-project-root)))

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

;;;###autoload
(defun ile-jump (target)
  "Jump to TARGET where target can be a federal rule, a date or a bates number."
  (interactive (list (let* ((def (or (ile-org-fed-rule-at-point) (ile-org-bates-at-point) (ile-org-date-at-point)))
                            (def (and def (substring-no-properties def)))
                            (prompt (if def
                                        (format "Date or bates number (%s): " def)
                                      "Date or bates number: ")))
                       (read-string prompt nil nil def))))
  (cond
   ((string-match ile-org--fed-rule-re target)
    (ile-org-lookup-fed-rule target))
   ((string-match ile-org--bates-re target)
    (ile-jump-bates target))
   ((string-match ile-org--date-re target)
    (ile-org-lookup-date target))))

(defun ile-nav-case-org-files ()
  "Return a list of all org files in the current case."
  (let ((root (projectile-root-bottom-up default-directory)))
    (mapcar (lambda (f) (concat root f))
            (-filter
             (lambda (f) (string-suffix-p ".org" f))
             (projectile-current-project-files)))))

(defun ile-nav-case-ids ()
  "Return an alist of cons cells representing ids in case org files.
Each cons cell will be (id . filename)"
  (save-mark-and-excursion
    (let ((files (ile-nav-case-org-files))
          (case-fold-search t)
	  (re (org-re-property "ID")))
      (cl-loop with values
               for f in files
               do (set-buffer (or (find-buffer-visiting f)
                                  (find-file-noselect f)))
                  (goto-char (point-min))
                  (while (re-search-forward re nil t)
                    (push (cons (org-entry-get (point) "ID") f) values))
               finally return values))))

(defun ile-nav-visit-id (id file)
  "Similar to 'org-id-goto, but we don't pop to the buffer when we find ID.
Returns the buffer.  FILE is the file to look in."
  (let ((m (org-id-find-id-in-file id file 'marker)))
    (unless m
      (error "Cannot find entry with ID \"%s\"" id))
    (let ((buf (marker-buffer m)))
      (set-buffer (marker-buffer m))
      (goto-char m)
      (move-marker m nil)
      (org-show-context)
      buf)))

;;;###autoload
(defun ile-nav-indirect-buffer-for-id (&optional id)
  "Pop to an indirect buffer narrowed to the headline associated with ID.
Start by looking for an existing buffer of the form *<id>*.  If
not found, one will be created."
  (interactive)
  (let* ((pairs (ile-nav-case-ids))
         (ids (delete-dups (mapcar (lambda (el) (car el)) pairs)))
         (id (or id (completing-read "Choose id: " ids nil t)))
         (buf-name (format "*%s*" id))
         (buf (get-buffer buf-name))
         (found (assoc id pairs)))
    (unless found
      (user-error "Unable to find %S in case" id))
    (if buf
        (pop-to-buffer-same-window buf)
      (let ((base-buf (with-current-buffer
                          (ile-nav-visit-id id (cdr found)))))
        (pop-to-buffer-same-window (make-indirect-buffer base-buf buf-name t)))
      (org-narrow-to-subtree))))

(defsubst ile-org--prefix-match-len (s1 s2)
  "Return the length of the common prefix for S1 and S2."
  (let ((l 0))
    (while (eq (elt s1 l) (elt s2 l))
      (setq l (1+ l)))
    (if (eq l 0)
        nil
      l)))

;;;###autoload
(defun ile-org-lookup-fed-rule (rule)
  "Switch to the appropriate buffer for RULE and move the cursor to it.
RULE should be something like FRCP 26(a)(b)."
  (interactive
   (list (let* ((def (ile-org-fed-rule-at-point))
                (def (and def (substring-no-properties def)))
                (prompt (if def
                            (format "Rule to look for (%s): " def)
                          "Rule to look for: ")))
           (read-string prompt nil nil def))))

  (unless (string-match "\\([A-Z]+\\) .*" rule)
    (user-error "Unable to parse rule %S.  Expected it to start with something like FRCP" rule))

  (let* ((id (match-string 1 rule))
         (m (org-id-find id 'marker)))
    (unless m
      (error "Cannot find entry with ID \"%s\"" id))
    (let ((buf (marker-buffer m)))
      (pop-to-buffer-same-window (marker-buffer m))
      (goto-char m)
      (move-marker m nil)

      (let* ((best-len 0)    ;; length of longest match
             (best-pos nil)  ;; position of longest match
             (ast (save-restriction (org-narrow-to-subtree) (org-element-parse-buffer)))
             (pos (org-element-map
                      ast
                      '(headline)
                    (lambda (el)
                      (if-let* ((title (org-element-property :raw-value el))
                                ((string-prefix-p rule title)))
                          (org-element-property :begin el)
                        (when-let* ((l (ile-org--prefix-match-len rule title))
                                    ((> l best-len)))
                            (setq best-len l
                                  best-pos (org-element-property :begin el)))
                        nil))
                    nil t)))
        (cond
         (pos (goto-char pos))
         (best-pos (goto-char best-pos))
         (t (message "Unable to find match for %S" rule)))))))

;;;###autoload
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
  (ile-nav-indirect-buffer-for-id (ile-org--make-case-id "timeline" case))
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
            (goto-char pos)))))

;;;###autoload
(defun ile-org-fill-subtree ()
  "Run 'org-fill-paragraph on the current subtree."
  (interactive)
  (save-mark-and-excursion
    (org-mark-subtree)
    (org-fill-paragraph nil t)))


(provide 'ile-navigation)
;;; ile-navigation.el ends here
