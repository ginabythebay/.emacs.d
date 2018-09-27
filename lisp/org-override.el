;;; org-override.el --- org overrides

;; Copyright (C) 2018  Gina White

;; Author: Gina White <ginabythebay@gmail.com>
;; Keywords: lisp
;; Version: 0.0.1

;;; Commentary:

;; overriding some default org-agenda functions.
;; org-agenda-get-category-icon now accepts and uses tags and
;; org-agenda-format-item passes tags in.

;;; Code:

;; code goes here

(require 'org)


(defun org-agenda-get-category-icon (tags)
  "Return an image for and TAGS according to `org-agenda-category-icon-alist'."
  (cond ((member "court" tags) (apply #'create-image '("~/.emacs.d/icons/baseline-lock-24px.svg" nil nil :ascent center)))
        ((member "hard" tags) (apply #'create-image '("~/.emacs.d/icons/baseline-lock-24px.svg" nil nil :ascent center)))
        ((member "soft" tags) (apply #'create-image '("~/.emacs.d/icons/baseline-lock_open-24px.svg" nil nil :ascent center)))
        ((member "other" tags) (apply #'create-image '("~/.emacs.d/icons/baseline-group-24px.svg" nil nil :ascent center)))))

(defun org-agenda-format-item (extra txt &optional level category tags dotime
				     remove-re habitp)
  "Format TXT to be inserted into the agenda buffer.
In particular, add the prefix and corresponding text properties.

EXTRA must be a string to replace the `%s' specifier in the prefix format.
LEVEL may be a string to replace the `%l' specifier.
CATEGORY (a string, a symbol or nil) may be used to overrule the default
category taken from local variable or file name.  It will replace the `%c'
specifier in the format.
DOTIME, when non-nil, indicates that a time-of-day should be extracted from
TXT for sorting of this entry, and for the `%t' specifier in the format.
When DOTIME is a string, this string is searched for a time before TXT is.
TAGS can be the tags of the headline.
Any match of REMOVE-RE will be removed from TXT."
  ;; We keep the org-prefix-* variable values along with a compiled
  ;; formatter, so that multiple agendas existing at the same time do
  ;; not step on each other toes.
  ;;
  ;; It was inconvenient to make these variables buffer local in
  ;; Agenda buffers, because this function expects to be called with
  ;; the buffer where item comes from being current, and not agenda
  ;; buffer
  (let* ((bindings (car org-prefix-format-compiled))
	 (formatter (cadr org-prefix-format-compiled)))
    (cl-loop for (var value) in bindings
	     do (set var value))
    (save-match-data
      ;; Diary entries sometimes have extra whitespace at the beginning
      (setq txt (org-trim txt))

      ;; Fix the tags part in txt
      (setq txt (org-agenda-fix-displayed-tags
		 txt tags
		 org-agenda-show-inherited-tags
		 org-agenda-hide-tags-regexp))

      (let* ((category (or category
			   (if buffer-file-name
			       (file-name-sans-extension
				(file-name-nondirectory buffer-file-name))
			     "")))
	     (category-icon (org-agenda-get-category-icon tags))
	     (category-icon (if category-icon
				(propertize " " 'display category-icon)
			      ""))
	     (effort (and (not (string= txt ""))
			  (get-text-property 1 'effort txt)))
	     ;; time, tag, effort are needed for the eval of the prefix format
	     (tag (if tags (nth (1- (length tags)) tags) ""))
	     (time-grid-trailing-characters (nth 2 org-agenda-time-grid))
	     time
	     (ts (if dotime (concat
			     (if (stringp dotime) dotime "")
			     (and org-agenda-search-headline-for-time txt))))
	     (time-of-day (and dotime (org-get-time-of-day ts)))
	     stamp plain s0 s1 s2 rtn srp l
	     duration breadcrumbs)
	(and (derived-mode-p 'org-mode) buffer-file-name
	     (add-to-list 'org-agenda-contributing-files buffer-file-name))
	(when (and dotime time-of-day)
	  ;; Extract starting and ending time and move them to prefix
	  (when (or (setq stamp (string-match org-stamp-time-of-day-regexp ts))
		    (setq plain (string-match org-plain-time-of-day-regexp ts)))
	    (setq s0 (match-string 0 ts)
		  srp (and stamp (match-end 3))
		  s1 (match-string (if plain 1 2) ts)
		  s2 (match-string (if plain 8 (if srp 4 6)) ts))

	    ;; If the times are in TXT (not in DOTIMES), and the prefix will list
	    ;; them, we might want to remove them there to avoid duplication.
	    ;; The user can turn this off with a variable.
	    (if (and org-prefix-has-time
		     org-agenda-remove-times-when-in-prefix (or stamp plain)
		     (string-match (concat (regexp-quote s0) " *") txt)
		     (not (equal ?\] (string-to-char (substring txt (match-end 0)))))
		     (if (eq org-agenda-remove-times-when-in-prefix 'beg)
			 (= (match-beginning 0) 0)
		       t))
		(setq txt (replace-match "" nil nil txt))))
	  ;; Normalize the time(s) to 24 hour
	  (if s1 (setq s1 (org-get-time-of-day s1 'string t)))
	  (if s2 (setq s2 (org-get-time-of-day s2 'string t)))

	  ;; Try to set s2 if s1 and
	  ;; `org-agenda-default-appointment-duration' are set
	  (when (and s1 (not s2) org-agenda-default-appointment-duration)
	    (setq s2
		  (org-duration-from-minutes
		   (+ (org-duration-to-minutes s1 t)
		      org-agenda-default-appointment-duration)
		   nil t)))

	  ;; Compute the duration
	  (when s2
	    (setq duration (- (org-duration-to-minutes s2)
			      (org-duration-to-minutes s1)))))

	(when (string-match "\\([ \t]+\\)\\(:[[:alnum:]_@#%:]+:\\)[ \t]*$" txt)
	  ;; Tags are in the string
	  (if (or (eq org-agenda-remove-tags t)
		  (and org-agenda-remove-tags
		       org-prefix-has-tag))
	      (setq txt (replace-match "" t t txt))
	    (setq txt (replace-match
		       (concat (make-string (max (- 50 (length txt)) 1) ?\ )
			       (match-string 2 txt))
		       t t txt))))

	(when remove-re
	  (while (string-match remove-re txt)
	    (setq txt (replace-match "" t t txt))))

	;; Set org-heading property on `txt' to mark the start of the
	;; heading.
	(add-text-properties 0 (length txt) '(org-heading t) txt)

	;; Prepare the variables needed in the eval of the compiled format
	(if org-prefix-has-breadcrumbs
	    (setq breadcrumbs (org-with-point-at (org-get-at-bol 'org-marker)
				(let ((s (org-display-outline-path nil nil "->" t)))
				  (if (eq "" s) "" (concat s "->"))))))
	(setq time (cond (s2 (concat
			      (org-agenda-time-of-day-to-ampm-maybe s1)
			      "-" (org-agenda-time-of-day-to-ampm-maybe s2)
			      (if org-agenda-timegrid-use-ampm " ")))
			 (s1 (concat
			      (org-agenda-time-of-day-to-ampm-maybe s1)
			      (if org-agenda-timegrid-use-ampm
                                  (concat time-grid-trailing-characters " ")
                                time-grid-trailing-characters)))
			 (t ""))
	      extra (or (and (not habitp) extra) "")
	      category (if (symbolp category) (symbol-name category) category)
	      level (or level ""))
	(if (string-match org-bracket-link-regexp category)
	    (progn
	      (setq l (if (match-end 3)
			  (- (match-end 3) (match-beginning 3))
			(- (match-end 1) (match-beginning 1))))
	      (when (< l (or org-prefix-category-length 0))
		(setq category (copy-sequence category))
		(org-add-props category nil
		  'extra-space (make-string
				(- org-prefix-category-length l 1) ?\ ))))
	  (if (and org-prefix-category-max-length
		   (>= (length category) org-prefix-category-max-length))
	      (setq category (substring category 0 (1- org-prefix-category-max-length)))))
	;; Evaluate the compiled format
	(setq rtn (concat (eval formatter) txt))

	;; And finally add the text properties
	(remove-text-properties 0 (length rtn) '(line-prefix t wrap-prefix t) rtn)
	(org-add-props rtn nil
	  'org-category category
	  'tags (mapcar 'org-downcase-keep-props tags)
	  'org-highest-priority org-highest-priority
	  'org-lowest-priority org-lowest-priority
	  'time-of-day time-of-day
	  'duration duration
	  'breadcrumbs breadcrumbs
	  'txt txt
	  'level level
	  'time time
	  'extra extra
	  'format org-prefix-format-compiled
	  'dotime dotime)))))


(defmacro org-odt--export-wrap (out-file &rest body)
  `(let* ((--out-file ,out-file)
	  (out-file-type (file-name-extension --out-file))
	  (org-odt-xml-files '("META-INF/manifest.xml" "content.xml"
			       "meta.xml" "styles.xml"))
	  ;; Initialize temporary workarea.  All files that end up in
	  ;; the exported document get parked/created here.
	  (org-odt-zip-dir (file-name-as-directory
			    (make-temp-file (format "%s-" out-file-type) t)))
	  (org-odt-manifest-file-entries nil)
	  (--cleanup-xml-buffers
	   (lambda ()
	     ;; Kill all XML buffers.
	     (dolist (file org-odt-xml-files)
	       (let ((buf (find-buffer-visiting
			   (concat org-odt-zip-dir file))))
		 (when buf
		   (with-current-buffer buf
		     (set-buffer-modified-p nil)
		     (kill-buffer buf)))))
	     ;; Delete temporary directory and also other embedded
	     ;; files that get copied there.
	     (delete-directory org-odt-zip-dir t))))
     (condition-case err
	 (progn
	   (unless (executable-find "7za")
	     ;; Not at all OSes ship with zip by default
	     (error "Executable \"7za\" needed for creating OpenDocument files"))
	   ;; Do export.  This creates a bunch of xml files ready to be
	   ;; saved and zipped.
	   (progn ,@body)
	   ;; Create a manifest entry for content.xml.
	   (org-odt-create-manifest-file-entry "text/xml" "content.xml")
	   ;; Write mimetype file
	   (let* ((mimetypes
		   '(("odt" . "application/vnd.oasis.opendocument.text")
		     ("odf" .  "application/vnd.oasis.opendocument.formula")))
		  (mimetype (cdr (assoc-string out-file-type mimetypes t))))
	     (unless mimetype
	       (error "Unknown OpenDocument backend %S" out-file-type))
	     (write-region mimetype nil (concat org-odt-zip-dir "mimetype"))
	     (org-odt-create-manifest-file-entry mimetype "/" "1.2"))
	   ;; Write out the manifest entries before zipping
	   (org-odt-write-manifest-file)
	   ;; Save all XML files.
	   (dolist (file org-odt-xml-files)
	     (let ((buf (find-buffer-visiting
			 (concat org-odt-zip-dir file))))
	       (when buf
		 (with-current-buffer buf
		   ;; Prettify output if needed.
		   (when org-odt-prettify-xml
		     (indent-region (point-min) (point-max)))
		   (save-buffer 0)))))
	   ;; Run zip.
	   (let* ((target --out-file)
		  (target-name (file-name-nondirectory target))
		  (cmds `(("7za" "-mx=0" ,target-name "mimetype")
			  ("7za" "-r -sdel -y" ,target-name "."))))
	     ;; If a file with same name as the desired output file
	     ;; exists, remove it.
	     (when (file-exists-p target)
	       (delete-file target))
	     ;; Zip up the xml files.
	     (let ((coding-system-for-write 'no-conversion) exitcode err-string)
	       (message "Creating ODT file...")
	       ;; Switch temporarily to content.xml.  This way Zip
	       ;; process will inherit `org-odt-zip-dir' as the current
	       ;; directory.
	       (with-current-buffer
		   (find-file-noselect (concat org-odt-zip-dir "content.xml") t)
		 (dolist (cmd cmds)
		   (message "Running %s" (mapconcat 'identity cmd " "))
		   (setq err-string
			 (with-output-to-string
			   (setq exitcode
				 (apply 'call-process (car cmd)
					nil standard-output nil (cdr cmd)))))
		   (or (zerop exitcode)
		       (error (concat "Unable to create OpenDocument file."
				      "  Zip failed with error (%s)")
			      err-string)))))
	     ;; Move the zip file from temporary work directory to
	     ;; user-mandated location.
	     (rename-file (concat org-odt-zip-dir target-name) target)
	     (message "Created %s" (expand-file-name target))
	     ;; Cleanup work directory and work files.
	     (funcall --cleanup-xml-buffers)
	     ;; Open the OpenDocument file in archive-mode for
	     ;; examination.
	     (find-file-noselect target t)
	     ;; Return exported file.
	     (cond
	      ;; Case 1: Conversion desired on exported file.  Run the
	      ;; converter on the OpenDocument file.  Return the
	      ;; converted file.
	      (org-odt-preferred-output-format
	       (or (org-odt-convert target org-odt-preferred-output-format)
		   target))
	      ;; Case 2: No further conversion.  Return exported
	      ;; OpenDocument file.
	      (t target))))
       (error
	;; Cleanup work directory and work files.
	(funcall --cleanup-xml-buffers)
	(message "OpenDocument export failed: %s"
		 (error-message-string err))))))


(provide 'org-override)
;;; org-override.el ends here
