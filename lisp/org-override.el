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


;; our version just calls read-char-exclusive instead of read-char
(defun org-clock-resolve (clock &optional prompt-fn last-valid fail-quietly)
  "Resolve an open Org clock.
An open clock was found, with `dangling' possibly being non-nil.
If this function was invoked with a prefix argument, non-dangling
open clocks are ignored.  The given clock requires some sort of
user intervention to resolve it, either because a clock was left
dangling or due to an idle timeout.  The clock resolution can
either be:

  (a) deleted, the user doesn't care about the clock
  (b) restarted from the current time (if no other clock is open)
  (c) closed, giving the clock X minutes
  (d) closed and then restarted
  (e) resumed, as if the user had never left

The format of clock is (CONS MARKER START-TIME), where MARKER
identifies the buffer and position the clock is open at (and
thus, the heading it's under), and START-TIME is when the clock
was started."
  (cl-assert clock)
  (let* ((ch
	  (save-window-excursion
	    (save-excursion
	      (unless org-clock-resolving-clocks-due-to-idleness
		(org-clock-jump-to-current-clock clock))
	      (unless org-clock-resolve-expert
		(with-output-to-temp-buffer "*Org Clock*"
		  (princ (format-message "Select a Clock Resolution Command:

i/q      Ignore this question; the same as keeping all the idle time.

k/K      Keep X minutes of the idle time (default is all).  If this
         amount is less than the default, you will be clocked out
         that many minutes after the time that idling began, and then
         clocked back in at the present time.

g/G      Indicate that you \"got back\" X minutes ago.  This is quite
         different from `k': it clocks you out from the beginning of
         the idle period and clock you back in X minutes ago.

s/S      Subtract the idle time from the current clock.  This is the
         same as keeping 0 minutes.

C        Cancel the open timer altogether.  It will be as though you
         never clocked in.

j/J      Jump to the current clock, to make manual adjustments.

For all these options, using uppercase makes your final state
to be CLOCKED OUT."))))
	      (org-fit-window-to-buffer (get-buffer-window "*Org Clock*"))
	      (let (char-pressed)
		(while (or (null char-pressed)
			   (and (not (memq char-pressed
					   '(?k ?K ?g ?G ?s ?S ?C
						?j ?J ?i ?q)))
				(or (ding) t)))
		  (setq char-pressed
			(read-char-exclusive (concat (funcall prompt-fn clock)
					   " [jkKgGSscCiq]? ")
				   nil 45)))
		(and (not (memq char-pressed '(?i ?q))) char-pressed)))))
	 (default
	   (floor (/ (float-time
		      (time-subtract (current-time) last-valid)) 60)))
	 (keep
	  (and (memq ch '(?k ?K))
	       (read-number "Keep how many minutes? " default)))
	 (gotback
	  (and (memq ch '(?g ?G))
	       (read-number "Got back how many minutes ago? " default)))
	 (subtractp (memq ch '(?s ?S)))
	 (barely-started-p (< (- (float-time last-valid)
				 (float-time (cdr clock))) 45))
	 (start-over (and subtractp barely-started-p)))
    (cond
     ((memq ch '(?j ?J))
      (if (eq ch ?J)
	  (org-clock-resolve-clock clock 'now nil t nil fail-quietly))
      (org-clock-jump-to-current-clock clock))
     ((or (null ch)
	  (not (memq ch '(?k ?K ?g ?G ?s ?S ?C))))
      (message ""))
     (t
      (org-clock-resolve-clock
       clock (cond
	      ((or (eq ch ?C)
		   ;; If the time on the clock was less than a minute before
		   ;; the user went away, and they've ask to subtract all the
		   ;; time...
		   start-over)
	       nil)
	      ((or subtractp
		   (and gotback (= gotback 0)))
	       last-valid)
	      ((or (and keep (= keep default))
		   (and gotback (= gotback default)))
	       'now)
	      (keep
	       (time-add last-valid (seconds-to-time (* 60 keep))))
	      (gotback
	       (time-subtract (current-time)
			      (seconds-to-time (* 60 gotback))))
	      (t
	       (error "Unexpected, please report this as a bug")))
       (and gotback last-valid)
       (memq ch '(?K ?G ?S))
       (and start-over
	    (not (memq ch '(?K ?G ?S ?C))))
       fail-quietly)))))

(defun gw/maybe-insert-item ()
  "Insert a list item if we are in a list.

If inserting and the current item has a checkbox, create a
  checkbox for the one we insert."
  (interactive)
  (when-let* ((item-start (org-in-item-p)))
    (let ((cbox))
      (save-excursion
        (goto-char item-start)
        (goto-char (point-at-bol))
        (setq cbox (org-at-item-checkbox-p)))
      (org-insert-item cbox))))


;; This override calls gw/maybe-insert-item, which handles checkboxes
;; instead of org-insert-item.
(defun org-meta-return (&optional arg)
  "Insert a new heading or wrap a region in a table.
If ARG is set, we call `org-insert-heading'.
Calls `org-insert-heading', `org-insert-item' or
`org-table-wrap-region', depending on context.  When called with
an argument, unconditionally call `org-insert-heading'."
  (interactive "P")
  (org-check-before-invisible-edit 'insert)
  (or (run-hook-with-args-until-success 'org-metareturn-hook)
      (call-interactively (cond (arg #'org-insert-heading)
				((org-at-table-p) #'org-table-wrap-region)
				((org-in-item-p) #'gw/maybe-insert-item)
				(t #'org-insert-heading)))))

(provide 'org-override)
;;; org-override.el ends here
