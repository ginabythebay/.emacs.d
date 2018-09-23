;;; org-noter-override.el --- org overrides

;; Copyright (C) 2018  Gina White

;; Author: Gina White <ginabythebay@gmail.com>
;; Keywords: lisp
;; Version: 0.0.1

;;; Commentary:

;; extending/overriding org-noter

;;; Code:

;; code goes here

(require 'org-noter)


(defun org-cycle-hide-drawers (state &optional exceptions)
  "Does nothing.  STATE is ignored.  EXCEPTIONS is ignored.")


(defun org-noter-insert-note (&optional precise-location)
  "Insert note associated with the current location.

This command will prompt for a title of the note and then insert
it in the notes buffer. When the input is empty, a title based on
`org-noter-default-heading-title' will be generated.

If there are other notes related to the current location, the
prompt will also suggest them. Depending on the value of the
variable `org-noter-closest-tipping-point', it may also
suggest the closest previous note.

PRECISE-LOCATION makes the new note associated with a more
specific location (see `org-noter-insert-precise-note' for more
info).

When you insert into an existing note and have text selected on
the document buffer, the variable `org-noter-insert-selected-text-inside-note'
defines if the text should be inserted inside the note."
  (interactive "P")
  (org-noter--with-valid-session
   (let* ((ast (org-noter--parse-root)) (contents (org-element-contents ast))
          (window (org-noter--get-notes-window 'force))
          (location-cons (org-noter--doc-approx-location (or precise-location 'infer)))
          (view-info (org-noter--get-view-info (org-noter--get-current-view) location-cons))

          (selected-text
           (cond
            ((eq (org-noter--session-doc-mode session) 'pdf-view-mode)
             (when (pdf-view-active-region-p)
               (mapconcat 'identity (pdf-view-active-region-text) ? )))

            ((eq (org-noter--session-doc-mode session) 'nov-mode)
             (when (region-active-p)
               (buffer-substring-no-properties (mark) (point)))))))

     (let ((inhibit-quit t))
       (with-local-quit
         (select-frame-set-input-focus (window-frame window))
         (select-window window)

         ;; IMPORTANT(nox): Need to be careful changing the next part, it is a bit
         ;; complicated to get it right...

         (let ((point (point))
               collection default default-begin title selection
               (target-post-blank (if org-noter-separate-notes-from-heading 2 1)))

           ;; NOTE(nox): When precise, it will certainly be a new note
           (if precise-location
               (setq default (and selected-text (replace-regexp-in-string "\n" " " selected-text)))

             (dolist (note-cons (org-noter--view-info-notes view-info))
               (let ((display (org-element-property :raw-value (car note-cons)))
                     (begin   (org-element-property :begin     (car note-cons))))
                 (push (cons display note-cons) collection)
                 (when (and (>= point begin) (> begin (or default-begin 0)))
                   (setq default display
                         default-begin begin)))))

           (setq collection (nreverse collection)
                 ;; Gina turn this off.  it isn't useful to me and slows the note capture down.
                 ;;title (completing-read "Note: " collection nil nil nil nil default)
                 selection (cdr (assoc title collection)))

           (if selection
               ;; NOTE(nox): Inserting on an existing note
               (let* ((reference-element (cdr selection))
                      (has-content
                       (org-element-map (org-element-contents reference-element) org-element-all-elements
                         (lambda (element) (not (memq (org-element-type element) '(section property-drawer))))
                         nil t))
                      (post-blank (org-element-property :post-blank reference-element)))

                 (when has-content (setq target-post-blank 2))

                 (goto-char (org-element-property :end reference-element))

                 ;; NOTE(nox): Org doesn't count `:post-blank' when at the end of the buffer
                 (when (org-next-line-empty-p) ;; This is only true at the end, I think
                   (goto-char (point-max))
                   (save-excursion
                     (beginning-of-line)
                     (while (looking-at "[[:space:]]*$")
                       (setq post-blank (1+ post-blank))
                       (beginning-of-line 0))))

                 (while (< post-blank target-post-blank)
                   (insert "\n")
                   (setq post-blank (1+ post-blank)))

                 (when (org-at-heading-p)
                   (forward-line -1))

                 (when (and org-noter-insert-selected-text-inside-note selected-text) (insert selected-text)))

             ;; NOTE(nox): Inserting a new note
             (let ((reference-element-cons (org-noter--view-info-reference-for-insertion view-info))
                   level)
               (when (zerop (length title))
                 (setq title (replace-regexp-in-string (regexp-quote "$p$") (number-to-string (car location-cons))
                                                       org-noter-default-heading-title)))

               (if reference-element-cons
                   (progn
                     (cond
                      ((eq (car reference-element-cons) 'before)
                       (goto-char (org-element-property :begin (cdr reference-element-cons))))
                      ((eq (car reference-element-cons) 'after)
                       (goto-char (org-element-property :end (cdr reference-element-cons)))))

                     (setq level (org-element-property :level (cdr reference-element-cons))))

                 (goto-char (org-element-map contents 'section
                              (lambda (section) (org-element-property :end section))
                              nil t org-element-all-elements))
                 (setq level (1+ (org-element-property :level ast))))

               ;; NOTE(nox): This is needed to insert in the right place...
               (outline-show-entry)
               (org-noter--insert-heading level title target-post-blank)
               (when (org-noter--session-hide-other session) (org-overview))

               (org-entry-put nil org-noter-property-note-location (org-noter--pretty-print-location location-cons))

               (setf (org-noter--session-num-notes-in-view session)
                     (1+ (org-noter--session-num-notes-in-view session)))))

           (org-show-entry)
           (org-show-children)
           (org-show-set-visibility t)
           (org-cycle-hide-drawers 'all)))
       (when quit-flag
         ;; NOTE(nox): If this runs, it means the user quitted while creating a note, so
         ;; revert to the previous window.
         (select-frame-set-input-focus (org-noter--session-frame session))
         (select-window (get-buffer-window (org-noter--session-doc-buffer session))))))))


(provide 'org-noter-override)
;;; org-noter-override.el ends here
