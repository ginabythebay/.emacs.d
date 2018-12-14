;;; marec.el --- manages my launcher                     -*- lexical-binding: t; -*-

;; Copyright (C) 2018  Gina White

;; Author: Gina White <ginabythebay@gmail.com>
;; Keywords: lisp
;; Version: 0.0.1

;;; Commentary:

;; not much to say here

;;; Code:

;; code goes here

(require 'diff)

;;;###autoload
(defun marec-update ()
  "Update the marec buffer.
Returns the status buffer."
  (interactive)
  (let* ((inhibit-read-only t)
         (candidates (seq-filter #'marec-buf-candidate-p (buffer-list)))
         (candidates-p (not (zerop (length candidates))))
         (status-buffer (get-buffer-create "*marec status*")))
    (with-current-buffer status-buffer
      (erase-buffer)
      (if (not candidates-p)
          (insert "No files recoverable buffers open")
        (insert "The following open buffers appear to be recoverable:")
        (cl-loop for c in candidates
                 do
                 (newline)
                 (let ((beg (point)))
                   (insert "  " (buffer-name c))
                   (put-text-property beg (point) 'marec-properties (list c)))))
      (if candidates-p
          (message "%d buffers appear to be recoverable" (length candidates))
        (message "No buffers appear to be recoverable")))
    status-buffer))

;;;###autoload
(defun marec-status ()
  "Look at open buffers potentially needing recovery."
  (interactive)
  (let ((buf (marec-update)))
    (with-current-buffer (marec-update)
      (marec-mode))
    (pop-to-buffer buf)))


;;;###autoload
(defalias 'marec 'marec-status)

(defun marec-buf-candidate-p (buf)
  "Return non-nil if BUF is a candidate for recovery.
Based on code in `after-find-file'"
  (with-current-buffer buf
    (and
     buffer-file-name
     (file-writable-p buffer-file-name)
     (not (buffer-base-buffer))   ;; exclude indirect buffers
     (file-newer-than-file-p (or buffer-auto-save-file-name
				 (make-auto-save-file-name))
			     buffer-file-name))))

(defvar marec-mode-map
  (let ((map (make-keymap)))
    (define-key map (kbd "d") #'marec-diff-buffer)
    (define-key map (kbd "g") #'marec-update)
    (define-key map (kbd "r") #'marec-recover)
    (define-key map (kbd "RET") #'marec-visit-buffer)
    map))

(defun marec-visit-buffer ()
  "Visit the buffer on the current line."
  (interactive)
  (switch-to-buffer (marec-current-buffer)))

(defun marec-recover ()
  "Recover the buffer on the current line."
  (interactive)
  (let ((buf (marec-current-buffer)))
    (unless (marec-buf-candidate-p buf)
      (user-error "Buffer %s no longer has auto-save data; %s" buf (substitute-command-keys "use `\\[marec-update]' to update.")))
    (with-current-buffer buf

      (let ((file-name (let ((buffer-file-name (buffer-file-name buf)))
		         (make-auto-save-file-name)))
            (inhibit-read-only t)
	    ;; Keep the current buffer-file-coding-system.
	    (coding-system buffer-file-coding-system)
	    ;; Auto-saved file should be read with special coding.
	    (coding-system-for-read 'auto-save-coding))
	(erase-buffer)
	(insert-file-contents file-name nil)
	(set-buffer-file-coding-system coding-system))
      (after-find-file nil nil t)
      (save-buffer)
      (marec-update))))

(defun marec-diff-buffer ()
  "Diff the buffer on the current line."
  (interactive)
  (let ((buf (marec-current-buffer)))
    (unless (marec-buf-candidate-p buf)
      (user-error "Buffer %s no longer has auto-save data; %s" buf (substitute-command-keys "use `\\[marec-update]' to update.")))
    (with-current-buffer buf
      (let ((save-file-name (or buffer-auto-save-file-name
			        (make-auto-save-file-name))))
        (diff (current-buffer) save-file-name "--strip-trailing-cr" 'noasync)))))

(defun marec-current-buffer ()
  "Return the buffer at line, if there is one and it is live."
  (let ((props (get-text-property (line-beginning-position)
				  'marec-properties)))
    (unless props
      (user-error "No buffer on this line"))
    (let ((buf (car props)))
      (unless (buffer-live-p buf)
	(user-error "Buffer %s has been killed; %s" buf (substitute-command-keys "use `\\[marec-update]' to update.")))
      buf)))

(define-derived-mode marec-mode special-mode "Marec"
  "A major mode for magic recovery."
  (setq buffer-read-only t)
  (buffer-disable-undo)
  (set (make-local-variable 'revert-buffer-function)
       #'marec-update))

(provide 'marec)
;;; marec.el ends here
