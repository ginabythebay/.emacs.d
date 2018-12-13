;;; marec.el --- manages my launcher                     -*- lexical-binding: t; -*-

;; Copyright (C) 2018  Gina White

;; Author: Gina White <ginabythebay@gmail.com>
;; Keywords: lisp
;; Version: 0.0.1

;;; Commentary:

;; not much to say here

;;; Code:

;; code goes here

;;;###autoload
(defun marec-status ()
  "Look at open buffers potentially needing recovery."
  (interactive)
  (let ((buf (marec-update)))
    (with-current-buffer (marec-update)
      (marec-mode))
    (pop-to-buffer buf)))

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
                 (insert "  " (buffer-name c))))
      (if candidates-p
          (message "%d buffers appear to be recoverable" (length candidates))
        (message "No buffers appear to be recoverable")))
    status-buffer))

(defalias #'marec-status 'marec)

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
    (define-key map (kbd "g") #'marec-update)
    map))


(define-derived-mode marec-mode special-mode "Marec"
  "A major mode for magic recovery."
  (setq buffer-read-only t)
  (buffer-disable-undo)
  (set (make-local-variable 'revert-buffer-function)
       #'marec-update))

  (provide 'marec)
;;; marec.el ends here
