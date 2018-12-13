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
  (let* ((inhibit-read-only t)
         (candidates (seq-filter #'marec-buf-candidate-p (buffer-list)))
         (candidates-p (not (zerop (length candidates))))
         (status-buffer (get-buffer-create "*marec status*")))
    (with-current-buffer status-buffer
      (setq buffer-read-only t)
      (erase-buffer)
      (if (not candidates-p)
          (insert "No files recoverable buffers open")
        (insert "The following open buffers appear to be recoverable:")
        (cl-loop for c in candidates
                 do
                 (newline)
                 (insert "  " (buffer-name c))))

      (when candidates-p
        (pop-to-buffer status-buffer))

      (if candidates-p
          (message "%d buffers appear to be recoverable" (length candidates))
        (message "No buffers appear to be recoverable"))
      )
    )
  )


(defun marec-buf-candidate-p (buf)
  "Return non-nil if BUF is a candidate for recovery.
Based on code in `after-find-file'"
  (with-current-buffer buf
    (when (and buffer-file-name (file-writable-p buffer-file-name))
      (file-newer-than-file-p (or buffer-auto-save-file-name
				  (make-auto-save-file-name))
			      buffer-file-name))))


(provide 'marec)
;;; marec.el ends here
