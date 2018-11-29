;;; ile-company.el --- Integrated Legal Environment -*- lexical-binding: t; -*-

;; Copyright (C) 2018  Gina White

;; Author: Gina White <ginabythebay@gmail.com>
;; Keywords: lisp
;; Version: 0.0.1

;;; Commentary:

;; Code for company completions.  See
;; http://justinhj.github.io/2018/10/24/radix-trees-dash-and-company-mode.html
;; for inspiration.

;;; Code:

;; code goes here

(require 'company)
(require 'pdf-cache)
(require 'pdf-info)
(require 'pdf-view)
(require 'radix-tree)
(require 'subr-x)


(defun ile-company--pdf-window ()
  "Return the window containing an open pdf, or nil if none found.

We evaluate all windows in the current frame, starting with the selected window."
  (cl-loop with sw = (selected-window)
           with candidate = sw
           when (with-current-buffer
                    (window-buffer candidate)
                  (string= major-mode "pdf-view-mode")) return candidate
           do (setq candidate (next-window candidate))
           until (eq candidate sw)))

(defun ile-company--pdf-text ()
  "Return text for the current pdf page."
  (when-let* ((w (ile-company--pdf-window)))
    (with-selected-window w
      (let ((p (pdf-view-current-page)))
        (message "text: %S" (pdf-info-gettext p (pdf-info-boundingbox p)))))))

;; abusing the namespace here because define-pdf-cache-function
;; expects the pdf-info prefix.
(defun pdf-info-radix-tree (page)
  "Return the text on PAGE as a radix tree."
  ;; TODO(ginabythebay) Look into using weak references ala
  ;; https://nullprogram.com/blog/2012/12/17/ to automatically free of
  ;; memory.
  (when-let* ((text (pdf-info-gettext page (pdf-info-boundingbox page)))
              (words (split-string text)))
    (seq-reduce (lambda (acc it) (radix-tree-insert acc it t)) words radix-tree-empty)))

(define-pdf-cache-function radix-tree t)

(defun ile-company--radix ()
  "Return radix tree for current pdf page."
  (when-let* ((w (ile-company--pdf-window)))
    (with-selected-window w
      (pdf-cache-radix-tree (pdf-view-current-page)))))

(defun ile-company--get-candidates (prefix)
  "Given a PREFIX, return a list of matching words that begin with it."
  (when (> (length prefix) 2)
    (when-let* ((tree (ile-company--radix)))
      (let ((keys '()))
        (radix-tree-iter-mappings (radix-tree-subtree tree prefix)
			          (lambda (key _)
                                    (setq keys (cons (concat prefix key) keys))))
        keys))))

;;;###autoload
(defun ile-company-pdf-dictionary (command &optional arg &rest _)
  "Company mode back end for a custom pdf page using COMMAND and ARG."
  (cl-case command
    ('prefix
     (when (ile-company--radix)
       (company-grab-word)))
    ('candidates
     (ile-company--get-candidates arg))
    ('ignore-case
     nil)))

(provide 'ile-company)
;;; ile-company.el ends here

