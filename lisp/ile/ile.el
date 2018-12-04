;;; ile-org-noter.el --- Integrated Legal Environment  -*- lexical-binding: t -*-

;; Copyright (C) 2018  Gina White

;; Author: Gina White <ginabythebay@gmail.com>
;; Keywords: lisp
;; Version: 0.0.1

;;; Commentary:

;; General stuff for ile

;;; Code:

;; code goes here

(require 'ile-navigation)

(defconst ile--exclude-bates-names
  '("CA"  ;; our fair state
    "SBN" ;; state bar number
    ;; todo keywords
    "TODO" "NEXT" "DONE" "WAITING" "HOLD" "CANCELLED" "PHONE" "MEETING"))

(defun ile--bates-matcher (limit)
  "Decide if we match a potential bates value, for font locking.

LIMIT is the limit of the search."
  (when (search-forward-regexp ile-org--bates-re limit t)
    (let ((name (match-string 1)))
      (unless (member name ile--exclude-bates-names)
        t))))

(defmacro ile--highlights (matcher)
  "Create an entry for 'font-lock-keywords based on the regular expression MATCHER."
  `(list ,matcher 0 font-lock-comment-face 'prepend))

(defconst ile--keywords
  (list
   (list 'ile--bates-matcher 0 font-lock-comment-face 'prepend)
   ;; avoid highlighting dates that are part of clock entries or
   ;; scheduling entries
   (ile--highlights (concat "[^[<]" ile-org--date-re))
   (ile--highlights ile-org--fed-rule-re)))

;;;###autoload
(define-minor-mode ile-mode
  "Integrated Legal Environment."
  nil " ile" nil

  (if ile-mode
      (font-lock-add-keywords nil ile--keywords t)
    (font-lock-remove-keywords nil ile--keywords))
  (if (fboundp #'font-lock-flush)
      (font-lock-flush)
    (when font-lock-mode
      (with-no-warnings (font-lock-fontify-buffer)))))

(provide 'ile)
;;; ile.el ends here


