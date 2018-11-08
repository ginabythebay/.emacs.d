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

(defmacro ile--highlights (re)
  "Create an entry for 'font-lock-keywords based on the regular expression RE."
  `(list ,re 0 font-lock-comment-face 'prepend))

(defconst ile--keywords
  (list
   (ile--highlights ile-org--bates-re)
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


