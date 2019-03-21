;;; ile-clock-tests.el --- tests for bates.el -*- lexical-binding: t; -*-

;; Copyright (C) 2018  Gina White

;; Author: Gina White <ginabythebay@gmail.com>
;; Keywords: lisp
;; Version: 0.0.1

;;; Commentary:

;; to run these tests:
;; emacs -L . -batch -l ert -l ile-clock-tests.el -f ert-run-tests-batch-and-exit

;;; Code:

;; code goes here

(require 'ile-clock)
(require 'ert)
(require 'org)
(require 'org-clock)

(ert-deftest ile-clock--minutes-to-duration ()
  "Tests time conversion"
  (should (equal (ile-clock--minutes-to-duration 83)
                 "1:23")))

(ert-deftest ile-clock--duration-to-minutes ()
  "Tests time conversion"
  (should (equal (ile-clock--duration-to-minutes "1:23")
                 83)))

(defmacro with-temp-org-buffer (contents &rest body)
  "Like `with-temp-buffer', but we insert CONTENTS and set `org-mode' before calling BODY."
  (declare (indent 0) (debug t))
  (let ((temp-buffer (make-symbol "temp-buffer")))
    `(let ((,temp-buffer (generate-new-buffer " *temp*")))
       ;; FIXME: kill-buffer can change current-buffer in some odd cases.
       (with-current-buffer ,temp-buffer
         (unwind-protect
	     (progn
               (insert ,contents)
               (org-mode)
               ,@body)
           (and (buffer-name ,temp-buffer)
                (kill-buffer ,temp-buffer)))))))

(ert-deftest ile-clock--parse-clock ()
  "Test clock parsing."

  (let* ((rstart 0.0)
         (rend (float-time
                (apply #'encode-time
                       (org-parse-time-string "2019-03-20 Wed 09:17"))))
         (fn (lambda (e) (ile-clock--parse-clock rstart rend e))))
    (with-temp-org-buffer "#+FILETAGS: :bob:
* TC with Client
   :LOGBOOK:
   CLOCK: [2019-03-15 Fri 12:02]--[2019-03-15 Fri 12:09] =>  0:07"
      (should (equal (org-element-map (org-element-parse-buffer)
                         'clock fn nil nil)
                     '((:filetags ":bob:"
                                  :task "TC with Client"
                                  :date "2019-03-15"
                                  :minutes 7)))))
    ;; outside the range
    (with-temp-org-buffer "#+FILETAGS: :bob:
* TC with Client
   :LOGBOOK:
   CLOCK: [2019-03-22 Fri 12:02]--[2019-03-22 Fri 12:10] =>  0:08"
      (should (equal (org-element-map (org-element-parse-buffer)
                         'clock fn nil nil)
                     nil)))
    ;; test that clock entries spanning dates raise an error
    (with-temp-org-buffer "#+FILETAGS: :bob:
* TC with Client
   :LOGBOOK:
   CLOCK: [2019-03-14 Thu 12:02]--[2019-03-15 Fri 12:09] =>  24:07"
      (should-error (org-element-map (org-element-parse-buffer)
                        'clock fn nil nil)))))

(ert-deftest ile-clock--combine-entries ()
  "Test clock combination"
  (should (equal (ile-clock--combine-entries
                  '((:filetags ":bob:"
                     :task "TC with Client"
                     :date "2019-03-15"
                     :minutes 7)
                    (:filetags ":bob:"
                     :task "TC with Client"
                     :date "2019-03-15"
                     :minutes 15)))
                 '(:filetags ":bob:"
                   :task "TC with Client"
                   :date "2019-03-15"
                   :minutes 22))))

(ert-deftest ile-clock-entries ()
  "Test clock entry extraction."
  (with-temp-org-buffer "#+FILETAGS: :bob:
* TC with Client
   :LOGBOOK:
   CLOCK: [2019-03-12 Wed 12:02]--[2019-03-12 Wed 12:09] =>  0:07
   CLOCK: [2019-03-12 Wed 12:02]--[2019-03-12 Wed 12:09] =>  0:17
   CLOCK: [2019-03-15 Fri 12:02]--[2019-03-15 Fri 12:09] =>  1:07
   :END:
* Analyze docs
   :LOGBOOK:
   CLOCK: [2019-03-13 Thu 12:02]--[2019-03-13 Thu 12:09] =>  0:08
   :END:"
    ;; with no range specified
    (should (equal (ile-clock-entries)
                   '((:filetags ":bob:"
                      :task "TC with Client"
                      :date "2019-03-12"
                      :minutes 24)
                     (:filetags ":bob:"
                      :task "Analyze docs"
                      :date "2019-03-13"
                      :minutes 8)
                     (:filetags ":bob:"
                      :task "TC with Client"
                      :date "2019-03-15"
                      :minutes 67))))
    ;; test specifying a range
    (let ((rstart (apply #'encode-time
                       (org-parse-time-string "2019-01-01 Fri 09:17")))
          (rend (apply #'encode-time
                       (org-parse-time-string "2019-03-14 Fri 09:17"))))
      (should (equal (ile-clock-entries nil (list rstart rend))
                     '((:filetags ":bob:"
                        :task "TC with Client"
                        :date "2019-03-12"
                        :minutes 24)
                       (:filetags ":bob:"
                        :task "Analyze docs"
                        :date "2019-03-13"
                        :minutes 8)))))))

;; ile-clock-test-current-file

(provide 'ile-clock-tests)
;;; ile-clock-tests.el ends here
