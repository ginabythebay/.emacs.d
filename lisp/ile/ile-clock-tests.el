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

(ert-deftest ile-clock--minutes-to-duration ()
  "Tests time conversion"
  (should (equal (ile-clock--minutes-to-duration 83)
                 "1:23")))

(ert-deftest ile-clock--duration-to-minutes ()
  "Tests time conversion"
  (should (equal (ile-clock--duration-to-minutes "1:23")
                 83)))

(ert-deftest ile-clock--parse-clock ()
  "Test clock parsing."
  (with-temp-buffer
    (insert "* TC with Client
   :LOGBOOK:
   CLOCK: [2019-03-15 Fri 12:02]--[2019-03-15 Fri 12:09] =>  0:07")
    (should (equal (org-element-map (org-element-parse-buffer) 'clock
                     #'ile-clock--parse-clock nil nil)
                   '((:task "TC with Client"
                            :date "2019-03-15"
                            :minutes 7)))))
  ;; test that clock entries spanning dates raise an error
  (with-temp-buffer
    (insert "* TC with Client
   :LOGBOOK:
   CLOCK: [2019-03-14 Thu 12:02]--[2019-03-15 Fri 12:09] =>  24:07")
    (should-error (org-element-map (org-element-parse-buffer) 'clock
                    #'ile-clock--parse-clock nil nil))))

(ert-deftest ile-clock--combine-entries ()
  "Test clock combination"
  (should (equal (ile-clock--combine-entries
                  '((:task "TC with Client"
                            :date "2019-03-15"
                            :minutes 7)
                     (:task "TC with Client"
                             :date "2019-03-15"
                             :minutes 15)))
                 '(:task "TC with Client"
                            :date "2019-03-15"
                            :minutes 22))))

(ert-deftest ile-clock-entries ()
  "Test clock entry extraction."
  (with-temp-buffer
    (insert "* TC with Client
   :LOGBOOK:
   CLOCK: [2019-03-12 Wed 12:02]--[2019-03-12 Wed 12:09] =>  0:07
   CLOCK: [2019-03-12 Wed 12:02]--[2019-03-12 Wed 12:09] =>  0:17
   CLOCK: [2019-03-15 Fri 12:02]--[2019-03-15 Fri 12:09] =>  1:07
   :END:
* Analyze docs
   :LOGBOOK:
   CLOCK: [2019-03-13 Thu 12:02]--[2019-03-13 Thu 12:09] =>  0:08
   :END:")
    (should (equal (ile-clock-entries)
             '((:task "TC with Client"
                      :date "2019-03-12"
                      :minutes 24)
               (:task "Analyze docs"
                      :date "2019-03-13"
                      :minutes 8)
               (:task "TC with Client"
                      :date "2019-03-15"
                      :minutes 67))))))

;; ile-clock-test-current-file

(provide 'ile-clock-tests)
;;; ile-clock-tests.el ends here
