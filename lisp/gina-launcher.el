;;; gina-launcher.el --- manages my launcher                     -*- lexical-binding: t; -*-

;; Copyright (C) 2017  Gina White

;; Author: Gina White <ginabythebay@gmail.com>
;; Keywords: lisp
;; Version: 0.0.1

;;; Commentary:

;; note much to say here

;;; Code:

;; code goes here

(require 'org-clock)
(require 'w32-browser)

(defvar timesheet-dir "c:/Users/gina/Documents/Gina/Noncases/timesheets/")

(defun gina-clock-in-to-recent-task ()
  "Prompt the user for a recent task and clock in to it."
  (interactive)
  (org-clock-in '(4)))

(defun gina-open-current-timesheet ()
  "Open the current time sheet.  Assumes windows."
  (interactive)
  (let ((candidates (directory-files timesheet-dir t "Time Sheet")))
    (unless (eq (length candidates) 1)
      (user-error
       "Expected to find exactly one timesheet in %S but found %S"
       timesheet-dir
       (pp-to-string candidates)))
    (w32-browser (car candidates))))

(defhydra gina-launcher-hydra (:hint nil :exit t)
  "
(quit with _q_)
_b_: bury buffer
_d_: toggle debug on error
_t_: open Time sheet
_i_: clock In to recent task
_c_: jump to Current clock"
  ("q" nil)
  ("b" bury-buffer)
  ("d" toggle-debug-on-error)
  ("t" gina-open-current-timesheet)
  ("i" gina-clock-in-to-recent-task)
  ("c" org-clock-jump-to-current-clock))

(provide 'gina-launcher)
;;; gina-launcher.el ends here
