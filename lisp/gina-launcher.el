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
(when (string= system-type "windows-nt")
  (require 'w32-browser))

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

(defun gina-open-case-list ()
  "Open the case list on Dan's PC."
  (interactive)
  (w32-browser "//Dan-PC/Users/Dan/Documents/Case List/Case List (2).doc"))

(provide 'gina-launcher)

(defhydra gina-launcher-hydra (:hint nil :exit t)
  "
(quit with _q_)
_b_: back (pop global mark)
_c_: jump to Current clock
_d_: toggle debug on error
_i_: clock In to recent task
_l_: open case list
_s_: iSearch forward
_t_: open Time sheet
_u_: bury buffer
"
  ("q" nil)
  ("b" pop-global-mark)
  ("c" org-clock-jump-to-current-clock)
  ("d" toggle-debug-on-error)
  ("i" gina-clock-in-to-recent-task)
  ("l" gina-open-case-list)
  ("s" isearch-forward)
  ("t" gina-open-current-timesheet)
  ("u" bury-buffer))
;;; gina-launcher.el ends here
