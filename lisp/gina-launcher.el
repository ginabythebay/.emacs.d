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
  "Open the case list."
  (interactive)
  (w32-browser "c:/Users/gina/Documents/Gina/CaseList.doc"))

(defun gina-refile ()
  "Open a new frame for filing things."
  (interactive)
  (select-frame-set-input-focus
   (make-frame `((fullscreen . maximized))))

  (split-window-right)

  (dired "c:/Users/gina/Documents/Downloads")
  (dired-sort-other "-lt")              ; also refreshes
  (dired "c:/Users/gina/Documents/Clare Lacy/ScannedDocuments")
  (dired-sort-other "-lt")              ; also refreshes

  (other-window 1)
  (dired "c:/Users/gina/Documents/Gina")

  (other-window 1))

(defun gina-open-explorer ()
  "Open windows explorer in the director for the buffer."
  (interactive)
  (if (eq major-mode 'dired-mode)
      (xah-show-in-desktop)
    (let ((dir (file-name-directory (buffer-file-name (buffer-base-buffer)))))
      (w32-shell-execute "explore"
                         (replace-regexp-in-string "/" "\\" dir t t)))))

(defun gina-org-jump-to-current-clock ()
  "Interactive version of 'org-clock-jump-to-current-clock."
  (interactive)
  (org-clock-jump-to-current-clock))

(provide 'gina-launcher)

(defhydra gina-launcher-hydra (:hint nil :exit t)
  "
(quit with _q_)
_b_: back (pop global mark)
_c_: jump to Current clock
_d_: toggle debug on error
_e_: open explorer
_i_: clock In to recent task
_l_: open case list
_r_: refile scans/downloads
_s_: iSearch forward
_t_: open Time sheet
_u_: bury buffer
_v_: toggle visual line mode
"
  ("q" nil)
  ("b" pop-global-mark :exit nil)
  ("c" gina-org-jump-to-current-clock)
  ("d" toggle-debug-on-error)
  ("e" gina-open-explorer)
  ("i" gina-clock-in-to-recent-task)
  ("l" gina-open-case-list)
  ("r" gina-refile)
  ("s" isearch-forward)
  ("t" gina-open-current-timesheet)
  ("u" bury-buffer)
  ("v" visual-line-mode))
;;; gina-launcher.el ends here
