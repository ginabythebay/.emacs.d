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

(provide 'gina-launcher)

(defhydra gina-launcher-hydra (:hint nil :exit t)
  "
(quit with _q_)
_b_: back (pop global mark)
_d_: toggle debug on error
_s_: iSearch forward
_u_: bury buffer
_v_: toggle visual line mode
"
  ("q" nil)
  ("b" pop-global-mark :exit nil)
  ("d" toggle-debug-on-error)
  ("i" gina-clock-in-to-recent-task)
  ("s" isearch-forward)
  ("u" bury-buffer)
  ("v" visual-line-mode))
;;; gina-launcher.el ends here
