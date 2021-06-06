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

(defun gina-open-shell-quietly (buffer-name)
  "If a shell with BUFFER-NAME doesn't exist, opens it with now window."
  (unless (get-buffer buffer-name)
    (let ((display-buffer-overriding-action '((display-buffer-same-window))))
      (shell buffer-name)
      (bury-buffer))))

(defun gina-open-default-buffers ()
  "Open some default buffers I usually want."
  (interactive)
  (when (file-directory-p "~/scan")
    (dired "~/scan")
    (bury-buffer))

  (gina-open-shell-quietly "/sys")
  (gina-open-shell-quietly "/scratch")

  (message "default buffers opened"))

  (defun gina-refile ()
    "Open a new frame for filing things."
    (interactive)
    (select-frame-set-input-focus
     (make-frame `((fullscreen . maximized))))

    (split-window-right)

    (dired "c:/Users/gina/Documents/Downloads")
    (dired-sort-other "-lt")            ; also refreshes
    (dired "c:/Users/gina/Documents/Clare Lacy/ScannedDocuments")
    (dired-sort-other "-lt")            ; also refreshes

    (other-window 1)
    (dired "c:/Users/gina/Documents/Gina")

    (other-window 1))

(provide 'gina-launcher)

(defhydra gina-launcher-hydra (:hint nil :exit t)
  "
(quit with _q_)
_b_: back (pop global mark)
_d_: open default buffers
_e_: toggle debug on error
_s_: iSearch forward
_u_: bury buffer
_v_: toggle visual line mode
"
  ("q" nil)
  ("b" pop-global-mark :exit nil)
  ("d" gina-open-default-buffers)
  ("e" toggle-debug-on-error)
  ("i" gina-clock-in-to-recent-task)
  ("s" isearch-forward)
  ("u" bury-buffer)
  ("v" visual-line-mode))
;;; gina-launcher.el ends here
