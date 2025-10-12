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

(buffer-name)
(defun gina-find-file-quietly (file-name)
  "If a shell with FILE-NAME doesn't exist, opens it with now window."
  (unless (get-file-buffer file-name)
      (find-file file-name)
      (bury-buffer)))

(defcustom gina-default-buffers
  '((shells "/scratch") (files "~/.emacs.d" "~/.emacs.d/init.el"))
  "Default buffers to open when 'gina-open-default-buffers' is called.

  An alist where the allowed KEYS are 'files or 'shells and the
  VALUES are lists of entries to open.  The entries for the
  'files key can be directories of files."

  :type 'sexp
  :group 'gina
  )

(defun gina-open-default-buffers ()
  "Open some buffers declared in 'gina-default-buffers'."
  (interactive)

  (let ((files 0)
        (shells 0))

    (cl-loop for x in (alist-get 'shells gina-default-buffers)
             do (gina-open-shell-quietly x)
             (setq shells (+ shells 1)))
    (cl-loop for x in (alist-get 'files gina-default-buffers)
             do (if (file-directory-p x)
                    (progn
                      (dired x)
                      (bury-buffer))
                  (gina-find-file-quietly x))
             (setq files (+ files 1)))

    (message "Opened %d files and %d shells" files shells)))

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

(defun split-3-windows-vertically-evenly ()
  (interactive)
  (command-execute 'split-window-vertically)
  (command-execute 'split-window-vertically)
  (command-execute 'balance-windows)
)

(require 'hydra)
(defhydra gina-launcher-hydra (:hint nil :exit t)
  "
(quit with _q_)
_b_: back (pop global mark)
_d_: open default buffers
_e_: toggle debug on error
_p_: export parts to batch file
_s_: iSearch forward
_u_: bury buffer
_v_: split 3 windows vertically
"
  ("q" nil)
  ("b" pop-global-mark :exit nil)
  ("d" gina-open-default-buffers)
  ("e" toggle-debug-on-error)
  ("i" gina-clock-in-to-recent-task)
  ("p" gina-export-batch)
  ("s" isearch-forward)
  ("u" bury-buffer)
  ("v" split-3-windows-vertically-evenly))

(provide 'gina-launcher)
;;; gina-launcher.el ends here
