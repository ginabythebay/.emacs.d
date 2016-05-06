;;; home-buffer.el --- manages the idea of a single home buffer                     -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Gina White

;; Author: Gina White <ginabythebay@gmail.com>
;; Keywords: lisp
;; Version: 0.0.1

;;; Commentary:

;; Tracks a single home buffer that we can switch to quickly

;;; Code:

;; code goes here

(defvar home-buffer nil
  "The buffer currently designated as home")

(defun set-current-buffer-to-home-buffer () 
  "Saves the current buffer as the home buffer"
  (interactive)
  (setq home-buffer (current-buffer))
  (message "Home buffer was set to %s" home-buffer))

(defun switch-to-home-buffer ()
  "Switches to the home buffer"
  (interactive)
  (switch-to-buffer home-buffer))

(provide 'home-buffer)
;;; home-buffer.el ends here
