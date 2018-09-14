;;; org-noter-override.el --- org overrides

;; Copyright (C) 2018  Gina White

;; Author: Gina White <ginabythebay@gmail.com>
;; Keywords: lisp
;; Version: 0.0.1

;;; Commentary:

;; extending/overriding org-noter

;;; Code:

;; code goes here

(require 'org-noter)


(defun org-cycle-hide-drawers (state &optional exceptions)
  "Does nothing.  STATE is ignored.  EXCEPTIONS is ignored.")

(provide 'org-noter-override)
;;; org-noter-override.el ends here
