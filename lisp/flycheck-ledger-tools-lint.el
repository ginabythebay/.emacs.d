;;; flycheck-ledger-tools-lint.el --- glue for flycheck                     -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Gina White

;; Author: Gina White <ginabythebay@gmail.com>
;; Keywords: lisp
;; Version: 0.0.1

;;; Commentary:

;; Provides a flycheck definition for ledger-tools

;;;; Setup

;; (eval-after-load 'flycheck '(require 'flycheck-ledger-tools-lint))

;;; Code:

;; code goes here

(require 'flycheck)

(flycheck-define-checker ledger-tools-lint
  "A ledger tool lint checker.

See URL `https://github.com/ginabythebay/ledger-tools'."
  :command ("ledger-tools" "lint" "--checkstyle" "-f" source-inplace)
  :error-parser flycheck-parse-checkstyle
  :modes ledger-mode)

(add-to-list 'flycheck-checkers 'ledger-tools-lint)

(provide 'flycheck-ledger-tools-lint)
;;; flycheck-ledger-tools-lint.el ends here
