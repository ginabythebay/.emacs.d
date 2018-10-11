;;; gina-keymap.el --- manages my keymap                     -*- lexical-binding: t; -*-

;; Copyright (C) 2017  Gina White

;; Author: Gina White <ginabythebay@gmail.com>
;; Keywords: lisp
;; Version: 0.0.1

;;; Commentary:

;; note much to say here

;;; Code:

;; code goes here


(defvar gina-map nil
  "My personal keymap.")

(global-unset-key (kbd "C-c 2"))

(define-prefix-command 'gina-map)
(global-set-key (kbd "C-2") 'gina-map)

(define-key gina-map (kbd "j") #'join-line)
(define-key gina-map (kbd "c") #'my-cleanup-region-date)
(define-key gina-map (kbd "t") #'ile-org-noter-dates)
(define-key gina-map (kbd "d") #'ile-duplicate)
(define-key gina-map (kbd "b") #'ile-jump-discovery)


(define-key gina-map (kbd "p") #'ile-org-noter-prev-page)
(define-key gina-map (kbd "n") #'ile-org-noter-next-page)

(define-key gina-map (kbd "z") #'ile-org-table-column-next-duplicate)

(provide 'gina-keymap)
;;; gina-keymap.el ends here
