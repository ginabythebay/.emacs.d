;;; bates.el --- helps with bates numbers                     -*- lexical-binding: t; -*-

;; Copyright (C) 2018  Gina White

;; Author: Gina White <ginabythebay@gmail.com>
;; Keywords: lisp
;; Version: 0.0.1

;;; Commentary:

;; not much to say here

;;; Code:

;; code goes here

(require 'cl-macs)
(require 'seq)

(defvar bates--ring '()
  "Stores the last few (up to two) bates numbers marked.")

(defvar bates--max-bates-no '()
  "Stores the last valid bates number of a file when we marked a bates page.")

(cl-defstruct (bates-page (:constructor create--bates-page)
              (:copier nil))
  prefix        ; e.g. "COB"
  no            ; the numeric portion of the bates number
  (width nil))  ; if set, the length fill to when printing

(defun create-bates-page (prefix no width)
  "Create a bates number with no filling.
PREFIX is something like \"COB\".
NO is a bates number.
WIDTH is the minimum printed width of the bates number, if set."
  (create--bates-page :prefix prefix :no no :width width))

(defun bates--decode-bates-range (name)
  "Decodes a NAME like 'COB0002421-COB0003964' into ('COB' 2421 3964)."
  (unless (string-match "\\([A-Z]+\\)\\([0-9]+\\)[^A-Z]*\\([A-Z]+\\)\\([0-9]+\\)" name)
    (user-error "Unable to decode `%s': Expected something like COB0002421-COB0003964" name))
  (let* ((prefix1 (match-string 1 name))
        (start (match-string 2 name))
        (prefix2 (match-string 3 name))
        (end (match-string 4 name))
        (width nil))
    (unless (string= prefix1 prefix2)
      (user-error "Invalid name `%s', `%s'!=`%s'.  Expected something like COB0002421-COB0003964" name prefix1 prefix2))
    (when (equal ?0 (aref start 0))
        (unless (equal
                 (string-width start)
                 (string-width end))
          (user-error "Invalid digits in `%s'.  When leading zeros are found, the numeric portions must have the same length" name))
        (setq width (string-width start)))
    (list (create-bates-page prefix1 (string-to-number start) width)
     (create-bates-page prefix1 (string-to-number end ) width))))

(ert-deftest bates--decode-bates-range ()
  "Tests bates file base name decoding"
  (should (equal
           (bates--decode-bates-range "COB0002421-COB0003964")
           (list (create-bates-page "COB" 2421 7)
            (create-bates-page "COB" 3964 7))))
  (should (equal
           (bates--decode-bates-range "COB0002421 - COB0003964")
           (list (create-bates-page "COB" 2421 7)
            (create-bates-page "COB" 3964 7))))
  (should (equal
           (bates--decode-bates-range "COB2421-COB3964")
           (list (create-bates-page "COB" 2421 nil)
            (create-bates-page "COB" 3964 nil)))))


(defun bates--format (val &optional short)
  "Format VAL for printing.
Optional parameter SHORT means to use short form."
  (let ((fmt (if (and (not short) (bates-page-width val))
                 (format "%%s%%0%dd" (bates-page-width val))
               "%s %d")))
    (format fmt (bates-page-prefix val) (bates-page-no val))))

(ert-deftest bates--format ()
  "Tests bates formatting"
  (should (equal
           (bates--format (create-bates-page "COB" 2421 7))
           "COB0002421"))
  (should (equal
           (bates--format (create-bates-page "COB" 2421 7) t)
           "COB 2421"))
  (should (equal
           (bates--format (create-bates-page "COB" 2421 nil))
           "COB 2421"))
  (should (equal
           (bates--format (create-bates-page "COB" 2421 nil) t)
           "COB 2421")))

(defun bates--expected-pdf-pages (file-range)
  "Return the expected number of pdf pages in FILE-RANGE."
  (+ 1 (- (bates-page-no (nth 1 file-range)) (bates-page-no (nth 0 file-range)))))


(ert-deftest bates--expected-pdf-pages ()
  "Tests bates pdf page expectations."
  (should (equal
           1
           (bates--expected-pdf-pages
            (list (create-bates-page "COB" 2421 7)
                  (create-bates-page "COB" 2421 7)))))
  (should (equal
           4
           (bates--expected-pdf-pages
            (list (create-bates-page "COB" 2421 nil)
                  (create-bates-page "COB" 2424 nil))))))

(defun bates--ring-new (e)
  "Add E to bates--ring, ensuring only 2 items at most are retained."
  (setq bates--ring (append  bates--ring (list e)))
  (when (> (length bates--ring) 2)
    (setq bates--ring (seq-subseq bates--ring -2))))

(ert-deftest bates--ring-new ()
  "Tests bates ring adding."
  (setq bates--ring (list))

  (bates--ring-new (create-bates-page "COB" 2423 nil))
  (should (equal 1 (length bates--ring)))
  (should (equal 2423 (bates-page-no (car (last bates--ring)))))

  (bates--ring-new (create-bates-page "COB" 2424 nil))
  (should (equal 2 (length bates--ring)))
  (should (equal 2424 (bates-page-no (car (last bates--ring)))))

  (bates--ring-new (create-bates-page "COB" 2425 nil))
  (should (equal 2 (length bates--ring)))
  (should (equal 2425 (bates-page-no (car (last bates--ring))))))

(defun bates--copy ()
  "Copy the current bates entry to the bates--ring."
  (let* ((file-range (bates--decode-bates-range(file-name-base (buffer-file-name))))
         (start (nth 0 file-range))
         (end (nth 1 file-range))
         (expected-pdf-pages (bates--expected-pdf-pages file-range)))
    (unless (equal expected-pdf-pages (pdf-cache-number-of-pages))
      (user-error "Based on a start of `%s' and an end of %d, we expected %d pages but found %d"
                  start end expected-pdf-pages (pdf-cache-number-of-pages)))

    (cl-incf (bates-page-no start) (- (pdf-view-current-page) 1))
    (bates--ring-new start)
    (setq bates--max-bates-no (bates-page-no end))

    (message "stored %s" (bates--format start))))

(defun bates--ordered-ring ()
  "Return the contents of the bates--ring in order."
  (let ((a (nth 0 bates--ring))
        (b (nth 1 bates--ring)))
    (unless (equal (bates-page-prefix a) (bates-page-prefix b))
      (user-error "It doesn't make sense to combine the prefixes `%s' and `%s'"
                  (bates-page-prefix a) (bates-page-prefix b)))

    ; no matter what order they were marked, put the small number first
    (if (< (bates-page-no a) (bates-page-no b))
        (list a b)
      (list b a))))

(defun bates--paste-text ()
  "Get the text to paste based on the bates--ring."
  (unless (> (length bates--ring) 1)
    (user-error "Not enough entries.  Run this command in pdf-view-mode at least twice first"))

  (let* ((ring (bates--ordered-ring))
         (a (nth 0 ring))
         (b (nth 1 ring)))
    (let ((atext (bates--format a t))
          (btext (bates--format b t)))
      (if (equal atext btext)
          atext
        (format "%s - %s" atext btext)))))


(ert-deftest bates--paste-text ()
  "Tests bates pdf page expectations."
  (setq bates--ring (list
                     (create-bates-page "COB" 2421 7)
                     (create-bates-page "COB" 2421 7)))
  (should (equal "COB 2421" (bates--paste-text)))
  (setq bates--ring (list
                     (create-bates-page "COB" 2421 6)
                     (create-bates-page "COB" 2423 6)))
  (should (equal "COB 2421 - COB 2423" (bates--paste-text)))
  (setq bates--ring (list
                     (create-bates-page "COB" 2423 nil)
                     (create-bates-page "COB" 2421 nil)))
  (should (equal "COB 2421 - COB 2423" (bates--paste-text))))


(defun bates--ring-maybe-next ()
  "Put the next page in the ring if won't exceed the last entry."
  (let* ((biggest (car (last (bates--ordered-ring))))
         (new (create-bates-page
                (bates-page-prefix biggest)
                (+ 1 (bates-page-no biggest))
                (bates-page-width biggest))))
    (when (<= (bates-page-no new) bates--max-bates-no)
      (bates--ring-new new)
      (bates--ring-new new))))

(ert-deftest bates--ring-maybe-next ()
  "Tests bates pdf page expectations."
  (setq bates--ring '())
  (bates--ring-new (create-bates-page "COB" 2423 nil))
  (bates--ring-new (create-bates-page "COB" 2421 nil))

  (setq bates--max-bates-no 3000)
  (bates--ring-maybe-next)
  (should (equal 2424 (bates-page-no (car (last bates--ring))))))


(defun bates--paste ()
  "Pastes the last two entries into the current buffer."
  (unless (> (length bates--ring) 1)
    (user-error "Not enough entries.  Run this command in pdf-view-mode at least twice first"))
  (kill-new (bates--paste-text))
  (yank)
  (bates--ring-maybe-next))

(defun bates-copy-or-paste ()
  "Copies the current bates number for the pdf in the current buffer to the kill ring."
  (interactive)
  (if (equal major-mode 'pdf-view-mode)
      (bates--copy)
    (bates--paste)))

(global-set-key "\C-b" 'bates-copy-or-paste)

(provide 'bates)
;;; bates.el ends here
