;;; bates-tests.el --- tests for bates.el -*- lexical-binding: t; -*-

;; Copyright (C) 2018  Gina White

;; Author: Gina White <ginabythebay@gmail.com>
;; Keywords: lisp
;; Version: 0.0.1

;;; Commentary:

;; not much to say here

;;; Code:

;; code goes here

(require 'bates)
(require 'ert)

(ert-deftest bates--calc-width ()
  "Tests bates file width calculations"
  (should (equal
           (bates--calc-width
            "16-0433 PITCHESS 10229-10736" "10229" "10736")
           nil))
  (should-error (bates--calc-width
                 "16-0433 PITCHESS 010229-0010736" "010229" "0010736"))
  (should (equal
           (bates--calc-width
            "16-0433 PITCHESS 0010229-0010736" "0010229" "0010736")
           7)))

(ert-deftest bates-parse-filename-to-range ()
  "Tests bates file base name decoding"
  (should (equal
           (bates-parse-filename-to-range "COB0002421-COB0003964")
           (list (create-bates-page "COB" 2421 7)
                 (create-bates-page "COB" 3964 7))))
  (should (equal
           (bates-parse-filename-to-range "COB0002421 - COB0003964")
           (list (create-bates-page "COB" 2421 7)
                 (create-bates-page "COB" 3964 7))))
  (should (equal
           (bates-parse-filename-to-range "COB2421-COB3964")
           (list (create-bates-page "COB" 2421 nil)
                 (create-bates-page "COB" 3964 nil))))
  (should (equal
           (bates-parse-filename-to-range "GANTT 563-894 Communications, etc")
           (list (create-bates-page "GANTT" 563 nil)
                 (create-bates-page "GANTT" 894 nil))))
  (should (equal
           (bates-parse-filename-to-range "16-0433 PITCHESS 10229-10736")
           (list (create-bates-page "PITCHESS" 10229 nil)
                 (create-bates-page "PITCHESS" 10736 nil)))))

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

(ert-deftest bates--ring-maybe-next ()
  "Tests bates pdf page expectations."
  (setq bates--ring '())
  (bates--ring-new (create-bates-page "COB" 2423 nil))
  (bates--ring-new (create-bates-page "COB" 2421 nil))

  (setq bates--max-bates-no 3000)
  (bates--ring-maybe-next)
  (should (equal 2424 (bates-page-no (car (last bates--ring))))))

(ert-deftest bates--range ()
  "Tests bates range."
  (should (equal
           (bates--range "794" "795")
           "794 - 795"))
  (should (equal
           (bates--range "794" "794")
           "794"))
  (should (equal
           (bates--range "794" "795" "AA")
           "AA 794 - 795"))
  (should (equal
           (bates--range "794" "794" "AA")
           "AA 794")))

(ert-deftest bates--range-link ()
  "Tests bates range linking."
  (should (equal
           (bates--range-link "foo.pdf" "794" "795")
           "[[foo.pdf][794 - 795]]"))
  (should (equal
           (bates--range-link "foo.pdf" "794" "794")
           "[[foo.pdf][794]]"))
  (should (equal
           (bates--range-link "foo.pdf" "794" "794" '(("page" . 3) ("zoom" . 100)))
           "[[foo.pdf#zoom=100&page=3][794]]")))

(provide 'bates-tests)
;;; bates-tests.el ends here
