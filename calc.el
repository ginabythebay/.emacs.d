;;; package --- Summary --- Gina White's Emacs Calc initialization

;;; Commentary:
;;; Stuff here


;;; Code:


(require 'calc-units)

(setq math-additional-units
      '(
        (usd nil "United States Dollar")))

(defun gaw-quick-calc ()
  "Support a popup quick calculator, meant to be called from the OS."
  (interactive)
  (select-frame-set-input-focus (selected-frame)))

;; First attempt at defining something in calc.  Assumes SFM is
;; surface feet per minute and DIAMETER is in inches.
;;
;; You can call this in elisp like:
;;   (setq var-diameter (calc-eval "3.82" 'raw))
;;   (setq var-sfm (calc-eval "100" 'raw))
;;   (calc-eval '("evalv(ilmrpm(sfm, diameter")' nil))
;;
;;  TODO(gina) try to understand calc units better and see if I can
;;  make this work in a way that is more compatible with the rest of
;;  calc (e.g. everything seems to assume meters).
;;
;; in calc, call this with algibriac mode like
;;  'ilmrpm(100, .5)
(defmath ilmrpm (sfm diameter)
  (* (/ sfm diameter) (/ 12 pi )))

;; Example call inside calc:
;;  'af(.55, .5, 100)
(defmath af (md cd lf)
  "Calc internal adjusted feed rate for major and cutter diameters.

  See URL 'https://www.harveyperformance.com/in-the-loupe/tag/internal-adjusted-feed/'.

  MD major (internal) diameter we are cutting.

  CD cutter diameter.

  LF feed rate we would use for a straight cut."
  (* (/ (- md cd) md) lf))


(provide 'calc)
;;; calc.el ends here



