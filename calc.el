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
(defmath ilmrpm (sfm diameter)
  (* (/ sfm diameter) (/ 12 pi )))


(provide 'calc)
;;; calc.el ends here



