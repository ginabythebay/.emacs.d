(setq math-additional-units
      '(
        (usd nil "United States Dollar")))

(defun gaw-quick-calc ()
  "Support a popup quick calculator, meant to be called from the OS."
  (interactive)
  (select-frame-set-input-focus (selected-frame)))
