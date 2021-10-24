;;; package --- Summary --- Gina Whites Emacs initialization for home linux machine

;;; Commentary:
;;; Stuff here

;;; Code:


(require 'gina-launcher)
(customize-set-variable
 'gina-default-buffers
 '((shells  "/sys" "/scratch")
   (files  "~/Source/278" "~/.emacs.d/init.el")))
xb
(use-package ledger-mode
  :ensure t
  :mode ("\\.ledger$" . ledger-mode)
  :config
  (require 'flycheck)
  (setq ledger-schedule-file "~/ledger-private/schedule.ledger")
  (setq ledger-post-amount-alignment-column 65)

  (use-package flycheck-ledger-tools-lint
    :ensure nil
    :load-path "lisp")
  (use-package flycheck-ledger
    :ensure
    :if (executable-find "ledger"))
  (flycheck-add-next-checker 'ledger '(warning . ledger-tools-lint)))

(provide 'init-octavia)
;;; init-octavia.el ends here
