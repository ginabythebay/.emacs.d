;;; package --- Summary --- Gina Whites Emacs initialization for home linux machine

;;; Commentary:
;;; Stuff here

;;; Code:


(require 'gina-launcher)
(customize-set-variable
 'gina-default-buffers
 '((shells  "/sys" "/scratch" "/scan" "/music" "/pinky-src" "/pinky" "/boltbarn" "/blog")
   (files
    "~/Source/278"
    "~/Source/DOER/snippets/snippets.org"
    "~/finances/inbox"
    "~/scan"
    "~/.emacs.d/init.el"
    "~/.emacs.d/hosts/init-octavia.el")))

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

;;  https://emacs.stackexchange.com/a/17890
(require 'exec-path-from-shell)
(exec-path-from-shell-copy-env "SSH_AGENT_PID")
(exec-path-from-shell-copy-env "SSH_AUTH_SOCK")

(provide 'init-octavia)
;;; init-octavia.el ends here
