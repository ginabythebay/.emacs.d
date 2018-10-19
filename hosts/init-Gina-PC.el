;;; package --- Summary --- Gina Whites Emacs initialization for SYB windows machine

;;; Commentary:
;;; Stuff here

;;; Code:

; backup configuration
; see https://www.gnu.org/software/emacs/manual/html_node/emacs/Backup.html
; see https://www.emacswiki.org/emacs/BackupDirectory
(setq
   backup-directory-alist
    '(("." . ".saves/"))    ; don't litter my fs tree
   delete-old-versions t
   kept-new-versions 15
   kept-old-versions 0
   version-control t)       ; use versioned backups

(require 'printing)
(setq pr-path-alist
	    '((windows   "c:/applications/executables" PATH ghostview)
	      (ghostview "C:/Program Files/Artifex Software/gsview6.0/bin")
	      ))
(setq pr-ps-name       'ricoh)
(setq pr-ps-printer-alist
      '((ricoh  "print"     nil nil "\\\\192.168.1.35\\AFICIOMP6002")
        ))
(pr-update-menus t)

;; See http://ergoemacs.org/emacs/emacs_hyper_super_keys.html
(setq w32-pass-lwindow-to-system nil)
w32-lwindow-modifier 'super ; Left Windows key

setq w32-pass-rwindow-to-system nil
setq w32-rwindow-modifier 'super ; Right Windows key

;;(setq w32-pass-apps-to-system nil)
;;(setq w32-apps-modifier 'hyper) ; Menu/App key


(use-package w32-browser
  :ensure t
  :bind (:map dired-mode-map
              ("M-RET" . dired-w32-browser)))

(setq-default  ispell-program-name "c:/mingw64/bin/hunspell.exe")

(defun org-user-idle-seconds ()
  "Return the current Win idle time in seconds."
  (/ (string-to-number (shell-command-to-string "C:/msys64/home/gina/bin/winidle.exe")) 1000))

(provide 'init-Gina-PC)
;;; init-Gina-PC.el ends here
