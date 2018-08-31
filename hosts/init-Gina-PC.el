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

(defun org-user-idle-seconds ()
  "Return the current Win idle time in seconds."
  (/ (string-to-number (shell-command-to-string "C:/msys64/home/gina/bin/winidle.exe")) 1000))

(provide 'init-Gina-PC)
;;; init-Gina-PC.el ends here
