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

(defconst emacs-info-dir "c:/users/gina/Desktop/emacs/share/info")

(defun my-fix-info-path ()
  "Fix Info-directory-list.

Something keeps stomping on it.  Until I figure out what that is,
make it easier to fix."
  (interactive)
  (unless (member emacs-info-dir Info-directory-list)
    (push emacs-info-dir Info-directory-list)))

(require 'printing)
(setq pr-path-alist
	    '((windows   "c:/applications/executables" PATH ghostview)
	      (ghostview "C:/Program Files/Artifex Software/gsview6.0/bin")))
(setq pr-ps-name 'ricoh)
(setq pr-ps-printer-alist
      '((ricoh "print" nil nil "\\\\192.168.1.35\\AFICIOMP6002")))
(pr-update-menus t)

(setq w32-apps-modifier 'super) ;; App key, aka context menu key

(require 'dired)

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
