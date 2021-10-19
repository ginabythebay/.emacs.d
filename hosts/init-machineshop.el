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

;(defconst emacs-info-dir "c:/users/gina/Desktop/emacs/share/info")

;(defun my-fix-info-path ()
;  "Fix Info-directory-list.

;Something keeps stomping on it.  Until I figure out what that is,
;make it easier to fix."
;  (interactive)
;  (unless (member emacs-info-dir Info-directory-list)
;    (push emacs-info-dir Info-directory-list)))

;(require 'printing)
;(setq pr-path-alist
;	    '((windows   "c:/applications/executables" PATH ghostview)
;	      (ghostview "C:/Program Files/Artifex Software/gsview6.0/bin")))
;(setq pr-ps-name 'ricoh)
;(setq pr-ps-printer-alist
;      '((ricoh "print" nil nil "\\\\192.168.1.35\\AFICIOMP6002")))
;(pr-update-menus t)

(setq w32-apps-modifier 'super) ;; App key, aka context menu key

(require 'dired)

(when (string= system-type "gnu/linux")
  (use-package w3m
    :ensure t
    :config
    (setq browse-url-generic-program (executable-find "w3m")
          browse-url-browser-function 'w3m)))

(use-package w32-browser
  :ensure t
  :bind (:map dired-mode-map
              ("M-RET" . dired-w32-browser)))

(setq-default  ispell-program-name "c:/Program Files/hunspell/bin/hunspell.exe")

(defun org-user-idle-seconds ()
  "Return the current Win idle time in seconds."
  (/ (string-to-number (shell-command-to-string "C:/msys64/home/gina/bin/winidle.exe")) 1000))

(defvar my-doc-dir
  (if (string= system-type "gnu/linux")
      "/mnt/c/Users/gina/Documents/Gina/"
    "c:/Users/gina/Documents/Gina/"))

;; See https://systemcrafters.net/publishing-websites-with-org-mode/building-the-site/
(setq org-html-validation-link nil      ;; Don't show validation link
      org-html-head-include-scripts nil ;; Use our own scripts
      org-html-head-include-default-style nil ;; Use our own styles
      org-html-head "<link rel=\"stylesheet\" href=\"https://cdn.simplecss.org/simple.min.css\" />")

(require 'org)
;(setq org-agenda-files
;      (mapcar
;       (lambda (r) (concat my-doc-dir r))
;       (quote ("Alsayyad/Alsayyad notes.org"
;               "Armstrong Temple/Armstrongtemple notes.org"
;               "Gamino/Gamino notes.org"
;               "Gantt/Gantt Notes.org"
;               "Eid/Eid Notes.org"
;               "Horn/Horn notes.org"
;               "Mangano/Mangano notes.org"
;               "Marin/Marin notes.org"
;               "Nevitt/Nevitt notes.org"
;               "Stec/Stec notes.org"
;               "Sullivan/Sullivan notes.org"
;               "Overall notes.org"
;               "rojas/Rojas notes.org"
;               "saw.org"
;               "Smith/Smith notes.org"
;               "Williams/Williams notes.org"))))

(provide 'init-machineshop)
;;; init-machineshop.el ends here
