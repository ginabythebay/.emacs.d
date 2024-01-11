;;; package --- Summary --- Gina Whites Emacs initialization for DOER windows machine

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

(customize-set-variable 'ispell-program-name "c:/Program Files/hunspell/bin/hunspell.exe")

(defun org-user-idle-seconds ()
  "Return the current Win idle time in seconds."
  (/ (string-to-number (shell-command-to-string "C:/msys64/home/gina/bin/winidle.exe")) 1000))

(defvar my-doc-dir
  (if (string= system-type "gnu/linux")
      "/mnt/c/Users/gina/Documents/Gina/"
    "c:/Users/gina/Documents/Gina/"))


(require 'org)

;; See https://systemcrafters.net/publishing-websites-with-org-mode/building-the-site/
(setq org-html-validation-link nil      ;; Don't show validation link
      org-html-head-include-scripts nil ;; Use our own scripts
      org-html-head-include-default-style nil ;; Use our own styles
      org-html-head "<link rel=\"stylesheet\" href=\"https://cdn.simplecss.org/simple.min.css\" />")

(setq browse-url-browser-function 'browse-url-generic
      browse-url-generic-program "/mnt/c/Program Files/Google/Chrome/Application/chrome.exe")

(require 'gina-launcher)
(customize-set-variable
 'gina-default-buffers
 '((shells  "/scratch")
   (files
    "~/Source/DOER"
    "/mnt/c/users/machinist/Source/f360_setup_sheet/README.md"
    "~/Source/DOER/snippets/snippets.org"
    "~/.emacs.d/init.el"
    "~/Source/DOER/org-roam/20230329133600-batch.org"
    "~/.emacs.d/hosts/init-DOER-Gina.el")))


(provide 'init-DOER-Gina)
;;; init-DOER-Gina.el ends here
