
;; not yet ready for prime time, so leaving it out of init.el 

(use-package denote
  :ensure t

  :commands denote-org-capture denote-dired-rename-file
  ;; the convert command below doesn't exist in the released (0.3.1)
  ;; version of denote.  Look into straight.el to load 0.4.0-dev?
  denote-dired-convert-file-to-denote 

  :init
  (require 'denote-dired)
  (require 'subr-x)
  
  (with-eval-after-load 'org-capture
    (require 'denote-org-capture)
    (add-to-list 'org-capture-templates
                 '("n" "New note (with Denote)" plain
                   (file denote-last-path)
                   #'denote-org-capture
                   :no-save t
                   :immediate-finish nil
                   :kill-buffer t
                   :jump-to-captured t)))

  

  :config
  (setq denote-dired-directories
        (list denote-directory))

  (setq denote-known-keywords '("tools" "machines" "contacts" "dailies" "weeklies"))

  (add-hook 'dired-mode-hook #'denote-dired-mode)


  (require 'savehist)
  (setq savehist-file (locate-user-emacs-file "savehist"))
  (setq history-length 500)
  (setq history-delete-duplicates t)
  (setq savehist-save-minibuffer-history t)
  (add-hook 'after-init-hook #'savehist-mode)

  (setq denote-infer-keywords t)
  (setq denote-sort-keywords t)
  (setq denote-prompts '(title keywords))

  ;; You will not need to `require' all those individually once the
  ;; package is available.
  (require 'denote-retrieve)
  (require 'denote-link)

  ;; By default, we fontify backlinks in their bespoke buffer.
  (setq denote-link-fontify-backlinks t)

  ;; Also see `denote-link-backlinks-display-buffer-action' which is a bit
  ;; advanced.

  ;; If you use Markdown or plain text files (Org renders links as buttons
  ;; right away)
  (add-hook 'find-file-hook #'denote-link-buttonize-buffer)

  ;; Here is a custom, user-level command from one of the examples we
  ;; showed in this manual.  We define it here and add it to a key binding
  ;; below.
  (defun my-denote-journal ()
    "Create an entry tagged 'journal', while prompting for a title."
    (interactive)
    (denote
     (denote--title-prompt)
     '("journal"))))
