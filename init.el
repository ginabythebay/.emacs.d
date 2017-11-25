;;; package --- Summary --- Gina Whites Emacs initialization

;;; Commentary:
;;; Stuff here
;; TODO(gina) enable gocode?  https://github.com/nsf/gocode

(setq-default indent-tabs-mode nil)

(setq visible-bell t)

;;; Code:

;; Support functions

(defun path-contains (haystack needle)
  "Return t if HAYSTACK has a path element NEEDLE."
  (let ((result nil))
    (dolist (token (split-string (getenv haystack) ":"))
      (when (string-match needle token)
	(setq result t)))
    result))

;; Get all this machine-written custom crud out of my cleanish init.el file.
;; See http://emacsblog.org/2008/12/06/quick-tip-detaching-the-custom-file/
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file 'noerror)

;; Stop asking me this already
(setq vc-follow-symlinks t)


; from https://aur.archlinux.org/packages/go-fonts-git/
(set-frame-font "Go Mono-12" nil t)

;; These come from https://fonts.google.com/?category=Monospace
;(set-frame-font "Roboto Mono-12" nil t)
;(set-frame-font "Inconsolata-16" nil t)
;(set-frame-font "PT Mono-14" nil t)
;(set-frame-font "Ubuntu Mono-16" nil t)
;(set-frame-font "Cousine-14" nil t)
;(set-frame-font "Oxygen Mono-14" nil t)

;; Save whatever’s in the current (system) clipboard before
;; replacing it with the Emacs’ text.
;; http://pragmaticemacs.com/emacs/add-the-system-clipboard-to-the-emacs-kill-ring/
;; https://github.com/dakrone/eos/blob/master/eos.org
(setq save-interprogram-paste-before-kill t)

; Turn on view mode for read-only files
; http://pragmaticemacs.com/emacs/view-mode-makes-for-great-read-only-reading/
(setq view-read-only t)

; Faster than default scp, according to
; https://www.emacswiki.org/emacs/TrampMode
(setq tramp-default-method "ssh")

;; colors in compilation mode: http://endlessparentheses.com/ansi-colors-in-the-compilation-buffer-output.html?source=rss
(require 'ansi-color)
(require 'compile)
(defun endless/colorize-compilation ()
  "Colorize from `compilation-filter-start' to `point'."
  (let ((inhibit-read-only t))
    (ansi-color-apply-on-region
     compilation-filter-start (point))))

(add-hook 'compilation-filter-hook
          #'endless/colorize-compilation)

(setq compilation-scroll-output 'first-error)

(show-paren-mode 1)
(defadvice show-paren-function
     (after show-matching-paren-offscreen activate)
     "If the matching paren is offscreen, show matching line in the echo area.
Has no effect if the character before point is not of
the syntax class ')'."
     (interactive)
     (let* ((cb (char-before (point)))
            (matching-text (and cb
                                (char-equal (char-syntax cb) ?\) )
                                (blink-matching-open))))
       (when matching-text (message matching-text))))

(require 'package)
(setq package-enable-at-startup nil)
; marmalade has expired certs as of 11/13/16.  skip it for now
(setq package-archives
        '(("gnu" . "http://elpa.gnu.org/packages/")
          ("melpa" . "http://melpa.org/packages/")
          ("melpa-stable" . "http://stable.melpa.org/packages/")
          ;("marmalade" . "http://marmalade-repo.org/packages/")
          ))
(setq package-archive-priorities
      '(("melpa-stable" . 20)
        ;("marmalade" . 20)
        ("gnu" . 10)
        ("melpa" . 0)))
(setq package-menu-hide-low-priority t)
(package-initialize)

;; Bootstrap `use-package'
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(global-set-key (kbd "C-f") 'fill-paragraph)

(use-package ivy
  :ensure t
  :diminish ivy-mode
  :init
  (require 'hydra)
  :config
  (ivy-mode 1)
  (bind-key "C-c C-r" 'ivy-resume))

(use-package counsel
  :ensure t
  :bind
  ("M-x" . counsel-M-x)
  ("C-x C-f" . counsel-find-file)
  ; these don't work
  ;("[f1] f" . counsel-describe-function)
  ;("[f1] v" . counsel-describe-variable)
  ;("[f1] l" . counsel-find-library)
  ;("[f2] i" . counsel-info-lookup-symbol)
  ;("[f2] u" . counsel-unicode-char)
  ("C-c g" . counsel-git)
  ("C-c j" . counsel-git-grep)
  ("C-c k" . counsel-ag)
  ("C-x l" . counsel-locate)
  ("C-S-o" . counsel-rhythmbox))
  ; these also don't work
  ;("C-z f" . counsel-describe-function)
  ;("C-z v" . counsel-describe-variable)
  ;("C-c k" . counsel-ag))

;; Themes
(use-package zenburn-theme
  :ensure t)
(use-package solarized-theme
  :ensure t)
;; Our default, for now
(load-theme 'zenburn)

;; Project management.
(use-package projectile
  :ensure t
  :config
  (projectile-global-mode t)
  (setq projectile-create-missing-test-files t)
  (setq projectile-switch-project-action #'projectile-commander))

(use-package gina-keymap
  :load-path "lisp")

(use-package multiple-cursors
  :ensure t
  :demand
  :bind (
	 ("C--" . mc/mark-next-like-this)
	 (:map gina-map
	      ("m" . mc/edit-lines)
	      ("a" . mc/mark-all-like-this))))

; see http://pragmaticemacs.com/emacs/copy-and-paste-files-with-dired-ranger/
(use-package dired-ranger
  :ensure t
  :demand
  :bind (:map dired-mode-map
              ("W" . dired-ranger-copy)
              ("X" . dired-ranger-move)
              ("Y" . dired-ranger-paste)
	      ("`" . dired-ranger-bookmark-visit)))

;; This package is a little janky, but probably better than me doing it all manually
;; Under the covers, this runs shell initialization, and copies the values of the resulting
;; environment variables back up into emacs.  Useful in the land of mac, where
;; environment variable are completely mysterious to me.
(use-package exec-path-from-shell
  :ensure t
  :if (memq window-system '(mac ns))
  :config
    (require 'eshell)  ;; next command has a bug that assumes eshell is loaded
    (exec-path-from-shell-copy-env "LANG")
    (exec-path-from-shell-copy-env "PATH")
    (exec-path-from-shell-copy-env "MANPATH")
    (exec-path-from-shell-copy-env "GOPATH"))

(use-package crux
  :bind (
	 ("C-a" . crux-move-beginning-of-line)
	 ("C-c r" . crux-rename-file-and-buffer)))

(use-package bm
  :ensure t
  :bind (("<C-f2>" . bm-toggle)
         ("<f2>" . bm-next)
         ("<S-f2>" . bm-previous))
  :config (setq bm-cycle-all-buffers t))

(use-package pt)

; While I like fish shell in terminals, I can't get it to look right in emacs shell
(when (file-exists-p "/bin/bash")
  (customize-set-variable 'explicit-shell-file-name "/bin/bash")
  (customize-set-variable 'explicit-bash-args '("--noediting" "--login" "-i")))

;; Notes for getting hunspell working on the mac.
;; Install the binary using brew
;; Get the dictionaries from a libreoffice extension.  This worked:
;; http://extensions.libreoffice.org/extension-center/english-dictionaries
;; Unzip the ofx file.  Copy *.aff and *.dic into ~/Library/Spelling/
(add-hook 'text-mode-hook 'flyspell-mode)
(add-hook 'prog-mode-hook 'flyspell-prog-mode)

(use-package highlight-symbol
  :ensure t
  :config
  (add-hook 'prog-mode-hook 'highlight-symbol-mode))

;; TODO(gina) delete after 8/22/2016.  Using bookmarks package (above) instead
;(use-package home-buffer
;  :load-path "lisp"
;  :bind (([f11] . switch-to-home-buffer)
;	 ([(control f11)] . set-current-buffer-to-home-buffer)
;	 ([f2] . switch-to-other-buffer)))

;; TODO(gina) look into getting a bunch of snippets, including go.  See https://github.com/capitaomorte/yasnippet
(use-package yasnippet
  :ensure t
  :config
  (yas-reload-all)
  (add-hook 'prog-mode-hook #'yas-minor-mode))

(use-package which-key
  :ensure t
  :config (which-key-mode))

(use-package expand-region
  :ensure t
  :bind (([(control =)] . er/expand-region)))

(use-package avy
  :ensure t
  :bind (("M-s" . avy-goto-word-1)))

(use-package ledger-mode
  :ensure t
  :mode ("\\.ledger$" . ledger-mode)
  :config
  (setq ledger-schedule-file "~/ledger-private/schedule.ledger")
  (setq ledger-post-amount-alignment-column 65))

(use-package flycheck-ledger-tools-lint
  :load-path "lisp")

(use-package flycheck-ledger
  :ensure t
  :if (executable-find "ledger"))

(flycheck-add-next-checker 'ledger '(warning . ledger-tools-lint))

(use-package company
  :ensure t
  :config
  ; when running mnt-gdrive inside emacs (for development) and trying
  ; to test things inside emacs, in shell mode, emacs locks up if
  ; company mode is on.
  ; ledger-mode has its own compltetion
  (setq company-global-modes `(not shell-mode ledger-mode))
  (setq company-dabbrev-downcase nil)
  (global-company-mode))


;; 8/23/16: Not sure if I like this, but giving it a try
;;
;; see https://github.com/Fuco1/smartparens
;; see https://ebzzry.github.io/emacs-pairs.html
(require 'help-mode) ;; smartparens cheat sheet will fail without this
(use-package smartparens
  :ensure t
  :bind
  ("C-c (" . wrap-with-parens)
  ("C-c [" . wrap-with-brackets)
  ("C-c {" . wrap-with-braces)
  ("C-c '" . wrap-with-single-quotes)
  ("C-c \"" . wrap-with-double-quotes)
  ("C-c `" . wrap-with-back-quotes)
  ("C-c u" . sp-unwrap-sexp)
  :demand
  :config
  (show-smartparens-global-mode 1)
  (add-hook 'prog-mode-hook 'turn-on-smartparens-mode)
  (add-hook 'markdown-mode-hook 'turn-on-smartparens-mode)

  (defmacro def-pairs (pairs)
    `(progn
       ,@(loop for (key . val) in pairs
	       collect
	       `(defun ,(read (concat
			       "wrap-with-"
			       (prin1-to-string key)
			       "s"))
		    (&optional arg)
		  (interactive "p")
		  (sp-wrap-with-pair ,val)))))

  (def-pairs ((paren        . "(")
	      (bracket      . "[")
	      (brace        . "{")
	      (single-quote . "'")
	      (double-quote . "\"")
	      (back-quote   . "`")))

  (sp-local-pair 'go-mode "{" nil :post-handlers '((my-create-newline-and-enter-sexp "RET")))
  (sp-local-pair 'go-mode "(" nil :post-handlers '((my-create-newline-and-enter-sexp "RET")))
  (sp-local-pair 'go-mode "[" nil :post-handlers '((my-create-newline-and-enter-sexp "RET")))
  (sp-local-pair 'go-mode "`" nil :post-handlers '((my-create-newline-and-enter-sexp "RET")))

  (defun my-create-newline-and-enter-sexp (&rest _ignored)
    "Open a new brace or bracket expression, with relevant newlines and indent. "
    (newline)
    (indent-according-to-mode)
    (forward-line -1)
    (indent-according-to-mode))
  )

;; js2 (javascript) mode

(use-package js2-mode
  :mode (("\\.js$" . js2-mode))
  :interpreter ("node" . js2-mode)
  :config (add-hook 'js2-mode-hook (
            (lambda ()
	      (define-key js2-mode-map (kbd "C-c C-c") 'projectile-compile-project)
	      (setq js2-basic-offset 2))))
          ;; from https://benhollis.net/blog/2015/12/20/nodejs-stack-traces-in-emacs-compilation-mode/
          (setq compilation-error-regexp-alist-alist
                (cons '(node "^[  ]+at \\(?:[^\(\n]+ \(\\)?\\([a-zA-Z\.0-9_/-]+\\):\\([0-9]+\\):\\([0-9]+\\)\)?$"
                             1 ;; file
                             2 ;; line
                             3 ;; column
                             )
                      compilation-error-regexp-alist-alist))
          (setq compilation-error-regexp-alist
                (cons 'node compilation-error-regexp-alist))
  )

;; BEGIN JSON CONFIGURATION

(use-package json-mode
  :ensure t)

(use-package flymake-json
  :ensure t
  :config (add-hook 'js-mode-hook 'flymake-json-maybe-load))




;; BEGIN TYPESCRIPT CONFIGURATION

(use-package typescript-mode
  :ensure t)

(use-package ts-comint
  :ensure t
  :config (add-hook 'typescript-mode-hook
	    (lambda ()
	      (local-set-key (kbd "C-x C-e") 'ts-send-last-sexp)
	      (local-set-key (kbd "C-M-x") 'ts-send-last-sexp-and-go)
	      (local-set-key (kbd "C-c b") 'ts-send-buffer)
	      (local-set-key (kbd "C-c C-b") 'ts-send-buffer-and-go)
	      (local-set-key (kbd "C-c l") 'ts-load-file-and-go))))

;; BEGIN GO CONFIGURATION

(use-package company-go
  :ensure t
  :commands company-go
  :init (add-to-list 'company-backends 'company-go))

(use-package go-mode
  :ensure t
  :mode ("\\.go$" . go-mode)
  :config (add-hook 'go-mode-hook
	    (lambda ()
	      (define-key go-mode-map (kbd "C-c C-c") 'compile)
	      (define-key go-mode-map (kbd "C-c h") 'godoc-at-point)
	      ; use guru for highlighting instead
	      (set (make-local-variable 'highlight-symbol-mode) nil)
	      (add-hook 'before-save-hook 'gofmt-before-save)
	      (setq tab-width 4)
	      (setq gofmt-command "goimports")
	      (setq indent-tabs-mode 1))))


(use-package go-expanderr
  :load-path "~/go/src/github.com/stapelberg/expanderr/lisp")

(use-package go-autocomplete
  :ensure t)


;; depends on https://github.com/cweill/gotests
(use-package gotests
  :load-path "lisp")

(use-package go-guru
  :ensure t
  :config
  (add-hook 'go-mode-hook #'go-guru-hl-identifier-mode))

(eval-and-compile
  (defun go-lint-load-path ()
    (concat (getenv "GOPATH")  "/src/github.com/golang/lint/misc/emacs")))
(use-package golint
  :load-path (lambda() (list (go-lint-load-path))))

;; END GO CONFIGURATION

(use-package flycheck
  :ensure t
  :config
    (global-flycheck-mode)
    (setq flycheck-go-vet-shadow t))

(use-package yaml-mode
  :ensure t
  :mode ("\\.yml\\'" . yaml-mode))

(use-package js2-mode
  :ensure t
  :mode ("\\.js\\'" . js2-mode)
  :config (setq js-indent-level 2))

(use-package gradle-mode
  :ensure t
  :mode ("\\.gradle\\'" . gradle-mode))

(use-package groovy-mode
  :ensure t
  :mode ("\\.gradle\\'" . groovy-mode))

(use-package web-mode
  :ensure t
  :mode (("\\.phtml\\'" . web-mode)
	 ("\\.tpl\\.php\\'" . web-mode)
	 ("\\.jsp\\'" . web-mode)
	 ("\\.as[cp]x\\'" . web-mode)
	 ("\\.erb\\'" . web-mode)
	 ("\\.mustache\\'" . web-mode)
	 ("\\.djhtml\\'" . web-mode)
	 ("\\.html?\\'" . web-mode)))

(use-package markdown-mode
  :ensure t
  :mode ("\\.md\\'" . markdown-mode)
  :config (let ((mm "/usr/local/bin/multimarkdown"))
	    (when (file-exists-p mm)
	      (setq markdown-command mm))))

(use-package magit
  :ensure t
  :bind ("C-x g" . magit-status))

(setq-default show-trailing-whitespace t)

(defun my-shell-setup ()
  "Turn off trailing whitespace here."
  (setq show-trailing-whitespace nil))
(setq shell-mode-hook 'my-shell-setup)

;; C
(setq c-default-style "stroustrup"
      c-basic-offset 2)
(setq c-default-style "whitesmith"
      c-basic-offset 2)

(use-package anaconda-mode
  :ensure t
  :config (add-hook 'python-mode-hook 'anaconda-mode))

(use-package company-anaconda
  :ensure t
  :commands company-anaconda
  :init (add-to-list 'company-backends 'company-anaconda))

;; Python
(setq c-default-style "python"
      c-basic-offset 2)
;; see https://github.com/boorad/emacs/blob/master/python/my-python-compile.el#L19
;; TODO(gina) extrat this stuff out to a separate file I can import
(defun my-python-mode-hook ()
  "Make compile available by keyboard."
  (local-unset-key "\C-c\C-c")
  (global-unset-key "\C-c\C-c")
  (local-set-key "\C-c\C-c" 'compile))
(add-hook 'python-mode-hook 'my-python-mode-hook)

;; Java
(setq c-default-style "java"
      c-basic-offset 2)

(delete-selection-mode t)
(setq column-number-mode t)  ;; put line number in mode line.

(server-start)

(provide 'init)
;;; init.el ends here


;; These are things from my old init file that I haven't decided yet if I want
;; SKIP?
;; (add-to-list 'load-path "~/.emacs.d/lisp")
;; (require 'auto-complete-config)
;; (add-to-list 'ac-dictionary-directories "~/.emacs.d/ac-dict")
;; (ac-config-default)
;;
;; (autoload 'gtags-mode "gtags" "" t)
;; 
;; (require 'deft)
;; (setq deft-directory "/home/gina/docs/deft")
;; (setq deft-extension "org")
;; (setq deft-text-mode 'org-mode)
;; (setq deft-use-filename-as-title t)
;; (setq deft-auto-save-interval 0)
;; ;;key to launch deft
;; (global-set-key (kbd "C-c d") 'deft)


(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'narrow-to-region 'disabled nil)
