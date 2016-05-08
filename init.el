;;; package --- Summary --- Gina Whites Emacs initialization

;;; Commentary:
;;; Stuff here
;; TODO(gina) projectile: http://batsov.com/projectile/
;; TODO(gina) investigate colors in compilation mode: http://endlessparentheses.com/ansi-colors-in-the-compilation-buffer-output.html?source=rss
;; TODO(gina) enable gocode?  https://github.com/nsf/gocode

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
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))

(package-initialize)

;; Bootstrap `use-package'
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; Themes
(use-package zenburn-theme
  :ensure t)
(use-package solarized-theme
  :ensure t)
;; Our default, for now
(load-theme 'zenburn)

;; I cannot decide if I want to dig into this or not
;(use-package smartparens
;  :ensure t
;  :config (smartparens-global-mode 1))

;; This package is a little janky, but probably better than me doing it all manually
;; Under the covers, this runs shell initialization, and copies the values of the resulting
;; environment variables back up into emacs.  Useful in the land of mac, where
;; environment variable are completely mysterious to me.
(use-package exec-path-from-shell
  :ensure t
  :if (memq window-system '(mac ns))
  :config
    (require 'eshell)  ;; next command has a bug that assumes eshell is loaded
    (exec-path-from-shell-copy-env "PATH")
    (exec-path-from-shell-copy-env "MANPATH")
    (exec-path-from-shell-copy-env "GOPATH"))

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
  :config (add-hook 'prog-mode-hook 'highlight-symbol-mode))

(use-package home-buffer
  :load-path "lisp"
  :bind (([f11] . switch-to-home-buffer)
	 ([(control f11)] . set-current-buffer-to-home-buffer)
	 ([f2] . switch-to-other-buffer)))

;; TODO(gina) look into getting a bunch of snippets, including go.  See https://github.com/capitaomorte/yasnippet
(use-package yasnippet
  :ensure t
  :config (setq yas-global-mode 1))

(use-package which-key
  :ensure t
  :config (which-key-mode))

(use-package avy
  :ensure t
  :bind (("M-s" . avy-goto-word-1)))

(use-package ledger-mode
  :ensure t
  :mode ("\\.ledger$" . ledger-mode))

;; BEGIN GO CONFIGURATION

(use-package go-mode
  :ensure t
  :mode ("\\.go$" . go-mode)
  :config (add-hook 'go-mode-hook
	    (lambda ()
	      (define-key go-mode-map (kbd "C-c C-c") 'compile)
	      (add-hook 'before-save-hook 'gofmt-before-save)
	      (setq tab-width 4)
	      (setq gofmt-command "goimports")
	      (setq indent-tabs-mode 1))))

(use-package go-autocomplete
  :ensure t)

;; Configure golint if it is installed
(let (
  (lint-cfg-path (eval-and-compile (concat (getenv "GOPATH")  "/src/github.com/golang/lint/misc/emacs"))))
  (use-package golint
    :if (and (getenv "GOPATH") (file-exists-p lint-cfg-path))
    :load-path lint-cfg-path))

;; END GO CONFIGURATION

(use-package flycheck
  :ensure t
  :config (global-flycheck-mode))

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

;; C
(setq c-default-style "stroustrup"
      c-basic-offset 2)
(setq c-default-style "whitesmith"
      c-basic-offset 2)

;; Python
(setq c-default-style "python"
      c-basic-offset 2)

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


