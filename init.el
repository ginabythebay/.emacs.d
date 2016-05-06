;;; package --- Summary --- Gina Whites Emacs initialization

;;; Commentary:
;;; Stuff here
;; TODO(gina) projectile: http://batsov.com/projectile/
;; TODO(gina) zenburn theme: https://github.com/bbatsov/zenburn-emacs
;; TODO(gina) solarized theme: https://github.com/bbatsov/solarized-emacs
;; TODO(gina) investigate colors in compilation mode: http://endlessparentheses.com/ansi-colors-in-the-compilation-buffer-output.html?source=rss
;; TODO(gina) enable flyspell?
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

; consider using smartparens: https://github.com/Fuco1/smartparens/wiki
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

; If GOPATH isn't set, try to set it
(unless (getenv "GOPATH")
  (when (file-exists-p "~/go")
    (setenv "GOPATH" "~/go")))

; If GOPATH/bin isn't part of path and it exists, append it
(when (getenv "GOPATH")
  (let ((go-bin (concat (getenv "GOPATH") "/bin")))
    (when (and (not (path-contains "PATH" go-bin)) (file-exists-p go-bin))
      (setenv "PATH" (concat (getenv "PATH") ":" go-bin)))))

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

;; (smartparens-global-mode t)



;; (server-start)

(provide 'init)
;;; init.el ends here


 
;; SKIP?
;; (add-to-list 'load-path "~/.emacs.d/lisp")
;; (require 'auto-complete-config)
;; (add-to-list 'ac-dictionary-directories "~/.emacs.d/ac-dict")
;; (ac-config-default)






