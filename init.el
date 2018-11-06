;;; package --- Summary --- Gina Whites Emacs initialization

;;; Commentary:
;;; Stuff here
;; TODO(gina) enable gocode?  https://github.com/nsf/gocode

(setq-default indent-tabs-mode nil)

(setq visible-bell t)

;;; Code:

;; Support functions

(add-to-list 'load-path (concat user-emacs-directory "lisp/"))

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


(global-auto-revert-mode 1)

(winner-mode 1)

;; enable gpg encryption
;; see https://stackoverflow.com/questions/41741477/emacs-epa-and-gnupg2-no-usable-configuration?rq=1
(require 'epa-file)
(custom-set-variables '(epg-gpg-program (executable-find "gpg")))
(epa-file-enable)

;; from https://aur.archlinux.org/packages/go-fonts-git/
;; Note that on linux I had 12 points, but on windows 11 felt better.
;; Might need to make this dependant on the screen or something.
(set-frame-font "Go Mono-11" nil t)

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
     (let ((cb (char-before (point))))
       (when (and cb (char-equal (char-syntax cb) ?\) ))
         (blink-matching-open))))

(require 're-builder)

(require 'package)
(setq package-enable-at-startup nil)
(setq package-archives
        '(("gnu" . "http://elpa.gnu.org/packages/")
          ("melpa" . "http://melpa.org/packages/")
          ("melpa-stable" . "http://stable.melpa.org/packages/")
          ("org" . "https://orgmode.org/elpa/")))
(setq package-archive-priorities
      '(("melpa-stable" . 20)
        ("gnu" . 10)
        ("melpa" . 0)))
(setq package-menu-hide-low-priority t)
(package-initialize)

;; Bootstrap `use-package'
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)
(setq use-package-always-ensure t)

(use-package benchmark-init
  :ensure t
  :config
  ;; To disable collection of benchmark data after init is done.
  (add-hook 'after-init-hook 'benchmark-init/deactivate))

(use-package key-chord
  :config
  (key-chord-mode 1))

(use-package use-package-chords
  :ensure t)

;; see https://nicolas.petton.fr/blog/per-computer-emacs-settings.html
(defconst my-host (substring (shell-command-to-string "hostname") 0 -1))
(let ((host-dir "~/.emacs.d/hosts/")
      (init-host-feature (intern (concat "init-" my-host))))
  (add-to-list 'load-path host-dir)
  (require init-host-feature))

(use-package deadgrep
  :ensure t
  :bind
  ("s-d" . deadgrep))

(use-package hydra
  :ensure t)

(use-package beacon
  :ensure t
  :config
  (beacon-mode 1))

(use-package paradox
  :ensure t
  :defer t
  :config
  (paradox-enable))

(use-package delight
  :ensure t)

(use-package autorevert
  :delight auto-revert-mode)

(use-package helpful
  :ensure t
  :config
  (defun my-helpful-switch-buffer (buf &optional action norecord)
    ;; if already in a helpful buffer, stay in the same window
    (if (memq major-mode '(helpful-mode Info-mode))
        (pop-to-buffer-same-window buf norecord)
      (pop-to-buffer buf action norecord)))
  (customize-set-variable helpful-switch-buffer-function #'my-helpful-switch-buffer)
  :bind
  ("C-h f" . helpful-callable)
  ("C-h v" . helpful-variable)
  ("C-h k" . helpful-key)
  ("C-c C-d" . helpful-at-point))

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
  ("C-x l" . counsel-locate)
  ("C-S-o" . counsel-rhythmbox))
  ; these also don't work
  ;("C-z f" . counsel-describe-function)
  ;("C-z v" . counsel-describe-variable)

;; Themes
(use-package zenburn-theme
  :ensure t)
(use-package solarized-theme
  :ensure t)
(use-package tangotango-theme
  :ensure t)
;; Our default, for now
(load-theme 'tangotango)

(use-package undo-tree
  :ensure t
  :config
  (global-undo-tree-mode)
  (setq undo-tree-visualizer-timestamps t)
  :diminish undo-tree-mode)

;; Project management.
(use-package projectile
  :ensure t
  :config
  (projectile-mode t)
  (setq projectile-create-missing-test-files t
        projectile-switch-project-action #'projectile-commander
        projectile-completion-system 'helm
        projectile-mode-line '(:eval (format " [%s]" (projectile-project-name))))
  :bind-keymap
  ("s-p" . projectile-command-map))

(use-package gina-keymap
  :load-path "lisp"
  :ensure nil)

(use-package multiple-cursors
  :after (gina-keymap)
  :ensure t
  :bind (
	 ("C--" . mc/mark-next-like-this)
	 (:map gina-map
	      ("m" . mc/edit-lines)
	      ("a" . mc/mark-all-like-this))))

(use-package dired
  :ensure nil
  :config
  (require 'dired-x))

; see http://pragmaticemacs.com/emacs/copy-and-paste-files-with-dired-ranger/
(use-package dired-ranger
  :ensure t
  :demand
  :bind (:map dired-mode-map
              ("W" . dired-ranger-copy)
              ("X" . dired-ranger-move)
              ("Y" . dired-ranger-paste)
	      ("`" . dired-ranger-bookmark-visit)))


(defun my-kill-all-pdf-view-buffers ()
  "Iterate through all buffers and kill the ones with major mode ‘pdf-view-mode’."
  (interactive)
  (let ((buffers (buffer-list)))
    (while buffers
      (with-current-buffer (car buffers)
        (when (string= "pdf-view-mode" major-mode)
          (kill-buffer)))
      (setq buffers (cdr buffers)))))

(defun my-wdired-finish-edit ()
  "Kill any open pdf view buffers then call ‘wdired-finish-edit’."
  (interactive)
  (my-kill-all-pdf-view-buffers)
  (wdired-finish-edit))

(defun my-dired-display-file ()
  "Open file cursor is on in other window and return to current window."
  (interactive)
  (let ((orig-window (selected-window)))
    (dired-find-file-other-window)
    (select-window orig-window)))


(defun my-dired-move-beginning-line ()
  "Move point to beginning of file name or beginning of line.
Inspired by crux-beginning-of-line."
  (interactive)
  (let ((orig-point (point)))
    (dired-move-to-filename)
    (when (= orig-point (point))
      (move-beginning-of-line 1))))

(require 'wdired)
(add-hook 'wdired-mode-hook
           (lambda ()
	     (local-set-key (kbd "C-a") 'my-dired-move-beginning-line)
	     (local-set-key (kbd "C-o") 'my-dired-display-file)
	     (local-set-key (kbd "C-c C-c") 'my-wdired-finish-edit)))

(advice-add
 'dired-do-flagged-delete
 :before (lambda (&optional blah) (my-kill-all-pdf-view-buffers)))

(advice-add
 'dired-ranger-move
 :before (lambda (&optional blah) (my-kill-all-pdf-view-buffers)))

;; This package is a little janky, but probably better than me doing it all manually
;; Under the covers, this runs shell initialization, and copies the values of the resulting
;; environment variables back up into emacs.  Useful in the land of mac, where
;; environment variable are completely mysterious to me.
;; TODO(gina) make this only work on mac?
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

(use-package company
  :ensure t
  :diminish company-mode
  :config
  ;; when running mnt-gdrive inside emacs (for development) and trying
  ;; to test things inside emacs, in shell mode, emacs locks up if
  ;; company mode is on.
  ;; ledger-mode has its own completion
  (when (equal my-host "octavia")
    (setq company-global-modes `(not shell-mode ledger-mode))
    )
  (setq company-idle-delay .2
        company-minimum-prefix-length 3
        company-show-numbers t
        company-selection-wrap-around t
        company-dabbrev-downcase nil)
  (global-company-mode))


(use-package company-quickhelp
  :after (company)
  :config
  (setq company-quickhelp-delay .1)
  :bind (:map company-active-map
              ("C-c h" . company-quickhelp-manual-begin)))


(use-package rainbow-delimiters
  :ensure t
  :config
  (add-hook 'emacs-lisp-mode-hook 'rainbow-delimiters-mode)

  (set-face-foreground 'rainbow-delimiters-depth-1-face "white")
  (set-face-foreground 'rainbow-delimiters-depth-2-face "cyan")
  (set-face-foreground 'rainbow-delimiters-depth-3-face "yellow")
  (set-face-foreground 'rainbow-delimiters-depth-4-face "green")
  (set-face-foreground 'rainbow-delimiters-depth-5-face "orange")
  (set-face-foreground 'rainbow-delimiters-depth-6-face "purple")
  (set-face-foreground 'rainbow-delimiters-depth-7-face "white")
  (set-face-foreground 'rainbow-delimiters-depth-8-face "cyan")
  (set-face-foreground 'rainbow-delimiters-depth-9-face "yellow")
  (set-face-foreground 'rainbow-delimiters-unmatched-face "red"))

;; 8/23/16: Not sure if I like this, but giving it a try
;;
;; see https://github.com/Fuco1/smartparens
;; see https://ebzzry.github.io/emacs-pairs.html
(require 'help-mode) ;; smartparens cheat sheet will fail without this
(use-package smartparens
  :after (hydra)
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
;; Hydra for Smartparens, from https://github.com/lunaryorn/old-emacs-configuration/blob/1d9f6656877386273e000a30cb67670d64b73759/init.el
  (defhydra lunaryorn-smartparens (:hint nil :exit t)
    "
Sexps (quit with _q_)
^Nav^            ^Barf/Slurp^                 ^Depth^
^---^------------^----------^-----------------^-----^-----------------
_f_: forward     _<right>_:   slurp forward   _R_:      splice
_b_: backward    _<left>_:    barf forward    _r_:      raise
_u_: backward ↑  _C-<left>_:  slurp backward  _<up>_:   raise backward
_d_: forward ↓   _C-<right>_: barf backward   _<down>_: raise forward
_p_: backward ↓
_n_: forward ↑
^Kill^           ^Misc^                       ^Wrap^
^----^-----------^----^-----------------------^----^------------------
_w_: copy        _j_: join                    _(_: wrap with ( )
_k_: kill        _s_: split                   _{_: wrap with { }
^^               _t_: transpose               _'_: wrap with ' '
^^               _c_: convolute               _\"_: wrap with \" \"
^^               _i_: indent defun"
    ("q" nil)
    ;; Wrapping
    ("(" (lambda (a) (interactive "P") (sp-wrap-with-pair "(")))
    ("{" (lambda (a) (interactive "P") (sp-wrap-with-pair "{")))
    ("'" (lambda (a) (interactive "P") (sp-wrap-with-pair "'")))
    ("\"" (lambda (a) (interactive "P") (sp-wrap-with-pair "\"")))
    ;; Navigation
    ("f" sp-forward-sexp nil :exit nil)
    ("b" sp-backward-sexp nil :exit nil)
    ("u" sp-backward-up-sexp nil :exit nil)
    ("d" sp-down-sexp nil :exit nil)
    ("p" sp-backward-down-sexp nil :exit nil)
    ("n" sp-up-sexp nil :exit nil)
    ;; Kill/copy
    ("w" sp-copy-sexp)
    ("k" sp-kill-sexp)
    ;; Misc
    ("t" sp-transpose-sexp)
    ("j" sp-join-sexp)
    ("s" sp-split-sexp)
    ("c" sp-convolute-sexp)
    ("i" sp-indent-defun)
    ;; Depth changing
    ("R" sp-splice-sexp)
    ("r" sp-splice-sexp-killing-around)
    ("<up>" sp-splice-sexp-killing-backward)
    ("<down>" sp-splice-sexp-killing-forward)
    ;; Barfing/slurping
    ("<right>" sp-forward-slurp-sexp)
    ("<left>" sp-forward-barf-sexp)
    ("C-<left>" sp-backward-barf-sexp)
    ("C-<right>" sp-backward-slurp-sexp))

  (key-chord-define-global "kk" #'lunaryorn-smartparens/body)

  (bind-keys
   :map smartparens-mode-map
   ;; (foo| bar) -> foo bar
   ("M-s" . sp-splice-sexp)

   ;; (foo| bar) -> [foo bar]
   ("M-S" . sp-rewrap-sexp)


   ;; (|foo) bar -> (|foo bar)
   ("<s-right>" . sp-forward-slurp-sexp)

   ;; (|foo bar) -> (|foo) bar
   ("<s-left>" . sp-forward-barf-sexp)

   ;; foo(1, |[2, 3], 4) -> foo(1, |, 2)
   ("C-M-k" . sp-kill-sexp)
   ("s-k" . sp-kill-sexp))

  (show-smartparens-global-mode 1)
  (add-hook 'prog-mode-hook #'turn-on-smartparens-strict-mode)
  (add-hook 'markdown-mode-hook #'turn-on-smartparens-mode)

  (defmacro def-pairs (pairs)
    `(progn
       ,@(cl-loop for (key . val) in pairs
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

  ;; see https://github.com/Fuco1/smartparens/wiki/Permissions
  (sp-local-pair 'emacs-lisp-mode "'" nil :actions nil)           ;; no '' pair in emacs-lisp-mode
  (sp-local-pair 'emacs-lisp-mode "`" nil :actions nil)           ;; no `` pair in emacs-lisp-mode
  (sp-local-pair 'suggest-mode "'" nil :actions nil)
  (sp-local-pair 'suggest-mode "`" nil :actions nil)
  (sp-local-pair 'markdown-mode "`" nil :actions '(insert))       ;; only use ` for auto insertion in markdown-mode

  (defun my-create-newline-and-enter-sexp (&rest _ignored)
    "Open a new brace or bracket expression, with relevant newlines and indent. "
    (newline)
    (indent-according-to-mode)
    (forward-line -1)
    (indent-according-to-mode)))


(use-package suggest
  :ensure t)

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
  :ensure nil
  :load-path "~/go/src/github.com/stapelberg/expanderr/lisp")

;; TODO(gina) figure out what to use instead.  It is slow to load and
;; is no longer maintained.  Currently its package page recommends
;; looking at https://github.com/mdempsky/gocode
;;(use-package go-autocomplete
;;  :ensure t)


;; depends on https://github.com/cweill/gotests
(use-package gotests
  :ensure nil
  :load-path "lisp")

(use-package go-guru
  :ensure t
  :config
  (add-hook 'go-mode-hook #'go-guru-hl-identifier-mode))

(eval-and-compile
  (defun go-lint-load-path ()
    (concat (getenv "GOPATH")  "/src/github.com/golang/lint/misc/emacs")  ; octavia
    (concat (getenv "GOPATH")  "/src/golang.org/x/lint/misc/emacs")))     ; Gina-PC
(use-package golint
  :load-path (lambda() (list (go-lint-load-path))))

;; END GO CONFIGURATION

(use-package flycheck
  :ensure t
  :config
    (setq flycheck-global-modes '(not org-mode))
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

(use-package fullframe
  :after (magit)
  :ensure t
  :config
  (fullframe magit-status magit-mode-quit-window))

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
  :defer t
  :init
  (autoload 'anaconda-mode "anaconda-mode" nil t)
  :config
  (add-hook 'python-mode-hook 'anaconda-mode))

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

(use-package pdf-tools
  :magic ("%PDF" . pdf-view-mode)
  :bind
  (:map pdf-view-mode-map
        ("G" . pdf-view-last-page))
  :config
  ;; initialise
  (dolist
      (pkg
       '(pdf-annot pdf-cache pdf-dev pdf-history pdf-info pdf-isearch
                   pdf-links pdf-misc pdf-occur pdf-outline pdf-sync
                   pdf-util pdf-view pdf-virtual))
    (require pkg))
  (pdf-tools-install)
  ;; open pdfs scaled to fit page
  (setq-default pdf-view-display-size 'fit-page)
  ;; automatically annotate highlights
  (setq pdf-annot-activate-created-annotations t))

(use-package htmlize)

(use-package org
  :ensure t
  :init
  (defun my-find-org-agenda-files ()
    "Find all agenda files."
    (interactive)
    (require 'org)
    (dolist (f org-agenda-files)
      (find-file-noselect f)))
  :bind
  (("C-c !" . org-time-stamp-inactive)
   ("C-x l" . org-refile-goto-last-stored)
   ("C-c l" . org-store-link)
   ("C-c a" . org-agenda)
   ("C-c c" . org-capture)
   ("C-c b" . org-switchb))
  :config
  ;; see https://stackoverflow.com/questions/22720526/set-clock-table-duration-format-for-emacs-org-mode
  (setq org-duration-format (quote h:mm))
  (setq org-clock-mode-line-total 'current)

  ;; turn off validation goo.  https://stackoverflow.com/a/15145594
  (setq org-html-validation-link nil)

  (setq org-reverse-note-order t)

  (setq org-default-notes-file "c:/Users/gina/Documents/Gina/overall notes.org")

  ;; see http://doc.norang.ca/org-mode.html#FindTasksToClockIn
  (setq org-time-stamp-rounding-minutes (quote (1 1)))

  ;; see http://cachestocaches.com/2016/9/my-workflow-org-agenda/
  (setq org-columns-default-format "%50ITEM(Task) %10CLOCKSUM %16TIMESTAMP_IA")

  ;; see http://cachestocaches.com/2016/9/my-workflow-org-agenda/
  (setq org-capture-templates
        '(("t" "todo" entry (file org-default-notes-file)
           "* TODO %?\n%u\n%a\n" :clock-in t :clock-resume t)
          ("m" "Meeting" entry (file org-default-notes-file)
           "* MEETING with %? :MEETING:\n%t" :clock-in t :clock-resume t)
          ("i" "Idea" entry (file org-default-notes-file)
           "* %? :IDEA: \n%t" :clock-in t :clock-resume t)
          ("n" "Next Task" entry (file+headline org-default-notes-file "Tasks")
           "** NEXT %?" :prepend t :clock-in t :clock-resume t)
          ("c" "Schedule court deadline in current buffer" entry (file+olp+datetree buffer-file-name "Court deadlines")
           "** %? " :time-prompt t)))

  (setq org-agenda-span 700)
  (setq org-agenda-show-all-dates nil)
  (setq org-agenda-show-future-repeats (quote next))
  (setq org-startup-folded (quote showeverything))
  (setq org-latex-default-table-environment "longtable")
  (setq org-table-copy-increment nil)
  (setq org-return-follows-link t)

  (setq org-use-speed-commands
        (lambda () (and (looking-at org-outline-regexp) (looking-back "^\**"))))

  (setq org-file-apps
        '(("\\.docx\\'" . default)
          ("\\.mm\\'" . default)
          ("\\.x?html?\\'" . default)
          (auto-mode . emacs)))

  (setq org-refile-targets (quote ((nil :regexp . "Tasks")
                                   (org-agenda-files :regexp . "Tasks"))))


  (setq org-todo-keywords
        (quote ((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d)")
                (sequence "WAITING(w@/!)" "HOLD(h@/!)" "|" "CANCELLED(c@/!)" "PHONE" "MEETING"))))

  (setq org-todo-state-tags-triggers
        (quote (("CANCELLED" ("CANCELLED" . t))
                ("WAITING" ("WAITING" . t))
                ("HOLD" ("WAITING") ("HOLD" . t))
                (done ("WAITING") ("HOLD"))
                ("TODO" ("WAITING") ("CANCELLED") ("HOLD"))
                ("NEXT" ("WAITING") ("CANCELLED") ("HOLD"))
                ("DONE" ("WAITING") ("CANCELLED") ("HOLD")))))
  (setq org-use-fast-tag-selection t)

  (add-hook 'org-mode-hook 'flyspell-mode)
  (add-hook 'org-mode-hook (lambda () (require 'org-override))))

(use-package org-collector
  :load-path "lisp"
  :after (org)
  :ensure nil)

;; See http://emacs-fu.blogspot.com/2009/08/managing-e-mail-addresses-with-bbdb.html for further notes
(use-package bbdb
  :ensure t
  :config
  (setq bbdb-file "~/bbdb"))

(use-package helm-bbdb
  :ensure t
  :after (bbdb helm))

;; Not quite ready yet.  Doesn't seem to display all agenda files?
;; (use-package org-super-agenda
;;   :ensure t)
;; (let ((org-super-agenda-groups
;;        '((:auto-category t))))
;;   (org-super-agenda-mode 1)
;;   (org-agenda-list))


(global-set-key "\C-h\C-f" #'find-function)

(use-package org-noter
  :commands org-noter
  :config
  (add-hook 'org-noter-notes-mode-hook (lambda () (require 'org-noter-override))))


;; TODO(gina) Figure out how to combine these into a single package.
;; See https://www.gnu.org/software/emacs/manual/html_node/elisp/Packaging.html#Packaging
(use-package bates
  :load-path "lisp"
  :defer t
  :ensure nil)
(use-package ile-org-noter
  :load-path "lisp"
  :defer t
  :ensure nil)
(use-package ile-pdf
  :load-path "lisp"
  :defer t
  :ensure nil
  :bind
  (:map pdf-view-mode-map
        ("e" . ile-pdf-extract-pages)))
(use-package ile-discovery
  :load-path "lisp"
  :defer t
  :ensure nil)
(use-package ile-link
  :load-path "lisp"
  :defer t
  :ensure nil)
(use-package ile-navigation
  :load-path "lisp"
  :ensure nil
  :bind
  (:map pdf-view-mode-map
        ("b" . ile-jump-bates-number))
  :init
  ;; TODO(gina) move this into ILE after I figure out the right way to packageize it.
  (defvar ile-map nil
    "ILE keymap.")
  (define-prefix-command 'ile-map)
  (define-key ile-map (kbd "c") #'my-cleanup-region-date)
  (define-key ile-map (kbd "d") #'ile-duplicate)
  (define-key ile-map (kbd "i") #'ile-nav-indirect-buffer-for-id)
  (define-key ile-map (kbd "q") #'ile-org-fill-subtree)
  (define-key ile-map (kbd "t") #'ile-org-noter-dates)

  ;; This part would remain out here somewhere
  (global-unset-key (kbd "C-l"))
  (global-set-key (kbd "C-l") 'ile-map)
  (define-key ile-map (kbd "l") #'recenter-top-bottom)

  (global-set-key (kbd "C-x 8 s") (lambda () (interactive) (insert "§"))))

(use-package free-keys
  :ensure t)

(use-package helm
  :ensure t
  :diminish helm-mode
  :config

  (helm-mode 1)
  (bind-key "M-x" 'helm-M-x)
  (bind-key "C-x r b" 'helm-filtered-bookmarks)
  (bind-key "C-h a" 'helm-apropos)
  (bind-key "C-x C-f" 'helm-find-files)
  (bind-key "C-x r l" 'helm-bookmarks))

(use-package helm-google
  :ensure t
)

(use-package helm-projectile
  :ensure t
  :config
  (helm-projectile-on)
)

(global-set-key [f2] 'mode-line-other-buffer)

(global-set-key [f5] 'kmacro-end-and-call-macro)


; These don't really seem to do anything yet
; TODO(gina) continue fixing these up
;;(use-package calfw)
;;(use-package calfw-org)

(setenv "LANG" "en_US")
(setq ispell-really-hunspell t)
(setq ispell-program-name "hunspell")
(setq ispell-dictionary "en_US")

(delete-selection-mode t)
(setq column-number-mode t)  ;; put line number in mode line.


; from
; https://emacs.stackexchange.com/questions/16640/can-i-export-a-specific-table-in-an-org-file-to-csv-from-the-command-line
(defun my-tbl-export (name)
  "Search for table named `NAME` and export."
  (interactive "sName of table: ")
  (show-all)
  (let ((case-fold-search t))
    (if (search-forward (concat "#+TBLNAME: " name))
    (progn
      (next-line)
      (org-table-export (format "%s.tsv" name) "orgtbl-to-tsv")))))

(defun my-named-subtree-export (name)
  "Search for table named `NAME` and export the subtree."
  (interactive "sName of table: ")
  (show-all)
  (let ((case-fold-search t))
    (if (search-forward (concat "#+TBLNAME: " name))
    (progn
      (next-line)
      (my-subtree-export)))))

(defun my-subtree-export ()
  "Export the current subtree."
  (interactive)
  (org-html-export-to-html nil t))

;; see http://emacsredux.com/blog/2013/03/27/copy-filename-to-the-clipboard/
(defun my-copy-file-name-to-clipboard ()
  "Copy the current buffer file name to the clipboard."
  (interactive)
  (let ((filename (if (equal major-mode 'dired-mode)
                      default-directory
                    (buffer-file-name))))
    (when filename
      (kill-new filename)
      (message "Copied buffer file name '%s' to the clipboard." filename))))

(defun my-print-each-agenda (dir)
  "Create an html file for every agenda file in DIR."
  (interactive "Postscript file to Save to: ")
  (save-excursion
    (let ((cnt 0))
      ;; TODO(gina) see if there is some code I can lift to deal, with, e.g. directory names
      (dolist (f org-agenda-files)
        (setq cnt (+ 1 cnt))
        (switch-to-buffer (find-file-noselect f nil nil nil))
        (let ((htmlfile (concat dir (format "%02d-%s.html" cnt (buffer-name)))))
          (org-agenda nil "a" 'buffer)
          (switch-to-buffer "*Org Agenda*")
          (org-agenda-write htmlfile)))
      (message "Wrote %d agenda entries to %s" cnt dir))))

(defun my-print-each-agenda-faces (filename)
  "Print a single pdf file with a section for each agenda file, to FILENAME."
  (interactive "Postscript file to Save to: ")
  (save-excursion
    (let ((cnt 0))
      ;; TODO(gina) see if there is some code I can lift to deal, with, e.g. directory names
      (dolist (f org-agenda-files)
        (setq cnt (+ 1 cnt))
        (switch-to-buffer (find-file-noselect f nil nil nil))
        (let* ((name (buffer-name))
               (ps-left-header (list 'name)))
          (org-agenda nil "a" 'buffer)
          (switch-to-buffer "*Org Agenda*")
          (ps-spool-buffer)))
      (ps-despool filename)
      (message "Wrote %d agenda entries to %s" cnt filename))))

(defun my-cleanup-region-date ()
  "Convert text in region to an org mode date and replace it."
  (interactive)
  (unless (use-region-p)
    (user-error
     "Command my-cleanup-region-date only works with an active region"))
  (let* ((bounds (cons (region-beginning) (region-end)))
         (text
          (org-read-date
           nil
           nil
           (buffer-substring-no-properties (car bounds) (cdr bounds)))))
    (delete-region (car bounds) (cdr bounds))
    (insert text)

    (when (org-at-timestamp-p 'lax)
      (org-timestamp-change 0 'day))))

;; adapted from https://emacs.stackexchange.com/questions/2259/how-to-export-top-level-headings-of-org-mode-buffer-to-separate-files
(defun my-org-export-all-html (pub-dir)
  "Export all subtrees that are tagged with :export: to separate files.

PUB-DIR is the directory to publish to.

Subtrees that do not have the :EXPORT_FILE_NAME: property set
are exported to a filename derived from the headline text."
  (interactive "DPublish directory: ")
  (let ((fn 'org-html-export-to-html)
        (modifiedp (buffer-modified-p)))
    (save-excursion
      (org-map-entries
       (lambda ()
         (let* ((orig-export-file (org-entry-get (point) "EXPORT_FILE_NAME"))
                (new-export-file
                 (if orig-export-file
                     (concat pub-dir (file-name-nondirectory orig-export-file))
                   (concat
                    pub-dir
                    (file-name-nondirectory
                     (replace-regexp-in-string
                      " "
                      "_"
                      (nth 4 (org-heading-components))))))))
             (org-set-property "EXPORT_FILE_NAME" new-export-file)
             (funcall fn nil t)
             (if orig-export-file
                 (org-set-property "EXPORT_FILE_NAME" orig-export-file)
               (org-delete-property "EXPORT_FILE_NAME"))
           (set-buffer-modified-p modifiedp)))
       "+export" 'file))))

(use-package gina-launcher
  :load-path "lisp"
  :after (hydra)
  :ensure nil
  :chords
  (("jk" . gina-launcher-hydra/body)))

;; see https://www.emacswiki.org/emacs/UnfillParagraph
(defun unfill-paragraph (&optional region)
  "Take a multi-line paragraph and make it into a single line of text.
If REGION is set, we use that instead of trying to guess the paragraph."
  (interactive (progn (barf-if-buffer-read-only) '(t)))
  (let ((fill-column (point-max))
        ;; This would override `fill-column' if it's an integer.
        (emacs-lisp-docstring-fill-column t))
    (fill-paragraph nil region)))

;; adapted from https://www.emacswiki.org/emacs/UpdateAutoloads
(defconst my-lisp-dir (concat user-emacs-directory "lisp/"))
(defun my-update-autoloads ()
  "Call `update-autoloads-from-directories' on my local Lisp directory."
  (interactive)
  (require 'autoload)
  (let ((generated-autoload-file (concat user-emacs-directory "loaddefs.el")))
    (update-directory-autoloads my-lisp-dir)
    (byte-compile-file generated-autoload-file)))
(load (concat user-emacs-directory "loaddefs.el") t)

(unless noninteractive
  (server-start))


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
