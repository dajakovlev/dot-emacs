;;;; init.el --- The Emacs Initialization File -*- lexical-binding: t -*-

;;;;;;;;;;;;;;
;;;        ;;;
;;; Common ;;;
;;;        ;;;
;;;;;;;;;;;;;;

;;; Text editing settings
(defun load-settings/text-editing ()
  ;; Never use tabs for indentation
  (setq-default indent-tabs-mode nil)

  ;; Add a newline automatically at the end of the file
  (setq-default require-final-newline t)

  ;; Set font
  (set-face-attribute 'default nil
		      :family "JetBrains Mono"
		      :height 110))

;;; User interface settings
(defun load-settings/user-interface ()
  ;; Display line numbers
  (global-display-line-numbers-mode)
  
  ;; Display column numbers
  (setq column-number-mode t))

;;; Other settings
(defun load-settings/other ()
  ;; Don't show warnings
  (setq warning-minimum-level :error))

;;; GNU Emacs settings
(use-package emacs
    :init
    (load-settings/text-editing)
    (load-settings/user-interface)
    (load-settings/other)
    :hook
    (go-mode . eglot-ensure))

;;; straight.el - package manager
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name
        "straight/repos/straight.el/bootstrap.el"
        (or (bound-and-true-p straight-base-dir)
            user-emacs-directory)))
      (bootstrap-version 7))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;;; use-package will use straight.el to install packages
(use-package straight
    :custom
    (straight-use-package-by-default t))

;;; exec-path-from-shell - environment variables synchronizer
(use-package exec-path-from-shell
    :if (memq window-system '(mac ns x))
    :config
    (exec-path-from-shell-initialize))

;;; ws-butler - whitespaces trimmer
(use-package ws-butler
  :init
  (setq ws-butler-keep-whitespace-before-point nil)
  :config
  (ws-butler-global-mode))

;;; company-mode - text and code completion framework
(use-package company
    :hook (after-init . global-company-mode))

;;; lab-theme - color theme
(use-package lab-themes
  :config
  (lab-themes-load-style 'light))

;;; rainbow-delimiters - parentheses like a rainbow
(use-package rainbow-delimiters
  :hook
  (lisp-interaction-mode . rainbow-delimiters-mode)
  (emacs-lisp-mode . rainbow-delimiters-mode)
  (lisp-mode . rainbow-delimiters-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                       ;;;
;;; Programming languages ;;;
;;;                       ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; dape - a debug adapter client
(use-package dape)

;;;
;;; Common Lisp
;;;

;;; slime-company - completion backend for Slime
(use-package slime-company
  :after (slime company)
  :config
  (setq slime-company-completion 'fuzzy
        slime-company-after-completion 'slime-company-just-one-space))

;;; slime - SLIME (Superior Lisp Interaction Mode for Emacs)
(use-package slime
  :config
  (setq inferior-lisp-program "sbcl")
  (slime-setup '(slime-fancy slime-company)))

;;;
;;; Go
;;;

(defun golang/organize-imports ()
  (call-interactively #'eglot-code-action-organize-imports))

;;; go-mode - a mode for editing Go code
(use-package go-mode
    :mode "\\.go\\'"
    :hook
    (before-save . golang/organize-imports)
    (before-save . eglot-format-buffer)
    :config
    (setq-default eglot-workspace-configuration
                  '((:gopls . ((gofumpt . t))))))

;;; gotest - run Go tests and programs
(use-package gotest
  :after go-mode
  :bind
  (:map go-mode-map
        ("C-c t f" . go-test-current-file)
        ("C-c t t" . go-test-current-test)
        ("C-c t j" . go-test-current-project)
        ("C-c t b" . go-test-current-benchmark)
        ("C-c t c" . go-test-current-coverage)
        ("C-c t x" . go-run)))
