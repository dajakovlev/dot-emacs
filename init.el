;;;; init.el --- The Emacs Initialization File -*- lexical-binding: t -*-


;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                    ;;;
;;; Package management ;;;
;;;                    ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;

;; straight.el - package management
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

;; use-package - package configuration
(straight-use-package 'use-package)

;; use-package will use straight.el to install packages
(use-package straight
  :custom
  (straight-use-package-by-default t))


;;;;;;;;;;;;;;;;;;;;
;;;              ;;;
;;; Text editing ;;;
;;;              ;;;
;;;;;;;;;;;;;;;;;;;;

;; Never use tabs for indentation
(setq-default indent-tabs-mode nil)

;; Add a newline automatically at the end of the file
(setq-default require-final-newline t)

;; Font
(set-face-attribute 'default nil
		    :family "JetBrains Mono"
		    :height 110)

;; ws-butler - trimming whitespaces
(use-package ws-butler
  :init
  (setq ws-butler-keep-whitespace-before-point nil)
  :config
  (ws-butler-global-mode))

;; auto-complete - auto completion
(use-package auto-complete
  :config
  (setq ac-auto-show-menu t)
  (setq ac-use-comphist t)
  (setq ac-quick-help-delay 1)
  (setq ac-quick-help-prefer-pos-tip nil)
  (define-globalized-minor-mode real-global-auto-complete-mode
    auto-complete-mode (lambda ()
                         (if (not (minibufferp (current-buffer)))
                             (auto-complete-mode 1))))
  (real-global-auto-complete-mode t))


;;;;;;;;;;;;;;;;;;;;;;
;;;                ;;;
;;; User Interface ;;;
;;;                ;;;
;;;;;;;;;;;;;;;;;;;;;;

;; Display line numbers
(global-display-line-numbers-mode)

;; Display column numbers
(setq column-number-mode t)

;; lab-themes - a color theme
(use-package lab-themes
  :config
  (lab-themes-load-style 'light))

;; rainbow-delimiters - colorize parentheses
(use-package rainbow-delimiters
  :hook
  (lisp-interaction-mode . rainbow-delimiters-mode)
  (emacs-lisp-mode . rainbow-delimiters-mode)
  (lisp-mode . rainbow-delimiters-mode))


;;;;;;;;;;;;;;;;;;;
;;;             ;;;
;;; Common Lisp ;;;
;;;             ;;;
;;;;;;;;;;;;;;;;;;;

;;; slime - SLIME (Superior Lisp Interaction Mode for Emacs)
(use-package slime
  :config
  (setq inferior-lisp-program "sbcl")
  (setq slime-load-failed-fasl 'always)
  (setq slime-net-coding-system 'utf-8-unix)
  (setq lisp-indent-function 'common-lisp-indent-function)
  (setq slime-description-autofocus t))

;;; ac-slime - auto completion for SLIME
(use-package ac-slime
  :config
  (add-to-list 'ac-modes 'slime-repl-mode)
  :hook
  (slime-mode . set-up-slime-ac)
  (slime-repl-mode . set-up-slime-ac))
