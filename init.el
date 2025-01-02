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
