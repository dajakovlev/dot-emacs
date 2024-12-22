;;;; init.el --- The Emacs Initialization File -*- lexical-binding: t -*-

;; straight.el
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

;; use-package
(straight-use-package 'use-package)

;; use-package will use straight.el to install packages
(use-package straight
  :custom
  (straight-use-package-by-default t))

;; emacs
(use-package emacs
  :config
  (global-display-line-numbers-mode)
  (setq column-number-mode t)
  (set-face-attribute 'default nil
		      :family "JetBrains Mono"
		      :height 110))

;; lab-themes
(use-package lab-themes
  :config
  (lab-themes-load-style 'light))

;; ws-butler
(use-package ws-butler
  :init
  (setq ws-butler-keep-whitespace-before-point nil)
  :config
  (ws-butler-global-mode))

;; racket-mode
(use-package racket-mode)
