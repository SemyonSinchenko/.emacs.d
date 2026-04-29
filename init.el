;;; init.el --- Main entry point for modular Emacs configuration -*- lexical-binding: t -*-

;;; Commentary:
;; This file bootstraps the configuration.  It sets up the load path,
;; optimizes garbage collection for startup, and loads modules from
;; the "modules/" directory.

;;; Code:

;; 1. Speed up startup (increase garbage collection threshold)
(setq gc-cons-threshold (* 150 1000 1000))

;; 2. Suppress startup screen and welcome messages
(setq inhibit-startup-screen t
      inhibit-startup-message t
      inhibit-startup-echo-area-message (user-login-name)
      initial-scratch-message nil)

;; 3. Определяем пути
(defvar my-modules-dir (expand-file-name "modules" user-emacs-directory))
(defvar my-lisp-dir (expand-file-name "lisp" user-emacs-directory))

;; Добавляем их в load-path
(add-to-list 'load-path my-modules-dir)
(add-to-list 'load-path my-lisp-dir)

;; 4. Файл для авто-генерируемых настроек (чтобы не пачкать init.el)
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))

;; 5. MODULE LOADING
;; Order matters: package manager first, then UI, then the rest
(require 'core-package)    ;; Most important: MELPA and use-package
(require 'core-ui)         ;; User interface / appearance
(require 'core-keys)       ;; Common keybindings
(require 'core-completion) ;; Autocompletion (Vertico/Corfu)

;; Tools
(require 'tools-dired)     ;; Git, Projectile, Terminal
(require 'tools-ai)        ;; Your AI config
(require 'tools-org)       ;; Org mode and notes

;; Languages and IDE features
(require 'lang-lsp)        ;; LSP (Eglot) and linters
(require 'lang-prog)       ;; Language-specific settings

;; 6. Возвращаем GC в норму
(setq gc-cons-threshold (* 2 1000 1000))

;; Buffer size
(setq read-process-output-max (* 4 1024 1024))

(message "Emacs init loaded successfully!")

(provide 'init)
;;; init.el ends here
