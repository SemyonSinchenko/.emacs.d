;;; init.el --- Main entry point for modular Emacs configuration -*- lexical-binding: t -*-

;;; Commentary:
;; This file bootstraps the configuration.  It sets up the load path,
;; optimizes garbage collection for startup, and loads modules from
;; the "modules/" directory.

;;; Code:

;; 1. Ускоряем старт (увеличиваем порог сборщика мусора)
(setq gc-cons-threshold (* 150 1000 1000))

;; 2. Определяем пути
(defvar my-modules-dir (expand-file-name "modules" user-emacs-directory))
(defvar my-lisp-dir (expand-file-name "lisp" user-emacs-directory))

;; Добавляем их в load-path
(add-to-list 'load-path my-modules-dir)
(add-to-list 'load-path my-lisp-dir)

;; 3. Файл для авто-генерируемых настроек (чтобы не пачкать init.el)
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))

;; 4. ЗАГРУЗКА МОДУЛЕЙ
;; Порядок важен: сначала пакетный менеджер, потом UI, потом остальное
(require 'core-package)    ;; Самый важный: MELPA и use-package
(require 'core-ui)         ;; Внешний вид
(require 'core-keys)       ;; Общие клавиши
(require 'core-completion) ;; Автодополнение (Vertico/Corfu)

;; Инструменты
(require 'tools-dired)     ;; Git, Projectile, Terminal
(require 'tools-ai)        ;; Твой AI конфиг
(require 'tools-org)       ;; Org mode и заметки

;; Языки и IDE функции
(require 'lang-lsp)        ;; LSP (Eglot) и линтеры
(require 'lang-prog)       ;; Настройки конкретных языков

;; ИИ-ассистент
(require 'tools-rss)

;; 5. Возвращаем GC в норму
(setq gc-cons-threshold (* 2 1000 1000))

(message "Emacs init loaded successfully!")

(provide 'init)
;;; init.el ends here
