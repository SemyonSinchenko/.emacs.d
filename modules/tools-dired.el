;;; tools-dired.el --- Git, Terminal, and Project management tools -*- lexical-binding: t; -*-

;;; Commentary:
;; This module contains configuration for Dired, Git (Magit),
;; Terminal (Vterm), and Projectile.

;;; Code:

;; 1. Environment Path
(use-package exec-path-from-shell
  :ensure t
  :config
  (when (memq window-system '(mac ns x pgtk))
    (exec-path-from-shell-initialize)))

;; 2. Dired (Файловый менеджер)
(use-package dired
  :ensure nil ;; Встроен в Emacs
  :custom
  (dired-dwim-target t)             ;; Умное копирование в соседнее окно
  (dired-kill-when-opening-new-dired-buffer t) ;; Не плодить буферы при навигации
  (dired-recursive-copies 'always)  ;; Копировать папки рекурсивно без лишних вопросов
  (dired-recursive-deletes 'always)) ;; Удалять папки рекурсивно

;; Пакет для скрытия gitignored файлов
(use-package dired-gitignore
  :ensure t
  :bind (:map dired-mode-map
         ;; Включаем/выключаем скрытие по нажатию C-c i
         ("C-c i" . dired-gitignore-mode))
  :config
  ;; Опционально: включить глобально во всех dired-буферах
  (dired-gitignore-global-mode 1))

;; 3. Git Client (Magit)
(use-package magit
  :ensure t)

;; 4. Terminal Emulator
(use-package vterm
  :ensure t)

;; 5. Search Tools (Ripgrep wrapper)
(use-package rg
  :ensure t
  :custom
  (dired-bind-jump nil) ;; Отключаем конфликт биндингов, если есть
  (rg-enable-default-bindings))

;; 6. Project Management
(use-package projectile
  :ensure t
  :config
  (define-key projectile-mode-map (kbd "C-c C-p") 'projectile-command-map)
  
  (with-eval-after-load 'consult
    (define-key projectile-mode-map (kbd "C-c C-p s") #'consult-ripgrep))
  
  (setq projectile-project-search-path '(("~/github" . 1)))
  (projectile-mode +1))

;; 7. Time Tracking
(use-package wakatime-mode
  :ensure t
  :config
  (global-wakatime-mode))

(use-package visual-regexp-steroids
  :ensure t
  :custom
  ;; Используем Python для обработки регулярок (нужен python в системе)
  (vr/engine 'python)
  :bind
  ;; C-c r - замена с визуальным фидбеком
  ("C-c r" . vr/replace)
  ;; C-c q - query replace (спрашивает подтверждение для каждого)
  ("C-c q" . vr/query-replace))

(provide 'tools-dired)
;;; tools-dired.el ends here
