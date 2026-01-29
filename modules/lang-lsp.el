;;; lang-lsp.el --- LSP (Eglot), Linting and Formatting setup -*- lexical-binding: t; -*-

;;; Commentary:
;; Configures Eglot (LSP client), Tree-sitter for highlighting,
;; Flycheck for linting, and Apheleia for formatting.

;;; Code:

;; 0. Компиляторные подсказки (чтобы Flycheck не ругался на неизвестные функции)
;; Сообщаем, что эта функция живет в core-completion.el
(declare-function my/eglot-capf "core-completion")
;; Сообщаем, что эти функции живут в eglot.el
(declare-function eglot-code-actions "eglot")
(declare-function eglot-format-buffer "eglot")
(declare-function eglot-rename "eglot")
;; Сообщаем, что эта функция живет в flycheck.el
(declare-function flycheck-add-next-checker "flycheck")


;; 1. Tree-sitter (Встроенный в Emacs 29+)
;; Мы используем treesit-auto, чтобы он сам скачал грамматики
;; и настроил маппинги (python-mode -> python-ts-mode)

(use-package treesit-auto
  :ensure t
  :custom
  (treesit-auto-install 'prompt) ;; Предложит установить грамматику, если её нет
  :config
  (treesit-auto-add-to-auto-mode-alist 'all) ;; Автоматически использовать ts-режимы для всех языков
  (global-treesit-auto-mode))

;; 2. Eglot (LSP Client)
(use-package eglot
  :ensure t
  :custom
  (eglot-ignored-server-capabilities '(:semanticTokensProvider :inlayHintProvider :documentOnTypeFormattingProvider))
  (eglot-stay-out-of '(indentation))
  (eglot-connect-timeout 120)
  :config
  ;; Хук для объединения с Yasnippet (функция из core-completion)
  (add-hook 'eglot-managed-mode-hook #'my/eglot-capf))

;; Глобальная карта клавиш для LSP действий
;; Используем defvar-keymap (Emacs 29+) или обычный defvar
(defvar my-eglot-map
  (let ((map (make-sparse-keymap)))
    (keymap-set map "a" #'eglot-code-actions)
    (keymap-set map "f" #'eglot-format-buffer)
    (keymap-set map "r" #'eglot-rename)
    map)
  "My key customizations for LSP.")

(keymap-global-set "C-x M-." my-eglot-map)


;; 3. Flycheck (Линтинг)
(use-package flycheck
  :ensure t
  :init
  (global-flycheck-mode 1))

(use-package flycheck-eglot
  :ensure t
  :after (flycheck eglot)
  :custom
  (flycheck-eglot-exclusive nil)
  :config
  (global-flycheck-eglot-mode 1)
  ;; Настраиваем цепочки чекеров
  (flycheck-add-next-checker 'python-ruff 'eglot-check)
  (flycheck-add-next-checker 'python-flake8 'eglot-check))


;; 4. Apheleia (Форматирование)
(use-package apheleia
  :ensure t
  :config
  (setf (alist-get 'python-mode apheleia-mode-alist) '(ruff)))

(provide 'lang-lsp)
;;; lang-lsp.el ends here
