;;; core-ui.el --- UI Configuration settings -*- lexical-binding: t; -*-

;;; Commentary:
;; This module handles themes, fonts, and visual settings that happen
;; after the frame is created.
;; Note: Toolbars, scrollbars, and undecorated frames are handled in early-init.el.

;;; Code:

;; 1. [cite_start]Номера строк [cite: 2]
(global-display-line-numbers-mode)

;; TUI / GUI
(if (display-graphic-p)
    ;; Настройки для GUI (графический интерфейс)
    (progn
      ;; Разрешаем минибуферу динамически менять размер ('grow-only или t)
      (setq resize-mini-windows 'grow-only)

      ;; 0.25 означает до 25% от высоты фрейма.
      (setq max-mini-window-height 0.15)

      ;; Разрешаем eldoc использовать несколько строк
      (setq eldoc-echo-area-use-multiline-p t))

  ;; Настройки для TUI (-nw, терминал)
  (progn
    ;; Запрещаем автоматическое изменение высоты (фиксируем в 1 строку)
    (setq resize-mini-windows nil)

    ;; Заставляем eldoc обрезать подсказки, чтобы они помещались в 1 строку
    (setq eldoc-echo-area-use-multiline-p nil)))

;; 2. [cite_start]Тема оформления [cite: 5]
;; Используем use-package вместо ручной проверки package-installed-p
(use-package catppuccin-theme
  :ensure t
  :config
  ;; Flavors: latte, frappe, macchiato, mocha
  (setq catppuccin-flavor 'frappe)
  ;; :no-confirm отключает запрос "Do you trust this theme?"
  (load-theme 'catppuccin :no-confirm))

(provide 'core-ui)
;;; core-ui.el ends here
