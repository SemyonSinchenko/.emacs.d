;;; core-ui.el --- UI Configuration settings -*- lexical-binding: t; -*-

;;; Commentary:
;; This module handles themes, fonts, and visual settings that happen
;; after the frame is created.
;; Note: Toolbars, scrollbars, and undecorated frames are handled in early-init.el.

;;; Code:

;; 1. [cite_start]Номера строк [cite: 2]
(global-display-line-numbers-mode)

;; 2. [cite_start]Тема оформления [cite: 5]
;; Используем use-package вместо ручной проверки package-installed-p
(use-package gruvbox-theme
  :ensure t
  :config
  ;; t в конце отключает запрос подтверждения "Do you trust this theme?"
  (load-theme 'gruvbox-dark-medium t))

(provide 'core-ui)
;;; core-ui.el ends here
