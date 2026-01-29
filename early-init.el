;;; early-init.el --- Early initialization settings -*- lexical-binding: t; -*-

;;; Commentary:
;; This file is loaded before the package system and GUI are initialized.
;; It sets up frame parameters to prevent UI flickering during startup.

;;; Code:

;; 1. Отключаем UI элементы в параметрах фрейма (до их отрисовки)
;; Это предотвращает "мигание" интерфейса при старте
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars . nil) default-frame-alist)

;; 2. Отключаем сами глобальные режимы (на всякий случай)
(setq menu-bar-mode nil
      tool-bar-mode nil
      scroll-bar-mode nil)

;; 3. Убираем декорации окна (Borderless режим)
;; Borderless config moved here for performance
(add-to-list 'default-frame-alist '(undecorated . t))
(add-to-list 'default-frame-alist '(fullscreen . maximized))

(provide 'early-init)
;;; early-init.el ends here
