;;; early-init.el --- Early initialization settings -*- lexical-binding: t; -*-

;;; Commentary:
;; This file is loaded before the package system and GUI are initialized.
;; It sets up frame parameters to prevent UI flickering during startup.

;;; Code:

;;; Compatibility: `set-local' is used by recent MELPA builds of
;;; vertico, corfu, magit, etc., but only exists in Emacs 31+.
;;; Provide it for Emacs 30 and earlier so the minibuffer doesn't
;;; break with "Symbol's function definition is void: set-local".
(unless (fboundp 'set-local)
  (defun set-local (symbol value)
    "Make SYMBOL buffer-local and set its value to VALUE.
This is equivalent to (set (make-local-variable SYMBOL) VALUE)."
    (set (make-local-variable symbol) value)))

;;; Disable TRAMP-GVFS
(setq tramp-archive-enabled nil)
(defalias 'tramp-archive-file-name-p #'ignore)

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
