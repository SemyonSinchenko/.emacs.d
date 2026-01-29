;;; core-keys.el --- Global keybindings and input behavior -*- lexical-binding: t; -*-

;;; Commentary:
;; This module configures global keybindings, window management shortcuts (ace-window),
;; clipboard integration (xclip), and safety checks.

;;; Code:

;; 1. Which-key (Подсказки клавиш)
(use-package which-key
  :ensure t
  :init
  (which-key-mode))

;; 2. Безопасный выход
(defun my-ask-before-exit ()
  "Prompt the user for confirmation before killing Emacs."
  (y-or-n-p "Are you sure you want to exit?"))

(add-hook 'kill-emacs-query-functions #'my-ask-before-exit)

;; 3. Ace-window (Быстрое переключение окон)
(use-package ace-window
  :ensure t
  :bind ("M-o" . ace-window))

;; 4. XClip (Общий буфер обмена с Linux)
(use-package xclip
  :ensure t
  :config
  (xclip-mode 1))

(provide 'core-keys)
;;; core-keys.el ends here
