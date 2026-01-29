;;; tools-dired.el --- Git, Terminal, and Project management tools -*- lexical-binding: t; -*-

;;; Commentary:
;; This module contains configuration for file management, version control (Magit),
;; terminal emulation (Vterm), and project navigation (Projectile).

;;; Code:

;; 1. Environment Path
;; Ensures Emacs sees system binaries (node, cargo, pip) on Linux/Mac
(use-package exec-path-from-shell
  :ensure t
  :config
  (when (memq window-system '(mac ns x pgtk))
    (exec-path-from-shell-initialize)))

;; 2. Git Client
;; Refactored: Use use-package instead of manual check
(use-package magit
  :ensure t)

;; 3. Terminal Emulator
(use-package vterm
  :ensure t)

;; 4. Search Tools (Ripgrep wrapper)
(use-package rg
  :ensure t
  :custom
  (rg-enable-default-bindings))

;; 5. Project Management
(use-package projectile
  :ensure t
  :config
  (define-key projectile-mode-map (kbd "C-c C-p") 'projectile-command-map)
  
  ;; Интеграция с Consult (перенес внутрь конфига для порядка)
  (with-eval-after-load 'consult
    (define-key projectile-mode-map (kbd "C-c C-p s") #'consult-ripgrep))
  
  (setq projectile-project-search-path '(("~/github" . 1)))
  (projectile-mode +1))

;; 6. Time Tracking
(use-package wakatime-mode
  :ensure t
  :config
  (global-wakatime-mode))

(provide 'tools-dired)
;;; tools-dired.el ends here
