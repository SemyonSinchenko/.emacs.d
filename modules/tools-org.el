;;; tools-org.el --- Org-mode and Note-taking configuration -*- lexical-binding: t; -*-

;;; Commentary:
;; Configures Org-mode, Org-Roam (Zettelkasten), and e-book reading.

;;; Code:

;; --- 1. Base Org Configuration ---

(use-package org
  :ensure t
  :hook
  ;; Включаем мягкий перенос строк (Soft Wrap) для всех org-файлов.
  ;; Критично для чтения дайджестов и заметок.
  (org-mode . visual-line-mode)
  :config
  ;; Визуальные отступы контента под заголовками (как в современных редакторах)
  ;; Убирает лишние звездочки и делает структуру чище.
  (setq org-startup-indented t)
  ;; Сворачивать все при открытии файла (overview), кроме текущего фокуса
  (setq org-startup-folded 'content))

;; --- 2. Org-Roam (Knowledge Base) ---

(use-package org-roam
  :ensure t
  :custom
  (org-roam-directory (file-truename "/var/home/sem/Org/org-roam/org-files/"))
  :bind
  ;; Стандартные клавиши для быстрого поиска и создания заметок
  (("C-c n f" . org-roam-node-find)
   ("C-c n i" . org-roam-node-insert))
  :config
  (org-roam-db-autosync-mode))

(provide 'tools-org)
;;; tools-org.el ends here
