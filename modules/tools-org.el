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
  (org-roam-directory (file-truename "~/Nextcloud/ORG/orgnote/"))
  :bind
  ;; Стандартные клавиши для быстрого поиска и создания заметок
  (("C-c n f" . org-roam-node-find)
   ("C-c n i" . org-roam-node-insert))
  :config
  (org-roam-db-autosync-mode))

;; Зависимости для UI (обычно подтягиваются сами, но можно оставить для явности)
(use-package websocket :ensure t :after org-roam)
(use-package emacsql :ensure t :after org-roam)

(use-package org-roam-ui
  :ensure t
  :after org-roam
  :config
  (setq org-roam-ui-sync-theme t
        org-roam-ui-follow t
        org-roam-ui-update-on-save t
        org-roam-ui-open-on-start t))

;; --- 3. E-Book Reader ---

(use-package reader
  :ensure t
  :vc (:url "https://codeberg.org/divyaranjan/emacs-reader" :make "all")
  :config
  ;; Можно добавить ассоциации файлов, если они не подхватились
  (add-to-list 'auto-mode-alist '("\\.epub\\'" . reader-mode)))

;; --- 4. Telega ---
(use-package telega
  :ensure t
  :config
  (setq telega-emoji-use-images nil))

(provide 'tools-org)
;;; tools-org.el ends here
