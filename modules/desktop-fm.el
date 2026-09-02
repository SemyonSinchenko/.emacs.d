;;; desktop-fm.el --- Dired / dirvish file management -*- lexical-binding: t; -*-

;;; Commentary:
;; Dired as the file manager, optionally enhanced by dirvish.

;;; Code:

(require 'desktop-config-defs)
(require 'desktop-core)

(use-package dired
  :ensure nil
  :custom
  (dired-dwim-target t)
  (dired-kill-when-opening-new-dired-buffer t)
  (dired-recursive-copies 'always)
  (dired-recursive-deletes 'always))

(use-package dired-gitignore
  :ensure t
  :after dired
  :bind (:map dired-mode-map ("C-c i" . dired-gitignore-mode)))

(when my-desktop-enable-dirvish
  (use-package dirvish
    :ensure t
    :after dired
    :config
    (dirvish-override-dired-mode 1)))

(use-package dired-rsync
  :ensure t
  :after dired
  :bind (:map dired-mode-map ("C-c C-r" . dired-rsync)))

(provide 'desktop-fm)
;;; desktop-fm.el ends here
