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

;; Hidden (dot) files: hidden by default, M-x `my-dired-toggle-hidden'
;; flips visibility in the current Dired buffer.
(require 'dired-x)                      ; provides `dired-omit-mode'
(setq dired-omit-files my-desktop-dired-omit-files)
(add-hook 'dired-mode-hook
          (lambda ()
            (when my-desktop-dired-hide-dotfiles
              (dired-omit-mode 1))))

(defun my-dired-toggle-hidden ()
  "Toggle visibility of hidden (dot) files in the current Dired buffer.
Uses `dired-omit-mode' with `my-desktop-dired-omit-files', so only
names matching that regexp (dotfiles by default) appear and disappear."
  (interactive)
  (unless (derived-mode-p 'dired-mode)
    (user-error "Not in a Dired buffer"))
  (dired-omit-mode (if dired-omit-mode -1 1))
  (message "Hidden files: %s"
           (if dired-omit-mode "hidden" "shown")))

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
