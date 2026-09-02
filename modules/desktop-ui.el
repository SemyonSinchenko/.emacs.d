;;; desktop-ui.el --- Theme and icons for the Emacs Desktop -*- lexical-binding: t; -*-

;;; Commentary:
;; Visual layer: Catppuccin theme, optional nerd-icons.

;;; Code:

(require 'desktop-config-defs)

(unless (eq my-desktop-theme-flavor 'none)
  (use-package catppuccin-theme
    :ensure t
    :config
    (setq catppuccin-flavor my-desktop-theme-flavor)
    (load-theme 'catppuccin :no-confirm)))

(when my-desktop-use-icons
  (use-package nerd-icons
    :ensure t
    :config
    (message "[desktop] nerd-icons enabled; if icons look wrong, \
install a Nerd Font system-wide")))

(provide 'desktop-ui)
;;; desktop-ui.el ends here
