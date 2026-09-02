;;; desktop-torrent.el --- transmission.el UI for the Desktop -*- lexical-binding: t; -*-

;;; Commentary:
;; Frontend for transmission-daemon (systemd user service):
;;   systemctl --user enable --now transmission-daemon
;; RPC credentials belong to ~/.authinfo (machine "transmission").

;;; Code:

(require 'desktop-config-defs)
(require 'desktop-core)

(use-package transmission
  :ensure t
  :commands (transmission)
  :custom
  (transmission-host my-desktop-torrent-host)
  (transmission-service my-desktop-torrent-port))

(defun my-torrent-open ()
  "Open the transmission session."
  (interactive)
  (transmission))

(provide 'desktop-torrent)
;;; desktop-torrent.el ends here
