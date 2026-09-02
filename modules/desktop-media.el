;;; desktop-media.el --- Media: listen.el (mpv) for music -*- lexical-binding: t; -*-

;;; Commentary:
;; listen.el drives mpv (or VLC) for music playback: `listen' opens
;; its transient menu (play, pause, seek, volume, queue), tracks are
;; browsed with `listen-library'.  Requires the `mpv' binary.

;;; Code:

(require 'desktop-config-defs)
(require 'desktop-core)

(my-desktop--require-bin "mpv" "media module (listen)")

(use-package listen
  :ensure t
  :commands (listen listen-play listen-pause listen-library)
  :custom
  (listen-directory (or (my-desktop--expand my-desktop-music-dir)
                        "~/Music")))

(defun my-media-play-file (file)
  "Play FILE with Listen."
  (interactive "FMedia file: ")
  (listen-play (listen-current-player) file))

(defun my-media-music-library ()
  "Open the music library at `my-desktop-music-dir'."
  (interactive)
  (let ((dir (my-desktop--expand my-desktop-music-dir)))
    (if (and dir (file-directory-p dir))
        (let ((default-directory dir))
          (call-interactively #'listen-library))
      (call-interactively #'listen-library))))

(defun my-media-toggle ()
  "Pause/resume playback."
  (interactive)
  (listen-pause (listen-current-player)))

(provide 'desktop-media)
;;; desktop-media.el ends here
