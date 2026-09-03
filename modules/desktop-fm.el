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

;; ------------------------------------------------------------------
;; Video files: open with `my-desktop-video-player', not as raw bytes
;; ------------------------------------------------------------------

(declare-function dired-get-file-for-visit "dired" (&optional error-if-not-file-p))
(declare-function dired-find-file "dired" nil)
;; Defined in dired-x (required above); repeated here for the byte compiler.
(defvar dired-guess-shell-alist-user nil)

(defun my-dired--video-regexp ()
  "Regexp matching file names opened by `my-desktop-video-player'."
  (concat "\\." (regexp-opt my-desktop-video-extensions) "\\'"))

(defun my-dired-open-video (file)
  "Play FILE with `my-desktop-video-player'."
  (let* ((cmd (append (split-string my-desktop-video-player nil t)
                      (list (expand-file-name file))))
         (buf (get-buffer-create " *my-video-player*")))
    (with-current-buffer buf
      (erase-buffer))
    (let ((proc (apply #'start-process "my-video-player" buf cmd)))
      ;; Do not ask about the running player when quitting Emacs
      (set-process-query-on-exit-flag proc nil)
      (message "Playing %s with %s"
               (file-name-nondirectory file) my-desktop-video-player))))

(defun my-dired-find-file ()
  "Open the file at point, like `dired-find-file'.
Video files (see `my-desktop-video-extensions') go to
`my-desktop-video-player' instead of a raw-bytes buffer."
  (interactive)
  (let ((file (dired-get-file-for-visit)))
    (if (and my-desktop-video-player
             (not (file-remote-p file))
             (string-match-p (my-dired--video-regexp) file))
        (my-dired-open-video file)
      (dired-find-file))))

;; `!' (dired-do-shell-command) proposes the player for video files
(when my-desktop-video-player
  (add-to-list 'dired-guess-shell-alist-user
               (list (my-dired--video-regexp) my-desktop-video-player)))

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
