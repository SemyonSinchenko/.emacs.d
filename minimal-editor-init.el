;;; minimal-editor-init.el --- Minimal daemon/TTY editor config -*- lexical-binding: t; -*-

;;; Commentary:
;; Standalone minimal Emacs config for daemon + terminal clients.
;; Goals:
;; - Fast startup.
;; - No GUI-focused modules, no LSP stack, no Org stack.
;; - Shared clipboard with GNOME (Wayland/X11).
;; - Search flow with C-s via consult-line.
;; - Fast exit with no prompts (discard unsaved changes).
;;
;; Usage examples:
;;   emacs --daemon --load ~/.emacs.d/minimal-editor-init.el
;;   emacsclient -t /path/to/file

;;; Code:

;; Startup performance.
(setq gc-cons-threshold (* 256 1024 1024)
      gc-cons-percentage 0.6
      package-enable-at-startup nil)

;; Quiet startup.
(setq inhibit-startup-screen t
      inhibit-startup-message t
      inhibit-startup-echo-area-message (user-login-name)
      initial-scratch-message nil
      ring-bell-function #'ignore)

;; No GUI chrome.
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

;; Core editing behavior.
(electric-pair-mode 1)
(global-font-lock-mode 1)
(global-auto-revert-mode 1)
(setq auto-revert-verbose nil)

(require 'server)

;; Keep backup/auto-save minimal and local.
(setq make-backup-files nil
      auto-save-default nil
      create-lockfiles nil)

;; Package setup (minimal dependency surface).
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

;; Minibuffer/search flow similar to main config.
(use-package vertico
  :init
  (vertico-mode 1)
  :custom
  (vertico-cycle t)
  :bind (:map vertico-map
              ("C-n" . vertico-next)
              ("C-p" . vertico-previous)
              ("C-s" . vertico-next)
              ("C-r" . vertico-previous)))

(use-package consult
  :bind (("C-s" . consult-line)
         ("C-x b" . consult-buffer)
         ("M-y" . consult-yank-pop)
         ("M-g g" . consult-goto-line)
         :map minibuffer-local-map
         ("M-s" . consult-history)
         ("M-r" . consult-history)))

;; Theme from main config.
(use-package gruvbox-theme
  :config
  (load-theme 'gruvbox-dark-medium t))

;; Optional key habit from main config.
(use-package ace-window
  :defer t
  :bind (("M-o" . ace-window)
         ("C-x o" . ace-window)))

;; Clipboard integration with GNOME.
(defun my-minimal--clipboard-backend ()
  "Return clipboard backend symbol for current environment."
  (cond
   ((and (getenv "WAYLAND_DISPLAY")
         (executable-find "wl-copy")
         (executable-find "wl-paste"))
    'wayland)
   ((and (getenv "DISPLAY") (executable-find "xclip"))
    'x11)
   (t nil)))

(defconst my-minimal--clipboard-backend
  (my-minimal--clipboard-backend)
  "Detected clipboard backend used by copy/paste integration.")

(defun my-minimal--clipboard-copy (text)
  "Copy TEXT to system clipboard."
  (pcase my-minimal--clipboard-backend
    ('wayland
     (let ((process-connection-type nil))
       (let ((proc (start-process "wl-copy" nil "wl-copy" "--type" "text/plain;charset=utf-8")))
         (process-send-string proc text)
         (process-send-eof proc))))
    ('x11
     (let ((process-connection-type nil))
       (let ((proc (start-process "xclip" nil "xclip" "-selection" "clipboard" "-in")))
         (process-send-string proc text)
         (process-send-eof proc))))))

(defun my-minimal--clipboard-paste ()
  "Return text from system clipboard, or nil if unavailable."
  (pcase my-minimal--clipboard-backend
    ('wayland
     (condition-case nil
         (with-temp-buffer
           (call-process "wl-paste" nil t nil "--no-newline")
           (buffer-string))
       (error nil)))
    ('x11
     (condition-case nil
         (with-temp-buffer
           (call-process "xclip" nil t nil "-selection" "clipboard" "-out")
           (buffer-string))
       (error nil)))
    (_ nil)))

(setq interprogram-cut-function #'my-minimal--clipboard-copy
      interprogram-paste-function #'my-minimal--clipboard-paste)

;; Fast, no-prompt exit.
(setq confirm-kill-emacs nil)
(setq kill-emacs-query-functions nil)
(setq confirm-nonexistent-file-or-buffer nil)

;; Always kill session immediately, even with unsaved buffers.
(defun my-minimal-fast-exit ()
  "Close client fast; kill Emacs only outside emacsclient."
  (interactive)
  (let ((confirm-kill-emacs nil)
        (kill-buffer-query-functions nil)
        (confirm-kill-processes nil)
        (save-some-buffers-action-alist nil)
        (confirm-nonexistent-file-or-buffer nil))
    (if (frame-parameter nil 'client)
        (server-save-buffers-kill-terminal nil)
      (kill-emacs 0))))

(global-set-key (kbd "C-x C-c") #'my-minimal-fast-exit)
(global-set-key (kbd "C-x #") #'my-minimal-fast-exit)

;; Restore sane GC after startup.
(add-hook
 'emacs-startup-hook
 (lambda ()
   (setq gc-cons-threshold (* 16 1024 1024)
         gc-cons-percentage 0.1)
   (garbage-collect)))

(provide 'minimal-editor-init)
;;; minimal-editor-init.el ends here
