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
(global-display-line-numbers-mode 1)
(setq auto-revert-verbose nil)

;; C-d: удалить выделение, иначе символ вперед.
;; По умолчанию C-d вызывает `delete-char' и игнорирует region.
;; `delete-forward-char' ведёт себя как <delete>: с активным region
;; удаляет выделение, иначе — символ в позиции курсора.
(global-set-key (kbd "C-d") #'delete-forward-char)

(require 'server)

;; Keep backup/auto-save minimal and local.
(setq make-backup-files nil
      auto-save-default nil
      create-lockfiles nil)

;; Package setup (minimal dependency surface).
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

;; Prefer tree-sitter Rust mode in minimal daemon.
(add-to-list 'auto-mode-alist '("\\.rs\\'" . rust-ts-mode))

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

;; Minibuffer/search flow similar to main config.
(use-package vertico
  :init
  (vertico-mode)
  :custom
  (vertico-cycle t)
  (read-buffer-completion-ignore-case t)
  (read-file-name-completion-ignore-case t)
  (completion-styles '(basic substring partial-completion flex))
  :bind (:map vertico-map
              ("C-n" . vertico-next)
              ("C-p" . vertico-previous)
              ("C-s" . vertico-next)
              ("C-r" . vertico-previous)))

(use-package vertico-sort
  :ensure nil
  :after vertico
  :custom
  (vertico-sort-function #'vertico-sort-history-length-alpha))

(use-package consult
  :bind (("C-s" . consult-line)
         ("C-x b" . consult-buffer)
         ("M-y" . consult-yank-pop)
         ("M-g g" . consult-goto-line)
         :map minibuffer-local-map
         ("M-s" . consult-history)
         ("M-r" . consult-history)))

;; Theme from main config.
(use-package catppuccin-theme
  :config
  ;; Flavors: latte, frappe, macchiato, mocha
  (setq catppuccin-flavor 'frappe)
  (load-theme 'catppuccin :no-confirm))

;; Optional key habit from main config.
(use-package ace-window
  :defer t
  :bind (("M-o" . ace-window)
         ("C-x o" . ace-window)))

;; Snippets for Markdown, reusing shared templates from ~/.emacs.d/snippets.
(use-package yasnippet
  :defer t
  :hook (markdown-mode . yas-minor-mode)
  :custom
  (yas-snippet-dirs (list (expand-file-name "snippets"
                                            user-emacs-directory))))

;; Time tracking (enabled globally by default).
(use-package wakatime-mode
  :config
  (global-wakatime-mode 1))

;; CSV highlighting
(use-package csv-mode
  :defer t
  :mode (("\\.csv\\'" . csv-mode)
         ("\\.tsv\\'" . tsv-mode))
  :bind (:map csv-mode-map
              ("C-c C-a" . csv-align-mode)
              ("C-c a"   . csv-align-fields)
              ("C-c u"   . csv-unalign-fields)
              ("C-c s"   . csv-guess-set-separator))
  :hook ((csv-mode . my-minimal-csv-setup)
         (tsv-mode . my-minimal-csv-setup))
  :custom
  (csv-align-max-width 40)
  :config
  (defvar my-minimal-csv-align-size-limit (* 10 1024 1024))

  (defun my-minimal-csv-buffer-size ()
    (- (point-max) (point-min)))

  (defun my-minimal-csv-setup ()
    (setq-local truncate-lines t)
    (ignore-errors
      (csv-guess-set-separator))
    (when (< (my-minimal-csv-buffer-size)
             my-minimal-csv-align-size-limit)
      (csv-align-mode 1))))

(use-package rainbow-csv
  :after csv-mode
  :commands (rainbow-csv-mode rainbow-csv-highlight)
  :bind (:map csv-mode-map
              ("C-c C-r" . rainbow-csv-mode))
  :hook ((csv-mode . my-minimal-rainbow-csv-maybe)
         (tsv-mode . my-minimal-rainbow-csv-maybe))
  :config
  (defvar my-minimal-rainbow-csv-size-limit (* 10 1024 1024))

  (defun my-minimal-rainbow-csv-maybe ()
    (when (< (my-minimal-csv-buffer-size)
             my-minimal-rainbow-csv-size-limit)
      (rainbow-csv-mode 1))))

;; Merge conflict resolution (git merge/rebase).
;; Built-in `smerge-mode'; its command prefix is C-c ^
;; (smerge-next, smerge-prev, smerge-keep-current, smerge-keep-mine, ...).
(use-package smerge-mode
  :ensure nil
  :defer t
  :custom
  (smerge-command-prefix (kbd "C-c ^"))
  :init
  (defun my-minimal-smerge-maybe-enable ()
    "Turn on `smerge-mode' when conflict markers are detected."
    (save-excursion
      (goto-char (point-min))
      (when (re-search-forward "^<<<<<<< " nil t)
        (smerge-mode 1))))
  (add-hook 'find-file-hook #'my-minimal-smerge-maybe-enable)
  (add-hook 'after-revert-hook #'my-minimal-smerge-maybe-enable))

;; Clipboard integration with GNOME.
;; XDG_RUNTIME_DIR is already present in the daemon; wl-clipboard needs only WAYLAND_DISPLAY on top of it.
(setenv "WAYLAND_DISPLAY" (or (getenv "WAYLAND_DISPLAY") "wayland-0"))
(setenv "DISPLAY" (or (getenv "DISPLAY") ":0"))

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
