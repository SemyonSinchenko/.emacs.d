;;; desktop-init.el --- Emacs Desktop: machinery entry point -*- lexical-binding: t; -*-

;;; Commentary:
;; Machinery entry point of the "Emacs Desktop" configuration.
;; This file contains NO user settings -- everything tunable lives in
;; desktop-config.el (the user file), whose schema is defined in
;; modules/desktop-config-defs.el.
;;
;; Usage:
;;   Run it (the only supported way):
;;     emacs -q -l ~/.emacs.d/desktop-init.el
;;   Do NOT run this config as a daemon: a leftover "desktop"
;;   server socket from an older instance breaks server-start on
;;   the next launch and splits the startup frame with warnings.
;;   Batch syntax / load check:
;;     emacs --batch -l ~/.emacs.d/desktop-init.el
;;
;; Startup order:
;;   1. hygiene (gc, quiet)         5. load desktop-config.el (+ -local)
;;   2. environment                 6. package bootstrap
;;   3. load-path                   7. modules (each failure isolated)
;;   4. settings schema             8. first frame, summary

;;; Code:

;; 1. Hygiene ---------------------------------------------------------

(defvar my-desktop--boot-time (current-time))
(defvar my-desktop--warnings nil)
(defvar my-desktop--first-frame-done nil)

(setq gc-cons-threshold (* 256 1024 1024)
      gc-cons-percentage 0.6
      package-enable-at-startup nil)

;; Silence JIT native-compilation warnings: after installing or
;; updating packages they flood *Warnings* and can freeze redisplay.
(setq native-comp-async-report-warnings-errors nil)

(setq inhibit-startup-screen t
      inhibit-startup-message t
      inhibit-startup-echo-area-message (user-login-name)
      initial-scratch-message nil
      ring-bell-function #'ignore)

;; 2. Environment (child processes like mpv/rclone need these) --------

(setenv "WAYLAND_DISPLAY" (or (getenv "WAYLAND_DISPLAY") "wayland-0"))
(setenv "DISPLAY" (or (getenv "DISPLAY") ":0"))

;; 3. Load path -------------------------------------------------------

(defvar my-desktop--dir
  (file-name-directory (or load-file-name user-emacs-directory))
  "Directory holding this file (the desktop config root).")

(add-to-list 'load-path (expand-file-name "modules" my-desktop--dir))
(make-directory user-emacs-directory t)

(defun my-desktop--warn (format &rest args)
  "Record and display a non-fatal warning with FORMAT and ARGS."
  (let ((msg (apply #'format (concat "[desktop] " format) args)))
    (push msg my-desktop--warnings)
    (message "%s" msg)))

;; 4. Settings schema, then user settings ------------------------------

(require 'desktop-config-defs)

(defun my-desktop--find (names)
  "Return the first existing file among NAMES, else nil."
  (seq-find #'file-exists-p names))

(let ((cfg (my-desktop--find
            (list (expand-file-name "desktop-config.el" user-emacs-directory)
                  (expand-file-name "desktop-config.el" my-desktop--dir)))))
  (if cfg
      (condition-case err
          (load cfg nil t)          ; never byte-compile the user file
        (error (my-desktop--warn "config load failed: %s" err)))
    (my-desktop--warn "desktop-config.el not found, using defaults")))

(let ((local (my-desktop--find
              (list (expand-file-name "desktop-config-local.el"
                                      user-emacs-directory)
                    (expand-file-name "desktop-config-local.el"
                                      my-desktop--dir)))))
  (when local
    (condition-case err
        (load local nil t)
      (error (my-desktop--warn "local config failed: %s" err)))))

;; 5. Frame recipe (before any frame exists in daemon mode) ------------

(defun my-desktop--frame-params ()
  "Build `default-frame-alist' from user settings."
  (let ((params))
    (when my-desktop-frame-undecorated
      (push '(undecorated . t) params))
    (when my-desktop-frame-maximized
      (push '(fullscreen . maximized) params))
    (when (and my-desktop-frame-opacity (> my-desktop-frame-opacity 0)
               (< my-desktop-frame-opacity 100))
      (push `(alpha-background . ,my-desktop-frame-opacity) params))
    (when my-desktop-font
      (push `(font . ,my-desktop-font) params))
    params))

(setq default-frame-alist (my-desktop--frame-params))

(defun my-desktop--apply-frame-now ()
  "Apply the frame recipe to the current frame (interactive runs)."
  (when (display-graphic-p)
    (when my-desktop-frame-undecorated
      (condition-case nil
          (set-frame-parameter nil 'undecorated t)
        (error nil)))
    (when my-desktop-frame-maximized
      (condition-case nil
          (set-frame-parameter nil 'fullscreen 'maximized)
        (error nil)))
    (when (and my-desktop-frame-opacity (> my-desktop-frame-opacity 0)
               (< my-desktop-frame-opacity 100))
      (set-frame-parameter nil 'alpha-background my-desktop-frame-opacity))
    (when my-desktop-font
      (condition-case nil
          (set-frame-font my-desktop-font nil t)
        (error nil)))))

;; 6. Package bootstrap -------------------------------------------------

(require 'package)
(add-to-list 'package-archives
             '("nongnu" . "https://elpa.nongnu.org/nongnu/") t)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)
(setq package-archive-priorities
      '(("gnu" . 10) ("nongnu" . 10) ("melpa" . 5)))
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)
(setq use-package-always-ensure t)

;; Optional quelpa bootstrap (only needed for git-only packages).
(when my-desktop-enable-quelpa
  (unless (package-installed-p 'quelpa)
    (condition-case err
        (progn
          (with-temp-buffer
            (url-insert-file-contents
             "https://raw.githubusercontent.com/quelpa/quelpa/master/quelpa.el"))
          (eval-buffer)
          (quelpa-self-upgrade))
      (error (my-desktop--warn "quelpa bootstrap failed: %s" err))))
  (when (package-installed-p 'quelpa)
    (unless (package-installed-p 'quelpa-use-package)
      (package-install 'quelpa-use-package))
    (require 'quelpa-use-package)))

;; Shared custom file (as in the other configs).
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (condition-case err
      (load custom-file)
    (error (my-desktop--warn "custom.el failed: %s" err))))

;; 7. Modules (each failure is isolated) ---------------------------------

(defun my-desktop--load-module (feature &optional flag-var)
  "Require FEATURE unless FLAG-VAR is bound and nil."
  (if (and flag-var (boundp flag-var) (not (symbol-value flag-var)))
      (message "[desktop] module %s disabled by flag" feature)
    (condition-case err
        (require feature)
      (error (my-desktop--warn "module %s failed: %s"
                               feature (error-message-string err))))))

(dolist (m '(desktop-core
             desktop-ui
             desktop-completion
             desktop-workspaces
             desktop-home
             (desktop-term . my-desktop-enable-ghostel)
             desktop-fm
             desktop-org
             (desktop-reader . my-desktop-enable-reader)
             (desktop-media . my-desktop-enable-media)
             (desktop-rss . my-desktop-enable-rss)
             desktop-social
             desktop-sync
             (desktop-torrent . my-desktop-enable-torrent)
             (desktop-comms . my-desktop-enable-telega)
             (desktop-ai . my-desktop-enable-ai)
             (desktop-roguelike . my-desktop-enable-roguelike)
             desktop-edit
             (desktop-lexicon . my-desktop-enable-lexicon)
             desktop-keys))
  (if (consp m)
      (my-desktop--load-module (car m) (cdr m))
    (my-desktop--load-module m)))

;; 8. First frame, gc, summary -------------------------------------------

(defun my-desktop--run-first-frame (frame)
  "Run the first-frame setup, in a fixed order, on FRAME.
Every session starts fresh with the workspaces listed in
`my-desktop-workspaces' (no session persistence yet)."
  (unless my-desktop--first-frame-done
    (setq my-desktop--first-frame-done t)
    (with-selected-frame frame
      ;; 1. Workspaces (perspective.el), fresh from the config.
      (when (fboundp 'my-ws-initialize)
        (condition-case err
            (my-ws-initialize)
          (error (my-desktop--warn "workspace init failed: %s"
                                   (error-message-string err)))))
      ;; 2. Home page.
      (when (fboundp 'my-home-open)
        (condition-case err
            (my-home-open)
          (error (my-desktop--warn "home open failed: %s"
                                   (error-message-string err)))))
      ;; 3. Deterministic first layout: Home alone in one window.
      ;;    Anything that split the frame during startup (scratch
      ;;    windows, popups, package noise) is discarded here.
      (delete-other-windows)
      ;; 4. Sidebar (treemacs, one tree per workspace), when wanted.
      (when (and (boundp 'my-desktop-sidebar-autoshow)
                 my-desktop-sidebar-autoshow
                 (fboundp 'my-sidebar-show))
        (condition-case err
            (my-sidebar-show)
          (error (my-desktop--warn "sidebar show failed: %s"
                                   (error-message-string err)))))
      ;; 5. Extra hooks from any module.
      (dolist (hook (default-value 'my-desktop-first-frame-hook))
        (condition-case err
            (funcall hook)
          (error (my-desktop--warn "first-frame hook %s failed: %s"
                                   hook
                                   (error-message-string err))))))))

(if (daemonp)
    (add-hook 'after-make-frame-functions
              (lambda (frame)
                (when (and (frame-live-p frame)
                           (frame-parameter frame 'client))
                  (my-desktop--run-first-frame frame))))
  (unless noninteractive
    (my-desktop--apply-frame-now)
    (run-with-idle-timer
     0.3 nil
     (lambda () (my-desktop--run-first-frame (selected-frame))))))

;; No Emacs server: the desktop runs as a plain interactive session
;; (the daemon experiment was abandoned -- a stale "desktop" server
;; socket from it caused startup warnings and frame splits).
(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold (* 16 1024 1024)
                  gc-cons-percentage 0.1)
            (garbage-collect)))

(let ((elapsed (float-time (time-subtract (current-time)
                                          my-desktop--boot-time))))
  (message "Emacs Desktop ready in %.2fs (%d warnings)"
           elapsed (length my-desktop--warnings))
  (dolist (w (nreverse my-desktop--warnings))
    (message "%s" w)))

(provide 'desktop-init)
;;; desktop-init.el ends here
