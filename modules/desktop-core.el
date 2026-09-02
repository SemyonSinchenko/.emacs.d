;;; desktop-core.el --- Core environment for the Emacs Desktop -*- lexical-binding: t; -*-

;;; Commentary:
;; Environment pieces shared by everything else: chrome, minibuffer,
;; clock/battery, recentf, binary checks, warning helper.

;;; Code:

(require 'desktop-config-defs)

;; Chrome.  In -q runs early-init is not loaded, so do it here.
(when (display-graphic-p)
  (unless my-desktop-show-menubar (menu-bar-mode -1))
  (tool-bar-mode -1)
  (scroll-bar-mode -1)
  (setq resize-mini-windows 'grow-only)
  (setq max-mini-window-height 0.15)
  (setq eldoc-echo-area-use-multiline-p t))
(unless my-desktop-show-menubar (menu-bar-mode -1))

;; Clock and battery.
(when my-desktop-display-time (display-time-mode 1))
(when my-desktop-display-battery
  (condition-case nil
      (progn (require 'battery)
             (when battery-status-function (display-battery-mode 1)))
    (error nil)))

;; recentf (feeds the Home tab "Recent files" widget).
(defun my-desktop--enable-recentf ()
  (setq recentf-max-saved-items 200
        recentf-exclude '("/tmp/" "/ssh:" "\\.git/" "elfeed/")
        recentf-auto-cleanup 60)
  (recentf-mode 1))
(my-desktop--enable-recentf)

;; Helpers used across modules.
(defun my-desktop--require-bin (name what)
  "Return the path of binary NAME or warn that WHAT needs it."
  (or (executable-find name)
      (progn (my-desktop--warn "%s needs binary `%s' (not found)"
                               what name)
             nil)))

(defun my-desktop--expand (path)
  "Expand PATH (~, env vars) or nil for nil."
  (when path (expand-file-name path)))

;; Tab-line off everywhere: the workspace sidebar replaces buffer tabs.
(when (fboundp 'global-tab-line-mode)
  (global-tab-line-mode -1))

;; Global clipboard.  Emacs' native selection support does not
;; always reach the GNOME clipboard on Wayland (build- and
;; compositor-dependent).  When wl-copy/xclip is available, every
;; copy (C-w/M-w) is pushed through it, so the desktop clipboard
;; always receives it.  Pasting stays native on GUI frames; terminal
;; frames paste through wl-paste/xclip as well.
(defun my-desktop--clipboard-backend ()
  "Return the clipboard bridge backend symbol for this session."
  (cond
   ((and (getenv "WAYLAND_DISPLAY")
         (executable-find "wl-copy")
         (executable-find "wl-paste"))
    'wayland)
   ((and (getenv "DISPLAY") (executable-find "xclip"))
    'x11)
   (t nil)))

(defconst my-desktop--clipboard-backend
  (my-desktop--clipboard-backend))

(defvar my-desktop--clipboard-copy-proc nil
  "Previous clipboard-bridge process; replaced on the next copy.")

(defun my-desktop--clipboard-copy (text)
  "Copy TEXT to the system clipboard."
  (when (process-live-p my-desktop--clipboard-copy-proc)
    (interrupt-process my-desktop--clipboard-copy-proc))
  (let ((proc nil)
        (process-connection-type nil))
    (pcase my-desktop--clipboard-backend
      ('wayland
       (setq proc (start-process "wl-copy" nil "wl-copy"
                                 "--type" "text/plain;charset=utf-8")))
      ('x11
       (setq proc (start-process "xclip" nil "xclip"
                                 "-selection" "clipboard" "-in"))))
    (when proc
      (setq my-desktop--clipboard-copy-proc proc)
      (process-send-string proc text)
      (process-send-eof proc))))

(defun my-desktop--clipboard-paste ()
  "Return text from the system clipboard, or nil."
  (pcase my-desktop--clipboard-backend
    ('wayland
     (condition-case nil
         (with-temp-buffer
           (call-process "wl-paste" nil t nil "--no-newline")
           (buffer-string))
       (error nil)))
    ('x11
     (condition-case nil
         (with-temp-buffer
           (call-process "xclip" nil t nil
                         "-selection" "clipboard" "-out")
           (buffer-string))
       (error nil)))))

(cond
 ;; Bridge available: copies always go through it, so the GNOME
 ;; clipboard receives them on Wayland/X11 regardless of Emacs build.
 (my-desktop--clipboard-backend
  (setq interprogram-cut-function #'my-desktop--clipboard-copy)
  (if (display-graphic-p)
      ;; GUI builds paste natively; keep that path untouched.
      (message "[desktop] clipboard: copies via %s, paste native"
               my-desktop--clipboard-backend)
    (setq interprogram-paste-function #'my-desktop--clipboard-paste)
    (message "[desktop] clipboard bridge: %s" my-desktop--clipboard-backend)))
 ;; No bridge: native handling only; warn in terminal, where it
 ;; cannot work at all.
 ((not (display-graphic-p))
  (message "[desktop] no clipboard bridge found (install wl-clipboard or xclip)")))

;; ------------------------------------------------------------------
;; Image clipboard: copy the image displayed at point (telega, lem,
;; bluesky, eww, ...) to the system clipboard via the bridge.
;; ------------------------------------------------------------------

(defconst my-clipboard--image-mimes
  '((png . "image/png") (jpeg . "image/jpeg") (jpg . "image/jpeg")
    (gif . "image/gif") (webp . "image/webp") (svg . "image/svg+xml")
    (xpm . "image/x-xpm") (bmp . "image/bmp") (tiff . "image/tiff"))
  "Map of Emacs image type symbols to MIME types.")

(defvar my-clipboard--image-proc nil
  "Previous image clipboard-bridge process.")

(defun my-clipboard--find-image-spec (spec)
  "Return the (image ...) spec inside SPEC, or nil."
  (cond ((and (consp spec) (eq (car spec) 'image)) spec)
        ((consp spec)
         (or (my-clipboard--find-image-spec (car spec))
             (my-clipboard--find-image-spec (cdr spec))))))

(defun my-clipboard--copy-image-data (mime data)
  "Push raw image DATA to the system clipboard as MIME."
  (when (process-live-p my-clipboard--image-proc)
    (interrupt-process my-clipboard--image-proc))
  (let ((proc nil)
        (process-connection-type nil))
    (pcase my-desktop--clipboard-backend
      ('wayland
       (setq proc (start-process "wl-copy" nil "wl-copy"
                                 "--type" mime)))
      ('x11
       (setq proc (start-process "xclip" nil "xclip"
                                 "-selection" "clipboard" "-t" mime "-i"))))
    (when proc
      (setq my-clipboard--image-proc proc)
      (process-send-string proc data)
      (process-send-eof proc)
      mime)))

(defun my-clipboard-copy-image ()
  "Copy the image displayed at point to the system clipboard.
Works wherever Emacs shows an image (telega, lem, bluesky, eww,
Dired thumbnails, ...).  Animated images are copied as-is."
  (interactive)
  (unless my-desktop--clipboard-backend
    (user-error "No clipboard bridge (install wl-clipboard or xclip)"))
  (let* ((spec (my-clipboard--find-image-spec
                (get-char-property (point) 'display))))
    (unless spec
      (user-error "No image at point"))
    (let* ((type (plist-get (cdr spec) :type))
           (file (plist-get (cdr spec) :file))
           (data (plist-get (cdr spec) :data))
           (raw (cond (file (with-temp-buffer
                              (insert-file-contents-literally file)
                              (buffer-string)))
                      (data data)
                      (t (user-error "Image spec has neither :file nor :data"))))
           (type (or type (image-type-from-data raw)))
           (mime (or (alist-get type my-clipboard--image-mimes)
                     (and type (format "image/%s" type)))))
      (unless mime
        (user-error "Cannot determine image type at point"))
      (let ((mime (my-clipboard--copy-image-data mime raw)))
        (message "Copied image to clipboard (%s, %d bytes)"
                 mime (length raw))))))

;; Which-key: C-c (and any prefix) pops up the available keys.
;; IMPORTANT: popup must be the minibuffer, not a side window --
;; the side-window popup corrupts the window tree when other side
;; windows exist (the workspace sidebar), producing
;; "Window ... has not same side ..." errors and stuck popups.
(use-package which-key
  :ensure t
  :config
  (setq which-key-popup-type 'minibuffer)
  (which-key-mode 1))

(provide 'desktop-core)
;;; desktop-core.el ends here
