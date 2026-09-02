;;; desktop-home.el --- Home tab: dashboard for the Desktop -*- lexical-binding: t; -*-

;;; Commentary:
;; dashboard-based start page: banner image, action buttons, folder
;; shortcuts (Dired), recent files, bookmarks, projects, agenda.
;; Every new empty client frame opens here.

;;; Code:

(require 'desktop-config-defs)
(require 'desktop-core)

(defface my-home-button
  '((t :inherit link :weight bold))
  "Face of Home tab action buttons."
  :group 'my-desktop)

(defvar my-home--show-banner nil
  "Non-nil when a banner image is configured and exists.")

(use-package dashboard
  :ensure t
  :commands (dashboard-refresh-buffer)
  :custom
  (dashboard-banner-logo-title my-desktop-banner-title)
  (dashboard-center-content nil)     ; old-gnome-menu style: top-left
  (dashboard-vertically-center-content nil)
  (dashboard-set-footer nil)         ; no random quotes
  (dashboard-set-heading-icons my-desktop-use-icons)
  (dashboard-set-file-icons my-desktop-use-icons)
  (dashboard-items
   `((recents . ,my-desktop-home-recents)
     (bookmarks . ,my-desktop-home-bookmarks)
     (projects . ,my-desktop-home-projects)
     ,@(when my-desktop-org-agenda-files '((agenda . 5)))))
  :config
  (setq my-home--show-banner nil)
  (when my-desktop-banner-image
    (let ((img (expand-file-name my-desktop-banner-image)))
      (when (file-exists-p img)
        (setq dashboard-startup-banner img
              my-home--show-banner t))))

  (defun my-home--insert-heading (title)
    "Insert a standalone section heading line for TITLE."
    (insert (propertize title 'face 'dashboard-heading) "\n"))

  (defun dashboard-insert-my-home-actions (&optional _list-size)
    "Insert the configured action buttons (LIST-SIZE ignored)."
    (my-home--insert-heading "Actions")
    (dolist (item my-desktop-home-actions)
      (let ((label (car item))
            (command (cdr item)))
        (when (fboundp command)
          (widget-create 'push-button
                         :tag (format "  %s" label)
                         :button-face 'my-home-button
                         :action (lambda (&rest _)
                                   (call-interactively command))
                         :mouse-face 'highlight
                         :help-echo (symbol-name command))
          (insert "\n"))))
    (insert "\n"))

  (defun dashboard-insert-my-home-folders (&optional _list-size)
    "Insert configured folder shortcuts (LIST-SIZE ignored)."
    (when my-desktop-home-folders
      (my-home--insert-heading "Folders")
      (dolist (item my-desktop-home-folders)
        (let ((label (car item))
              (dir (expand-file-name (cdr item))))
          (widget-create 'push-button
                         :tag (format "  %s" label)
                         :button-face 'my-home-button
                         :action (lambda (&rest _) (find-file dir))
                         :mouse-face 'highlight
                         :help-echo dir)
          (insert "\n")))
      (insert "\n")))

  (setq dashboard-startupify-list
        (append (when my-home--show-banner
                  '(dashboard-insert-banner dashboard-insert-newline))
                (when my-desktop-banner-title
                  '(dashboard-insert-banner-title
                    dashboard-insert-newline))
                '(dashboard-insert-my-home-actions
                  dashboard-insert-newline)
                (when my-desktop-home-folders
                  '(dashboard-insert-my-home-folders
                    dashboard-insert-newline))
                `((dashboard-insert-recents . ,my-desktop-home-recents)
                  dashboard-insert-newline
                  (dashboard-insert-bookmarks . ,my-desktop-home-bookmarks)
                  dashboard-insert-newline
                  (dashboard-insert-projects . ,my-desktop-home-projects))
                (when my-desktop-org-agenda-files
                  '((dashboard-insert-agenda . 5)
                    dashboard-insert-newline)))))

;; Make sure the toggle also works from inside the dashboard buffer.
(with-eval-after-load 'dashboard
  (define-key dashboard-mode-map (kbd "C-c `") #'my-sidebar-toggle))

(defun my-home--buffer ()
  "Return a freshly rendered Home (dashboard) buffer, or nil.
Rendering must NOT switch windows: `dashboard-refresh-buffer'
steals the selected window, which made the dashboard pop up over
whatever the user had just chosen."
  (condition-case err
      (progn
        (require 'dashboard)  ; autoloads exist, but variables need a load
        (let ((buffer (get-buffer-create dashboard-buffer-name)))
          (with-current-buffer buffer
            (dashboard-insert-startupify-lists 'force-refresh))
          buffer))
    (error (message "[desktop] dashboard failed: %s"
                    (error-message-string err))
           nil)))

(defun my-home--initial-buffer ()
  "Initial buffer for new empty client frames: the Home tab."
  (or (ignore-errors (my-home--buffer))
      (get-buffer "*scratch*")))

(setq initial-buffer-choice #'my-home--initial-buffer)

(defun my-home-open ()
  "Open the Home tab now."
  (interactive)
  (if-let* ((buffer (my-home--buffer)))
      (switch-to-buffer buffer)
    (message "[desktop] dashboard not available")))

(provide 'desktop-home)
;;; desktop-home.el ends here
