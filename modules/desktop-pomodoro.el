;;; desktop-pomodoro.el --- Pomodoro timer (pomm) for the Desktop -*- lexical-binding: t; -*-

;;; Commentary:
;; pomm.el (MELPA) with GNOME desktop notifications over D-Bus.
;; Notifications use the built-in notifications.el
;; (org.freedesktop.Notifications), which GNOME Shell serves on
;; Wayland -- no notify-send / libnotify binary needed.  pomm\='s own
;; alert call stays an in-Emacs echo message.
;;
;; Keys live under C-c o p (see desktop-keys.el).

;;; Code:

(require 'desktop-config-defs)
(require 'desktop-core)

(use-package pomm
  :ensure t
  :defer t
  :commands (pomm pomm-start pomm-stop pomm-pause pomm-set-context
                  pomm-start-with-context pomm-mode-line-mode)
  :custom
  (pomm-work-period my-desktop-pomodoro-work-minutes)
  (pomm-short-break-period my-desktop-pomodoro-short-break-minutes)
  (pomm-long-break-period my-desktop-pomodoro-long-break-minutes)
  (pomm-number-of-periods my-desktop-pomodoro-periods-per-round)
  :init
  ;; pomm errors at play time when no .wav player is found, so enable
  ;; the sound only when one is actually available.  A plain setq
  ;; here wins, because defcustom never overwrites a bound variable.
  (when (and my-desktop-pomodoro-sound
             (or (executable-find "aplay")
                 (executable-find "afplay")))
    (setq pomm-audio-enabled t)))

(defvar pomm--state)
(declare-function pomm--sync-org-clock "pomm")

;; ------------------------------------------------------------------
;; GNOME notifications over D-Bus (org.freedesktop.Notifications)
;; ------------------------------------------------------------------

(defun my-pomodoro--notify (title body &optional urgency)
  "Send a desktop notification TITLE/BODY with URGENCY over D-Bus."
  (condition-case err
      (progn
        (require 'notifications)
        (notifications-notify :title title
                              :body body
                              :app-name "Emacs"
                              :urgency (or urgency 'normal)))
    (error (my-desktop--warn "pomodoro notification failed: %s"
                             (error-message-string err)))))

(defun my-pomodoro--status-notify ()
  "Send a GNOME notification about the current pomodoro status.
Intended for `pomm-on-status-changed-hook'."
  (let ((context (alist-get 'context pomm--state)))
    (pcase (cons (alist-get 'status pomm--state)
                 (alist-get 'kind (alist-get 'current pomm--state)))
      (`(running . work)
       (my-pomodoro--notify
        "Pomodoro"
        (concat pomm-work-message
                (if context (format " (%s)" context) ""))))
      (`(running . short-break)
       (my-pomodoro--notify "Pomodoro" pomm-short-break-message))
      (`(running . long-break)
       (my-pomodoro--notify "Pomodoro" pomm-long-break-message))
      (`(stopped . ,_)
       (my-pomodoro--notify "Pomodoro" "Timer stopped." 'low)))))

(when my-desktop-pomodoro-notifications
  (add-hook 'pomm-on-status-changed-hook #'my-pomodoro--status-notify))

;; org-clock integration, as documented by pomm.el: clock in on the
;; last task when work starts, clock out on breaks/stops.
(when my-desktop-pomodoro-org-clock
  (add-hook 'pomm-on-status-changed-hook #'pomm--sync-org-clock))

(provide 'desktop-pomodoro)
;;; desktop-pomodoro.el ends here
