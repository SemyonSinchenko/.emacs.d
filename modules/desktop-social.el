;;; desktop-social.el --- Feeds: Lemmy, Bluesky, Hacker News -*- lexical-binding: t; -*-

;;; Commentary:
;; The social feed readers of the Desktop, each switchable by flag:
;;
;; - lem.el (Lemmy): set `my-desktop-lem-instance-url' to your
;;   instance; the first `my-lem' call asks for username/password and
;;   stores the token in a plstore under user-emacs-directory.
;; - bluesky.el: credentials come from auth-source -- add to
;;   ~/.authinfo:  machine bsky.social login YOUR-HANDLE password
;;   YOUR-APP-PASSWORD (an app password, not the account password).
;; - hackernews.el: no account needed, `my-hackernews' just works.

;;; Code:

(require 'desktop-config-defs)
(require 'desktop-core)

;; ------------------------------------------------------------------
;; Lemmy (lem.el)
;; ------------------------------------------------------------------

(when my-desktop-enable-lem
  (use-package lem
    :ensure t
    :commands (lem)
    :init
    (when my-desktop-lem-instance-url
      (setq lem-instance-url my-desktop-lem-instance-url))
    :config
    (when my-desktop-lem-username
      (setq lem-current-user my-desktop-lem-username)))

  (defun my-lem ()
    "Open Lemmy."
    (interactive)
    (if (fboundp 'lem)
        (lem)
      (user-error "lem is not installed"))))

;; ------------------------------------------------------------------
;; Bluesky (bluesky.el)
;; ------------------------------------------------------------------

(when my-desktop-enable-bluesky
  (use-package bluesky
    :ensure t
    :commands (bluesky bluesky-search bluesky-notifications))

  (defun my-bluesky ()
    "Open the Bluesky timeline."
    (interactive)
    (if (fboundp 'bluesky)
        (bluesky)
      (user-error "bluesky is not installed (it needs Emacs 30.1+)"))))

;; ------------------------------------------------------------------
;; Reddit (reddigg via browser-gt)
;; ------------------------------------------------------------------
;; reddigg runs Reddit requests inside YOUR browser (where you are
;; logged in) through the browser-gt bridge: Emacs side (this
;; module, started automatically on first use) + the browser-gt
;; extension installed in Firefox/Chrome + a logged-in reddit.com
;; tab.  See https://github.com/dmgerman/browser-gt

(when my-desktop-enable-reddit
  (use-package browser-gt
    :ensure t
    :commands (browser-gt-start))

  (use-package reddigg
    :ensure t
    :commands (reddigg-view-frontpage reddigg-view-main reddigg-view-sub
                                      reddigg-view-comments)
    :init
    (when my-desktop-reddit-subs
      (setq reddigg-subs (append my-desktop-reddit-subs nil)))
    :config
    (condition-case err
        (browser-gt-start)
      (error (my-desktop--warn "browser-gt start failed: %s"
                               (error-message-string err)))))

  (defun my-reddit ()
    "Open Reddit (front page, via the browser bridge)."
    (interactive)
    (if (and (fboundp 'reddigg-view-frontpage) (fboundp 'browser-gt-start))
        (reddigg-view-frontpage)
      (user-error "reddigg/browser-gt not installed (browser-gt needs Emacs 30.1+)"))))

;; ------------------------------------------------------------------
;; Hacker News (hackernews.el)
;; ------------------------------------------------------------------

(when my-desktop-enable-hackernews
  (use-package hackernews
    :ensure t
    :commands (hackernews))

  (defun my-hackernews ()
    "Open Hacker News (top stories)."
    (interactive)
    (if (fboundp 'hackernews)
        (hackernews)
      (user-error "hackernews is not installed"))))

(provide 'desktop-social)
;;; desktop-social.el ends here
