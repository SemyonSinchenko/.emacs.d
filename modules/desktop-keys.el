;;; desktop-keys.el --- Keybindings and the Desktop root menu -*- lexical-binding: t; -*-

;;; Commentary:
;; Applies the machinery default bindings, then the user's
;; `my-desktop-keybindings' table on top (same key = user wins).
;; Unknown commands are reported as warnings, never break startup.
;; Also defines `my-desktop-menu', the root transient.

;;; Code:

(require 'transient)
(require 'desktop-config-defs)
(require 'desktop-core)

;; Workspaces live under the C-c w prefix (perspective-map): press
;; C-c w and which-key lists everything -- s switch/create, r rename,
;; c close, n/p or <right>/<left> cycle, 1..9/0 by position,
;; b switch buffer, o buffers overview, m/u merge/unmerge,
;; C-s/C-l state save/load.
;; Social apps live under the C-c s prefix: b bluesky, h hackernews,
;; l lemmy, r reddit, t telegram.
(defvar my-social-map (make-sparse-keymap)
  "Nested keymap of social apps, bound to C-c s.")

(defvar my-ai-map (make-sparse-keymap)
  "Nested keymap of AI commands, bound to C-c a.")

(defvar my-lexicon-map (make-sparse-keymap)
  "Nested keymap of lexicon AI text transforms, bound to C-c l.
Only bound when the lexicon module is loaded; the commands come
from the local lexicon-org checkout (see
`my-desktop-lexicon-dir').")

;; Roguelike copilot: Super-prefixed commands, active only in game
;; terminal buffers.  The keymap itself is defined by
;; desktop-roguelike.el, which loads before this file.
(defvar my-roguelike-keymap)

(defconst my-desktop--default-keys
  '((global ("C-x b" . persp-switch-to-buffer*) ; buffers of this workspace
            ("C-x k" . persp-kill-buffer*)      ; kill within workspace
            ("C-c `" . my-sidebar-toggle)       ; treemacs file tree
            ("C-c m" . my-sync-menu)
            ("C-c t" . my-term-new)
            ("C-c T" . my-term-project)
            ("C-c p" . my-media-toggle)
            ("C-c M-w" . my-clipboard-copy-image)) ; copy image at point
    (dired ("RET" . my-dired-find-file)) ; videos play in the external player
    (social ("b" . my-bluesky)
            ("h" . my-hackernews)
            ("l" . my-lem)
            ("r" . my-reddit)
            ("t" . my-telega))
    (roguelike ("s-d" . my-roguelike-describe-state)
               ("s-o" . my-roguelike-describe-object)
               ("s-a" . my-roguelike-advise)
               ("s-q" . my-roguelike-ask)
               ("s-h" . my-roguelike-explain-last)
               ("s-t" . my-roguelike-story)
               ("s-c" . my-roguelike-chat))
    (ai ("a" . my-ai-chat)            ; open a new chat
        ("r" . my-ai-session-rename)  ; rename session file
        ("o" . my-ai-session-open)    ; resume by name
        ("g" . gptel-menu)            ; gptel settings menu
        ("l" . my-ai-session-search)  ; search sessions
        ("s" . my-ai-session-save)    ; save session now
        ("z" . my-ai-zai-usage))      ; Z-AI coding plan usage
    (lexicon ("t" . lexicon-org-transform)        ; transform region
             ("p" . lexicon-org-transform-prompt) ; prompt for transform
             ("d" . lexicon-org-download)         ; download model
             ("s" . lexicon-org-status)           ; show status
             ("r" . lexicon-org-remove-last))))   ; undo last transform

(defun my-keys--bind (map key command)
  "Bind KEY to COMMAND in MAP, warning about problems."
  (cond
   ((not (fboundp command))
    (my-desktop--warn "key %s: command `%s' not defined"
                      key command))
   (t (define-key map (kbd key) command))))

(defun my-keys--apply-context (context binds)
  "Apply BINDS for CONTEXT (a symbol)."
  (pcase context
    ('global
     (dolist (b binds)
       (my-keys--bind global-map (car b) (cdr b))))
    ('dired
     (with-eval-after-load 'dired
       (dolist (b binds)
         (my-keys--bind dired-mode-map (car b) (cdr b)))))
    ('ghostel
     (with-eval-after-load 'ghostel
       (dolist (b binds)
         (my-keys--bind ghostel-mode-map (car b) (cdr b)))))
    ('social
     (dolist (b binds)
       (my-keys--bind my-social-map (car b) (cdr b))))
    ('ai
     (dolist (b binds)
       (my-keys--bind my-ai-map (car b) (cdr b))))
    ('lexicon
     ;; Bound only when the lexicon-org autoloads exist (the module
     ;; is loaded AND the checkout is installed); otherwise skip
     ;; silently -- the module already warned.
     (when (fboundp 'lexicon-org-transform)
       (dolist (b binds)
         (my-keys--bind my-lexicon-map (car b) (cdr b)))))
    ('roguelike
     (dolist (b binds)
       (my-keys--bind my-roguelike-keymap (car b) (cdr b))))
    (_ (my-desktop--warn "unknown keybinding context %S" context))))

(defun my-keys-apply ()
  "Apply default and user keybindings."
  (dolist (entry my-desktop--default-keys)
    (my-keys--apply-context (car entry) (cdr entry)))
  (define-key global-map (kbd "C-c s") my-social-map)
  (define-key global-map (kbd "C-c a") my-ai-map)
  (when (fboundp 'lexicon-org-transform)
    (define-key global-map (kbd "C-c l") my-lexicon-map))
  (dolist (entry (append my-desktop-keybindings nil))
    (my-keys--apply-context (car entry) (cdr entry))))

(my-keys-apply)

;; ------------------------------------------------------------------
;; Root menu
;; ------------------------------------------------------------------

(transient-define-prefix my-desktop-menu ()
  "Emacs Desktop root menu."
  [["Workspaces"
    ("g" "Switch workspace" my-ws-switch
     :if (lambda () (fboundp 'my-ws-switch)))
    ("n" "New workspace" my-ws-new
     :if (lambda () (fboundp 'my-ws-new)))
    ("r" "Rename workspace" my-ws-rename-group
     :if (lambda () (fboundp 'my-ws-rename-group)))
    ("d" "Close workspace" my-ws-close
     :if (lambda () (fboundp 'my-ws-close)))
    ("`" "Files (treemacs)" my-sidebar-toggle
     :if (lambda () (fboundp 'my-sidebar-toggle)))
    ("o" "Buffers overview" my-sidebar-buffers
     :if (lambda () (fboundp 'my-sidebar-buffers)))]
   ["Apps"
    ("t" "New terminal" my-term-new
     :if (lambda () (fboundp 'my-term-new)))
    ("a" "AI chat" my-ai-chat
     :if (lambda () (fboundp 'my-ai-chat)))
    ("z" "Z-AI usage" my-ai-zai-usage
     :if (lambda () (fboundp 'my-ai-zai-usage)))
    ("l" "Lexicon transform" lexicon-org-transform
     :if (lambda () (fboundp 'lexicon-org-transform)))
    ("m" "Music library" my-media-music-library
     :if (lambda () (fboundp 'my-media-music-library)))
    ("e" "Elfeed" my-rss-open
     :if (lambda () (fboundp 'my-rss-open)))
    ("L" "Lemmy" my-lem
     :if (lambda () (fboundp 'my-lem)))
    ("B" "Bluesky" my-bluesky
     :if (lambda () (fboundp 'my-bluesky)))
    ("H" "Hacker News" my-hackernews
     :if (lambda () (fboundp 'my-hackernews)))
    ("T" "Telegram" my-telega
     :if (lambda () (fboundp 'my-telega)))
    ("R" "Reddit" my-reddit
     :if (lambda () (fboundp 'my-reddit)))]
   ["System"
    ("s" "Sync menu" my-sync-menu
     :if (lambda () (fboundp 'my-sync-menu)))
    ("r" "Reload config" my-desktop-reload-config)]])

;; ------------------------------------------------------------------
;; Reload
;; ------------------------------------------------------------------

(defun my-desktop-reload-config ()
  "Re-read desktop-config.el and re-apply what can be applied live."
  (interactive)
  (let* ((cfg (expand-file-name "desktop-config.el" user-emacs-directory))
         (local (expand-file-name "desktop-config-local.el"
                                  user-emacs-directory)))
    (dolist (f (list cfg local))
      (when (file-exists-p f)
        (condition-case err
            (load f nil t)
          (error (message "[desktop] reload of %s failed: %s"
                          f (error-message-string err))))))
    (my-keys-apply)
    ;; Frame look (font, opacity) applies live too.
    (when (fboundp 'my-desktop--apply-frame-now)
      (my-desktop--apply-frame-now))
    (message
     "[desktop] config reloaded (frame/package/module-flag changes \
need a daemon restart)")))

(provide 'desktop-keys)
;;; desktop-keys.el ends here
