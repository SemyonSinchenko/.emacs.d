;;; desktop-workspaces.el --- Workspaces: perspective.el + treemacs -*- lexical-binding: t; -*-

;;; Commentary:
;; Workspaces are perspective.el perspectives:
;; - every workspace isolates its buffer list (switching and killing
;;   buffers stays inside the current workspace),
;; - every workspace keeps its own window layout,
;; - treemacs, scoped per perspective, is the sidebar navigator
;;   (it replaces the hand-rolled sidebar panel).
;;
;; Workspaces start fresh from `my-desktop-workspaces' on the first
;; frame.  Session save/restore is postponed: no package can serialize
;; live processes (terminals, mpv), so a relaunch-profile design is
;; planned instead of a custom session format.
;;
;; Public API (kept stable for user keybindings and other modules):
;;   my-ws-switch, my-ws-new, my-ws-rename-group, my-ws-close,
;;   my-ws-cycle-next, my-ws-cycle-prev, my-ws-current-group,
;;   my-ws-initialize,
;;   my-sidebar-show, my-sidebar-hide, my-sidebar-toggle, my-sidebar-focus,
;;   my-sidebar-buffers
;;
;; Buffer switching notes:
;; - C-x b is rebound to `persp-switch-to-buffer*': the completion
;;   shows only the buffers of the current workspace (plus shared
;;   buffers); a selected buffer from another workspace is imported.
;; - `switch-to-prev-buffer-skip' keeps C-x <arrow> buffer cycling
;;   inside the current workspace.
;; - `consult-buffer' intentionally still lists all buffers: it is the
;;   cross-workspace escape hatch.

;;; Code:

(require 'desktop-config-defs)
(require 'desktop-core)

;; ------------------------------------------------------------------
;; Top status strip: the tab bar repurposed
;; ------------------------------------------------------------------
;; Now-playing (listen) on the left, clock/battery on the right.
;; Segments read specific VARIABLES (display-time-string,
;; battery-mode-line-string, listen's lighter function) -- NEVER
;; global-mode-string wholesale: listen pushes its own mode-line
;; template into it, which errors while rendered from the tab bar.
;; No tabs are ever displayed: `tab-bar-format' contains no tab
;; entries, which keeps perspective's tab-bar caveat moot.

(defun my-tab-bar-now-playing ()
  "Tab-bar segment: what listen.el is playing, or nothing.
Calls listen's lighter FUNCTION (never its global-mode-string
element, which breaks rendering while its timer is not yet
running) and never lets errors reach redisplay."
  (when (and (bound-and-true-p listen-player)
             (fboundp 'listen-mode-lighter))
    (condition-case nil
        (let ((lighter (listen-mode-lighter)))
          (unless (string-empty-p (string-trim lighter))
            (concat (propertize " ♫ " 'face 'font-lock-keyword-face)
                    lighter)))
      (error nil))))

(defun my-tab-bar-clock ()
  "Tab-bar segment: time and battery, right-aligned neighbors."
  (condition-case nil
      (string-trim
       (concat (and (boundp 'display-time-string) display-time-string)
               " "
               (and (boundp 'battery-mode-line-string)
                    battery-mode-line-string)))
    (error nil)))

(defun my-tab-bar-workspaces ()
  "Tab-bar segment: perspective's workspace block.
Reuses perspective's own rendering, so all workspaces are listed
with the current one highlighted (and clickable)."
  (condition-case nil
      (when (bound-and-true-p persp-mode)
        (apply #'concat (persp-mode-line)))
    (error nil)))

(setq tab-bar-format
      '(my-tab-bar-workspaces
        my-tab-bar-now-playing
        tab-bar-format-align-right
        my-tab-bar-clock))

(unless noninteractive (tab-bar-mode 1))
(setq tab-bar-show t)

;; Time/battery now live in the top strip; keep the mode line slim.
(setq mode-line-misc-info
      (assq-delete-all 'global-mode-string
                       (copy-alist mode-line-misc-info)))

;; ------------------------------------------------------------------
;; Engine: perspective.el
;; ------------------------------------------------------------------

(use-package perspective
  :ensure t
  :custom
  ;; The WHOLE perspective command set lives on C-c w (which-key
  ;; lists it): s = switch/create, r = rename, c = kill, n/p or
  ;; <right>/<left> = cycle, 1..9/0 = switch by position, b = switch
  ;; buffer, o = buffers overview, m/u = merge/unmerge,
  ;; C-s/C-l = state save/load.
  (persp-mode-prefix-key (kbd "C-c w"))
  ;; Stable GNOME-like order: oldest workspace first, so numbers and
  ;; cycling keep their positions across renames.
  (persp-sort 'oldest)
  :config
  (persp-mode 1)
  ;; C-c w b must agree with C-x b: scope to the current workspace.
  ;; The stock binding lists the buffers of ALL workspaces.
  (define-key perspective-map (kbd "b") #'persp-switch-to-buffer*)
  ;; C-c w o: the buffers-by-workspace overview panel.
  (define-key perspective-map (kbd "o") #'my-sidebar-buffers)
  ;; C-c w 1..9 and 0: switch to the Nth workspace.
  (dotimes (n 9)
    (define-key perspective-map (kbd (number-to-string (1+ n)))
                (lambda () (interactive) (persp-switch-by-number (1+ n)))))
  (define-key perspective-map (kbd "0")
              (lambda () (interactive) (persp-switch-by-number 10)))
  ;; Buffer cycling (C-x <right>/<left>) must not leave the workspace.
  (setq switch-to-prev-buffer-skip
        (lambda (_win buff _bury-or-kill)
          (and (bound-and-true-p persp-mode)
               (not (persp-is-current-buffer buff))))))

;; ------------------------------------------------------------------
;; Sidebar: treemacs, one scoped tree per workspace
;; ------------------------------------------------------------------

(use-package treemacs
  :ensure t
  :defer t
  :custom
  ;; Hidden files stay hidden until toggled with
  ;; M-x treemacs-toggle-show-dotfiles.
  (treemacs-show-hidden-files nil)
  :config
  (when my-desktop-sidebar-width
    (setq treemacs-width my-desktop-sidebar-width))
  (treemacs-filewatch-mode 1)
  (treemacs-follow-mode 1)
  ;; Do not thrash on huge repositories.
  (setq treemacs-git-mode 'deferred))

(use-package treemacs-perspective
  :ensure t
  :after (treemacs perspective)
  :config
  (treemacs-set-scope-type 'Perspectives))

;; ------------------------------------------------------------------
;; Fresh-session workspace setup
;; ------------------------------------------------------------------

(defvar my-ws--seeded nil
  "Workspaces whose treemacs root project was already seeded.")

(defun my-ws--workspace-names ()
  "Ordered, de-duplicated list of workspace names from the config.
When `my-desktop-workspaces' is empty, fall back to the default
workspace name -- never add it as an EXTRA workspace."
  (or (delete-dups (append my-desktop-workspaces nil))
      (list (or my-desktop-default-workspace "main"))))

(defun my-ws--seed-sidebar (name)
  "Give workspace NAME's treemacs tree its root project.
Prevents the \"select the root of the first project\" prompt."
  (when (and (fboundp 'treemacs-do-add-project-to-workspace)
             my-desktop-treemacs-root
             (not (member name my-ws--seeded)))
    (push name my-ws--seeded)
    (condition-case err
        (let ((result (treemacs-do-add-project-to-workspace
                       (expand-file-name my-desktop-treemacs-root)
                       (format "Home (%s)" name))))
          (unless (member (car-safe result) '(success duplicate-project
                                                      includes-project))
            (my-desktop--warn "treemacs seed: %S" result)))
      (error (my-desktop--warn "treemacs seed failed: %s"
                               (error-message-string err))))))

(defun my-ws--ensure-treemacs ()
  "Load the treemacs machinery; return non-nil when available."
  (condition-case err
      (progn (require 'treemacs)
             (require 'treemacs-perspective)
             t)
    (error (my-desktop--warn "treemacs sidebar unavailable: %s"
                             (error-message-string err))
           nil)))

(defun my-ws-initialize ()
  "Create the workspaces configured in `my-desktop-workspaces'.
The boot perspective (\"main\") is renamed to the first configured
workspace, so no stray \"main\" remains.  Harmless to call again."
  (interactive)
  (when (bound-and-true-p persp-mode)
    (when my-desktop-sidebar-autoshow
      ;; Load the sidebar machinery now, so seeding lands in the
      ;; right per-workspace tree and no prompt appears later.
      (my-ws--ensure-treemacs))
    (let* ((names (my-ws--workspace-names))
           (first (car names)))
      (unless (or (member (persp-current-name) names)
                  (member first (persp-names)))
        (condition-case nil (persp-rename first) (error nil)))
      (dolist (name names)
        (unless (member name (persp-names))
          (persp-switch name))
        (my-ws--seed-sidebar name))
      (persp-switch first))))

;; ------------------------------------------------------------------
;; Workspace commands (stable my-ws-* API over perspective.el)
;; ------------------------------------------------------------------

(defun my-ws-current-group ()
  "Name of the current workspace."
  (if (bound-and-true-p persp-mode)
      (persp-current-name)
    (or my-desktop-default-workspace "main")))

(defun my-ws-switch (&optional name)
  "Switch to workspace NAME, creating it when it does not exist.
Without NAME, prompt; typing a new name creates that workspace."
  (interactive)
  (unless name
    (setq name (completing-read
                (format "Switch or create workspace (default %s): "
                        (my-ws-current-group))
                (persp-names) nil nil nil nil
                (my-ws-current-group))))
  (persp-switch name))

(defun my-ws-new (name)
  "Create and switch to a new workspace NAME."
  (interactive (list (read-string
                      "New workspace: "
                      (format "Workspace %d"
                              (1+ (length (persp-names)))))))
  (persp-switch name))

(defun my-ws-rename-group (name)
  "Rename the current workspace to NAME."
  (interactive (list (read-string "Rename workspace to: "
                                  (my-ws-current-group))))
  (persp-rename name))

(defun my-ws-close ()
  "Close the current workspace (its buffers stay in memory)."
  (interactive)
  (if (<= (length (persp-names)) 1)
      (user-error "Refusing to close the last workspace")
    (persp-kill (persp-current-name))))

(defun my-ws-cycle-next ()
  "Switch to the next workspace."
  (interactive)
  (persp-next))

(defun my-ws-cycle-prev ()
  "Switch to the previous workspace."
  (interactive)
  (persp-prev))

;; ------------------------------------------------------------------
;; Sidebar commands (stable my-sidebar-* API over treemacs)
;; ------------------------------------------------------------------

(defun my-sidebar--visible-p ()
  "Non-nil when the treemacs window is visible on this frame."
  (and (fboundp 'treemacs-current-visibility)
       (eq (treemacs-current-visibility) 'visible)))

(defun my-sidebar--width-cols ()
  "Sidebar width in columns from the user settings."
  (or my-desktop-sidebar-width
      (max 24 (min 48 (/ (frame-width)
                         (max 2 my-desktop-sidebar-fraction))))))

(defun my-sidebar--apply-width ()
  "Apply the configured sidebar width to treemacs."
  (setq treemacs-width (my-sidebar--width-cols)))

(defconst my-ws-buffers-buffer "*Workspaces*"
  "Name of the workspace buffer-overview panel.")

(defun my-sidebar-buffers ()
  "Show a sidebar panel of buffers grouped by workspace.
Every workspace is one group, like a GNOME-workspace overview.
The panel takes the left side window (replacing the treemacs tree
while open); `C-c `' brings the file tree back."
  (interactive)
  (require 'ibuffer)
  (let ((buf (get-buffer-create my-ws-buffers-buffer)))
    (with-current-buffer buf
      (unless (derived-mode-p 'ibuffer-mode)
        (ibuffer-mode))
      (persp-ibuffer-set-filter-groups)
      (ibuffer-auto-mode 1)          ; keep the overview fresh
      (ibuffer-update nil t))
    (pop-to-buffer
     buf
     `((display-buffer-in-side-window
        (side . left)
        (window-width . ,(my-sidebar--width-cols)))))))

(defun my-sidebar-show ()
  "Show the treemacs sidebar without stealing focus."
  (interactive)
  (when (and (my-ws--ensure-treemacs)
             (not (my-sidebar--visible-p)))
    (my-ws--seed-sidebar (my-ws-current-group))
    (my-sidebar--apply-width)
    (save-selected-window (treemacs))))

(defun my-sidebar-hide ()
  "Hide the treemacs sidebar."
  (interactive)
  (when (and (fboundp 'treemacs-get-local-window)
             (my-sidebar--visible-p))
    (delete-window (treemacs-get-local-window))))

(defun my-sidebar-toggle ()
  "Toggle the treemacs sidebar (file tree)."
  (interactive)
  (when (my-ws--ensure-treemacs)
    (my-ws--seed-sidebar (my-ws-current-group))
    (my-sidebar--apply-width)
    (treemacs)))

(defun my-sidebar-focus ()
  "Select (and if needed open) the treemacs sidebar window."
  (interactive)
  (when (my-ws--ensure-treemacs)
    (my-ws--seed-sidebar (my-ws-current-group))
    (treemacs-select-window)))

(provide 'desktop-workspaces)
;;; desktop-workspaces.el ends here
