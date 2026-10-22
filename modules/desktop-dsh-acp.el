;;; desktop-dsh-acp.el --- DSH ACP agents through agent-shell -*- lexical-binding: t; -*-

;;; Commentary:
;; ACP bridge to the DSH dev containers.
;;
;; The web/dsh-emacs integration lives in desktop-dsh.el and stays
;; available as a fallback.  This module is the new default: the same
;; run-dsh container machinery, but with `run-dsh --acp', which starts
;; `dsh --profile acp' inside the project container and speaks ACP over
;; stdio.  agent-shell owns the client and the chat UI.
;;
;; One agent-shell buffer per project.  Opening the perspective's agent
;; focuses the existing buffer instead of starting another ACP process;
;; killing it closes stdin and stops the ACP container.  The project
;; root is resolved through the project perspective first, then
;; project.el, then `default-directory'.
;;
;; `C-c a d r' / `k' target this ACP agent.  The old dsh-emacs web
;; commands remain reachable from the same menu as `w' / `W' and through
;; `C-c d' / `M-x my-dsh-open'.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'project)
(require 'transient)
(require 'desktop-config-defs)

;; agent-shell is installed from MELPA; defer loading it until the first
;; agent command.  Install it once if missing, then let use-package set
;; up the autoloads.  A failed install leaves the module loaded and the
;; commands report a clear error instead of breaking the Desktop.
(require 'use-package nil t)
(unless (package-installed-p 'agent-shell)
  (condition-case err
      (progn
        (package-refresh-contents)
        (package-install 'agent-shell))
    (error
     (if (fboundp 'my-desktop--warn)
         (my-desktop--warn "agent-shell install failed: %s"
                           (error-message-string err))
       (message "DSH ACP: agent-shell install failed: %s"
                (error-message-string err))))))
(when (package-installed-p 'agent-shell)
  (use-package agent-shell
    :ensure t
    :defer t
    :commands (agent-shell-start
               agent-shell-project-buffers
               agent-shell-get-config)))

(declare-function agent-shell-start "agent-shell" t)
(declare-function agent-shell-project-buffers "agent-shell" (&optional cwd))
(declare-function agent-shell-get-config "agent-shell" (buffer))
(declare-function agent-shell-clear-buffer "agent-shell" ())
(declare-function agent-shell-send-file "agent-shell"
                  (&optional prompt-for-file pick-shell))
(declare-function agent-shell--make-acp-client "agent-shell" t)
(declare-function agent-shell-deepseek-make-agent-config "agent-shell-deepseek")
(declare-function agent-shell-deepseek--welcome-message "agent-shell-deepseek"
                  (config))
(declare-function my-project-current-root "desktop-project")
(declare-function persp-add-buffer "perspective" (buffer-or-name))
(declare-function my-dsh-open "desktop-dsh" (dir))
(declare-function my-dsh-kill "desktop-dsh" ())
(declare-function my-desktop--warn "desktop-init" (&rest args))

;; Mark the agent-shell options special before the first `let'-binding:
;; agent-shell itself is deferred, so its `defcustom' has not run yet.
(defvar agent-shell-session-strategy)
(defvar agent-shell-command-prefix)

(defun my-dsh-acp--require-agent-shell ()
  "Load agent-shell or report a clear installation error."
  (unless (require 'agent-shell nil t)
    (user-error "DSH ACP: `agent-shell' is not installed; \
install it from MELPA")))

(defun my-dsh-acp--make-client (buffer)
  "Create the ACP client for BUFFER.
The command comes from `my-desktop-dsh-acp-command'; run-dsh owns the
container, project directory and environment, so no environment
variables are injected here."
  (unless buffer
    (error "Missing required argument: :buffer"))
  (my-dsh-acp--require-agent-shell)
  ;; run-dsh already owns the container; a global command prefix would
  ;; wrap the launcher a second time.
  (let ((agent-shell-command-prefix nil))
    (agent-shell--make-acp-client
     :command (car my-desktop-dsh-acp-command)
     :command-params (cdr my-desktop-dsh-acp-command)
     :context-buffer buffer)))

(defun my-dsh-acp--agent-config ()
  "Return the agent-shell configuration for the DSH ACP server.
It starts from the DeepSeek configuration (welcome art, prompt and
icon), but gets its own identifier, buffer name and client maker."
  (my-dsh-acp--require-agent-shell)
  (let ((config (agent-shell-deepseek-make-agent-config)))
    (setf (alist-get :identifier config) 'dsh-acp
          (alist-get :mode-line-name config) "DSH ACP"
          (alist-get :buffer-name config) "DSH"
          (alist-get :shell-prompt config) "DSH> "
          (alist-get :shell-prompt-regexp config) "DSH> "
          (alist-get :client-maker config)
          (lambda (buffer) (my-dsh-acp--make-client buffer))
          (alist-get :default-config-options config)
          (lambda () my-desktop-dsh-acp-default-config-options)
          (alist-get :install-instructions config)
          "Install run-dsh from sem-dev-container on `exec-path'; the
image must provide `run-dsh --acp'.")
    config))

(defun my-dsh-acp--project-root ()
  "Return the canonical project root for the current perspective."
  (let* ((root (or (and (fboundp 'my-project-current-root)
                        (my-project-current-root))
                   (when-let* ((project (project-current)))
                     (project-root project))
                   default-directory))
         (root (file-name-as-directory (expand-file-name root))))
    (unless (file-directory-p root)
      (user-error "DSH ACP: project directory does not exist: %s" root))
    root))

(defun my-dsh-acp--project-shell (root)
  "Return the live DSH ACP shell buffer for ROOT, or nil."
  (my-dsh-acp--require-agent-shell)
  (let ((shells
         (seq-filter
          (lambda (buffer)
            (and (buffer-live-p buffer)
                 (eq (alist-get :identifier (agent-shell-get-config buffer))
                     'dsh-acp)))
          (agent-shell-project-buffers root))))
    (when (> (length shells) 1)
      (message "DSH ACP: %d agent shells in %s; using %s"
               (length shells) root (buffer-name (car shells))))
    (car shells)))

(defun my-dsh-acp--start (root)
  "Start a DSH ACP agent-shell for ROOT."
  (let ((command my-desktop-dsh-acp-command))
    (unless (and (consp command) (stringp (car command)))
      (user-error "DSH ACP: `my-desktop-dsh-acp-command' must be \
a non-empty list of strings"))
    (unless (executable-find (car command))
      (user-error "DSH ACP: `%s' not found on `exec-path'; \
install run-dsh from sem-dev-container"
                  (car command))))
  ;; Load agent-shell before let-binding its option: its `defcustom'
  ;; must establish the global default, and it does not override an
  ;; already-bound value.
  (my-dsh-acp--require-agent-shell)
  (let ((default-directory root)
        (agent-shell-session-strategy my-desktop-dsh-acp-session-strategy))
    (condition-case err
        (let ((buffer (agent-shell-start :config (my-dsh-acp--agent-config))))
          (when (and (buffer-live-p buffer) (fboundp 'persp-add-buffer))
            (persp-add-buffer buffer))
          (when (buffer-live-p buffer)
            (pop-to-buffer buffer))
          buffer)
      (error
       (user-error "DSH ACP failed to start in %s: %s"
                   root (error-message-string err))))))

;;;###autoload
(defun my-dsh-acp-open ()
  "Open or focus the DSH ACP agent for this project."
  (interactive)
  (let* ((root (my-dsh-acp--project-root))
         (buffer (my-dsh-acp--project-shell root)))
    (if (buffer-live-p buffer)
        (progn
          (when (fboundp 'persp-add-buffer)
            (persp-add-buffer buffer))
          (pop-to-buffer buffer)
          (message "DSH ACP: focused %s" (buffer-name buffer)))
      (my-dsh-acp--start root))))

;;;###autoload
(defun my-dsh-acp-kill ()
  "Stop the DSH ACP agent for this project.
Killing the agent-shell buffer closes the ACP client's stdin, which
stops the container."
  (interactive)
  (let* ((root (my-dsh-acp--project-root))
         (buffer (my-dsh-acp--project-shell root)))
    (unless (buffer-live-p buffer)
      (user-error "No DSH ACP agent in this perspective"))
    (kill-buffer buffer)
    (if (buffer-live-p buffer)
        (message "DSH ACP: kill cancelled; agent still running")
      (message "DSH ACP: stopped the agent in %s" root))))

;;;###autoload
(defun my-dsh-acp-restart ()
  "Restart the DSH ACP agent for this project."
  (interactive)
  (let* ((root (my-dsh-acp--project-root))
         (buffer (my-dsh-acp--project-shell root)))
    (when (buffer-live-p buffer)
      (kill-buffer buffer)
      (when (buffer-live-p buffer)
        (user-error "DSH ACP: agent is still running")))
    (my-dsh-acp--start root)))

;;;###autoload
(defun my-dsh-acp-clear-buffer ()
  "Clear the DSH ACP agent buffer for this project.
When called from a DSH ACP shell, clear that buffer; otherwise clear
the shell belonging to the current project."
  (interactive)
  (my-dsh-acp--require-agent-shell)
  (let ((buffer
         (if (and (derived-mode-p 'agent-shell-mode)
                  (eq (alist-get :identifier
                                 (agent-shell-get-config (current-buffer)))
                      'dsh-acp))
             (current-buffer)
           (my-dsh-acp--project-shell (my-dsh-acp--project-root)))))
    (unless (buffer-live-p buffer)
      (user-error "No DSH ACP agent in this perspective"))
    (with-current-buffer buffer
      (unless (fboundp 'agent-shell-clear-buffer)
        (user-error "DSH ACP: `agent-shell-clear-buffer' is not available"))
      (agent-shell-clear-buffer)
      (message "DSH ACP: cleared %s" (buffer-name buffer)))))

;;;###autoload
(defun my-dsh-acp-send-file (&optional prompt-for-file)
  "Send a file to the DSH ACP agent.
With prefix argument PROMPT-FOR-FILE, always prompt for the file."
  (interactive "P")
  (my-dsh-acp--require-agent-shell)
  (agent-shell-send-file prompt-for-file))

;; ------------------------------------------------------------------
;; Menu: C-c a d
;; ------------------------------------------------------------------

(transient-define-prefix my-dsh-acp-menu ()
  "DSH menu: ACP agent by default, old dsh-emacs web underneath."
  [["DSH ACP"
    ("r" "Run / focus agent" my-dsh-acp-open :transient nil)
    ("k" "Kill agent" my-dsh-acp-kill :transient nil)
    ("R" "Restart agent" my-dsh-acp-restart :transient nil)
    ("n" "Clear agent buffer" my-dsh-acp-clear-buffer :transient nil)
    ("f" "Send file to agent" my-dsh-acp-send-file :transient nil)]
   ["Old dsh-emacs (web)"
    ("w" "Run / reconnect web" my-dsh-open :transient nil
     :if (lambda () (fboundp 'my-dsh-open)))
    ("W" "Kill web session" my-dsh-kill :transient nil
     :if (lambda () (fboundp 'my-dsh-kill)))]])

(provide 'desktop-dsh-acp)
;;; desktop-dsh-acp.el ends here
