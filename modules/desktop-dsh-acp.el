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
;; `C-c a d r' / `k' target this ACP agent.  `o' resumes a session by
;; id, `l' lists this project's sessions, and `s' searches the project's
;; transcripts with Consult.  Session discovery reads the transcript
;; front matter (one file per shell start, same `session_id' after a
;; resume), so it needs no ACP `session/list' capability.  The old
;; dsh-emacs web commands remain reachable from the same menu as
;; `w' / `W' and through `C-c d' / `M-x my-dsh-open'.

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
(declare-function agent-shell-restart "agent-shell" t)
(declare-function agent-shell-session-id "agent-shell" t)
(declare-function agent-shell-project-buffers "agent-shell" (&optional cwd))
(declare-function agent-shell-get-config "agent-shell" (buffer))
(declare-function consult-ripgrep "consult" (&optional dir initial))
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

(defun my-dsh-acp--project-root (&optional no-fallback)
  "Return the canonical project root for the current perspective.
When NO-FALLBACK is non-nil, return nil instead of falling back to
`default-directory'."
  (let* ((root (or (and (fboundp 'my-project-current-root)
                        (my-project-current-root))
                   (when-let* ((project (project-current)))
                     (project-root project))
                   (unless no-fallback
                     default-directory)))
         (root (and root
                    (file-name-as-directory (expand-file-name root)))))
    (when root
      (unless (file-directory-p root)
        (user-error "DSH ACP: project directory does not exist: %s" root))
      root)))

(defun my-dsh-acp--history-root ()
  "Return the current project root for session history, or nil.
Unlike `my-dsh-acp--project-root', never fall back to
`default-directory'; a missing project root means no history."
  (condition-case nil
      (my-dsh-acp--project-root 'no-fallback)
    (user-error nil)))

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

(defun my-dsh-acp--start (root &optional session-id)
  "Start a DSH ACP agent-shell for ROOT.
With SESSION-ID, resume that session instead of starting a new one."
  (setq session-id (and session-id
                         (not (string-empty-p (string-trim session-id)))
                         (string-trim session-id)))
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
        ;; An explicit session id must not go through the prompt/latest
        ;; strategy: a failed resume would otherwise fall into the
        ;; session picker instead of reporting the problem.
        (agent-shell-session-strategy
         (if session-id 'new my-desktop-dsh-acp-session-strategy)))
    (condition-case err
        (let ((buffer (agent-shell-start :config (my-dsh-acp--agent-config)
                                         :session-id session-id)))
          (when (and (buffer-live-p buffer) (fboundp 'persp-add-buffer))
            (persp-add-buffer buffer))
          (when (buffer-live-p buffer)
            (pop-to-buffer buffer))
          buffer)
      (error
       (user-error "DSH ACP failed to %s in %s: %s"
                   (if session-id "resume" "start")
                   root (error-message-string err))))))

;; ------------------------------------------------------------------
;; Session history: transcript front matter
;; ------------------------------------------------------------------

(defconst my-dsh-acp--transcript-prefix-limit 65536
  "Bytes read from a transcript when indexing its front matter.")

(defconst my-dsh-acp--session-title-width 80
  "Maximum width of a session title in the session list.")

(defun my-dsh-acp--transcripts-dir (root)
  "Return the agent-shell transcript directory under ROOT.
This follows agent-shell's default `.agent-shell/transcripts'
layout.  If you customize `agent-shell-transcript-file-path-function'
or `agent-shell-dot-subdir-function', update this helper too."
  (expand-file-name ".agent-shell/transcripts/"
                    (file-name-as-directory root)))

(defun my-dsh-acp--transcript-files (root)
  "Return the transcript files under ROOT, or nil."
  (let ((dir (my-dsh-acp--transcripts-dir root)))
    (when (file-directory-p dir)
      (directory-files dir t "\\`[0-9].*\\.md\\'"))))

(defun my-dsh-acp--transcript-frontmatter-region ()
  "Return the front matter bounds of the current buffer, or nil.
The region starts after the opening `---' line and ends at the
beginning of the closing `---' line."
  (save-excursion
    (goto-char (point-min))
    (when (looking-at "^---\r?$")
      (let ((beg (line-end-position)))
        (forward-line 1)
        (when (re-search-forward "^---\r?$" nil t)
          (cons beg (line-beginning-position)))))))

(defun my-dsh-acp--frontmatter-field (name)
  "Return the decoded front matter field NAME in the current buffer.
NAME is a string such as \"session_id\".  agent-shell writes values
with `json-encode-string', so decode quoted values as JSON instead
of assuming a UUID-safe character set."
  (save-excursion
    (goto-char (point-min))
    (when (re-search-forward
           (format "^%s:[ \t]*\\(.*\\)$" (regexp-quote name)) nil t)
      (let ((raw (string-trim (match-string 1))))
        (cond
         ((string-empty-p raw) nil)
         ((equal raw "null") nil)
         ((eq (aref raw 0) ?\")
          (condition-case nil
              (json-parse-string raw :object-type 'alist :array-type 'list)
            (error (string-trim raw "\""))))
         ((eq (aref raw 0) ?')
          (string-trim raw "'" "'"))
         (t raw))))))

(defun my-dsh-acp--transcript-frontmatter ()
  "Return the front matter of the current transcript as an alist."
  (when-let* ((region (my-dsh-acp--transcript-frontmatter-region)))
    (save-restriction
      (narrow-to-region (car region) (cdr region))
      (list (cons 'session-id (my-dsh-acp--frontmatter-field "session_id"))
            (cons 'started (my-dsh-acp--frontmatter-field "started"))
            (cons 'working-directory
                  (my-dsh-acp--frontmatter-field "working_directory"))))))

(defun my-dsh-acp--transcript-title ()
  "Return the first user message line of the current transcript.
Skip blank lines and code-fence lines, and strip the `> ' prefix
that agent-shell adds when replaying a restored user turn."
  (save-excursion
    (save-restriction
      (widen)
      (goto-char (point-min))
      (when-let* ((region (my-dsh-acp--transcript-frontmatter-region)))
        (goto-char (cdr region)))
      (when (re-search-forward "^## User (" nil t)
        (forward-line 1)
        (let (line)
          (while (and (not line) (not (eobp)))
            (let ((candidate
                   (string-trim
                    (buffer-substring-no-properties
                     (line-beginning-position) (line-end-position)))))
              (if (or (string-empty-p candidate)
                      (string-match-p "\\`\\(?:```+\\|~~~+\\)" candidate))
                  (forward-line 1)
                (setq line candidate))))
          (when line
            (setq line (replace-regexp-in-string "\\`>+[ \t]*" "" line))
            (truncate-string-to-width
             line my-dsh-acp--session-title-width nil nil "…")))))))

(defun my-dsh-acp--parse-transcript-buffer ()
  "Parse the current buffer as an agent-shell transcript.
Return a plist with :id, :started, :working-directory and :title,
or nil when the buffer has no usable front matter."
  (save-restriction
    (widen)
    (when-let* ((frontmatter (my-dsh-acp--transcript-frontmatter))
                (id (alist-get 'session-id frontmatter)))
      (when (and (stringp id) (not (string-empty-p id)))
        (let* ((started-raw (alist-get 'started frontmatter))
               (started (condition-case nil
                            (and started-raw (date-to-time started-raw))
                          (error nil))))
          (list :id id
                :started started
                :working-directory
                (alist-get 'working-directory frontmatter)
                :title (or (my-dsh-acp--transcript-title)
                           "(no message)")))))))

(defun my-dsh-acp--parse-transcript-file (file)
  "Parse transcript FILE and return a session plist, or nil."
  (condition-case nil
      (with-temp-buffer
        (insert-file-contents file nil 0 my-dsh-acp--transcript-prefix-limit)
        (when-let* ((session (my-dsh-acp--parse-transcript-buffer)))
          (plist-put session :file file)
          (unless (plist-get session :started)
            (plist-put session :started
                       (or (file-attribute-modification-time
                            (file-attributes file))
                           (seconds-to-time 0))))
          session))
    (error nil)))

(defun my-dsh-acp--merge-session (table session)
  "Merge SESSION into TABLE, keyed by session id.
Keep the earliest title and the latest start time, so a resumed
session stays one row and is sorted by its most recent shell."
  (let* ((id (plist-get session :id))
         (started (plist-get session :started))
         (existing (gethash id table)))
    (if (not existing)
        (puthash id
                 (list :id id
                       :title (plist-get session :title)
                       :first-started started
                       :last-started started
                       :file (plist-get session :file)
                       :working-directory
                       (plist-get session :working-directory))
                 table)
      (when (time-less-p started (plist-get existing :first-started))
        (plist-put existing :first-started started)
        (plist-put existing :title (plist-get session :title)))
      (when (time-less-p (plist-get existing :last-started) started)
        (plist-put existing :last-started started)
        (plist-put existing :file (plist-get session :file))
        (plist-put existing :working-directory
                   (plist-get session :working-directory))))))

(defun my-dsh-acp--sessions (root)
  "Return ROOT's sessions, newest first."
  (let ((table (make-hash-table :test #'equal)))
    (dolist (file (my-dsh-acp--transcript-files root))
      (when-let* ((session (my-dsh-acp--parse-transcript-file file)))
        (my-dsh-acp--merge-session table session)))
    (let (sessions)
      (maphash (lambda (_id session) (push session sessions)) table)
      (sort sessions
            (lambda (a b)
              (time-less-p (plist-get b :last-started)
                           (plist-get a :last-started)))))))

(defun my-dsh-acp--session-label (session)
  "Return the `date | id | title' list label for SESSION."
  (format "%s | %s | %s"
          (format-time-string "%Y-%m-%d %H:%M"
                              (plist-get session :last-started))
          (plist-get session :id)
          (plist-get session :title)))

(defun my-dsh-acp--clean-session-id (string)
  "Return STRING with transcript-header decoration and quoting removed.
Accept both a bare id and a pasted `session_id: \"...\"' line."
  (let ((id (string-trim (or string ""))))
    (when (string-match "\\`session_id:[ \t]*\\(.*\\)\\'" id)
      (setq id (string-trim (match-string 1 id))))
    (when (and (not (string-empty-p id))
               (eq (aref id 0) ?\"))
      (let ((parsed (condition-case nil
                        (json-parse-string id :object-type 'alist
                                           :array-type 'list)
                      (error nil))))
        (setq id (if (stringp parsed)
                     parsed
                   (string-trim id "\"")))))
    (string-trim (or id ""))))

(defun my-dsh-acp--looks-like-session-id-p (string)
  "Return non-nil when STRING is a plausible session id."
  (and (stringp string)
       (not (string-empty-p (string-trim string)))
       (not (string-match-p "[ \t\n\r]" (string-trim string)))))

(defun my-dsh-acp--session-id-at-point ()
  "Return a session id on the current line, or the symbol at point."
  (save-excursion
    (beginning-of-line)
    (cond
     ((looking-at "session_id:[ \t]*\\(.*\\)$")
      (let ((id (my-dsh-acp--clean-session-id (match-string 1))))
        (unless (or (string-empty-p id) (equal id "null"))
          id)))
     (t
      (let ((symbol (thing-at-point 'symbol t)))
        (when (and (my-dsh-acp--looks-like-session-id-p symbol)
                   (not (member symbol
                                '("session_id" "agent" "started"
                                  "working_directory" "model" "---"))))
          (string-trim symbol)))))))

(defun my-dsh-acp--read-session-id ()
  "Read a DSH session id, defaulting to one at point or in the kill ring."
  (let* ((at-point (my-dsh-acp--session-id-at-point))
         (killed (car kill-ring))
         (default (or at-point
                      (and (my-dsh-acp--looks-like-session-id-p killed)
                           (string-trim killed)))))
    (read-string "Resume DSH session ID: " default)))

(defun my-dsh-acp--resume (root session-id)
  "Resume SESSION-ID in ROOT, reusing the project's existing shell."
  (let ((shell (my-dsh-acp--project-shell root)))
    (cond
     ((and (buffer-live-p shell)
           (equal (ignore-errors
                    (agent-shell-session-id :shell-buffer shell))
                  session-id))
      (when (fboundp 'persp-add-buffer)
        (persp-add-buffer shell))
      (pop-to-buffer shell)
      (message "DSH ACP: focused session %s" session-id))
     ((buffer-live-p shell)
      (with-current-buffer shell
        (agent-shell-restart :session-id session-id))
      (let ((new-shell (my-dsh-acp--project-shell root)))
        (when (buffer-live-p new-shell)
          (when (fboundp 'persp-add-buffer)
            (persp-add-buffer new-shell))
          (pop-to-buffer new-shell)))
      (message "DSH ACP: resumed session %s" session-id))
     (t
      (my-dsh-acp--start root session-id)
      (message "DSH ACP: resumed session %s" session-id)))))

;;;###autoload
(defun my-dsh-acp-resume (session-id)
  "Resume a DSH ACP session by SESSION-ID."
  (interactive (list (my-dsh-acp--read-session-id)))
  (let ((session-id (my-dsh-acp--clean-session-id session-id)))
    (when (string-empty-p session-id)
      (user-error "DSH ACP: session ID cannot be empty"))
    (my-dsh-acp--resume (my-dsh-acp--project-root) session-id)))

;;;###autoload
(defun my-dsh-acp-list-sessions ()
  "List this project's DSH sessions and resume the chosen one."
  (interactive)
  (if-let* ((root (my-dsh-acp--history-root)))
      (let ((sessions (my-dsh-acp--sessions root)))
        (if (null sessions)
            (message "DSH ACP: no session history in %s" root)
          (let* ((choices (mapcar (lambda (session)
                                    (cons (my-dsh-acp--session-label session)
                                          session))
                                  sessions))
                 ;; Vertico's default sort would destroy the date order.
                 (completion-extra-properties
                  '(:display-sort-function identity))
                 (choice (completing-read "Resume DSH session: "
                                          choices nil t)))
            (when-let* ((session (cdr (assoc choice choices))))
              (my-dsh-acp--resume root (plist-get session :id))))))
    (message "DSH ACP: no project here; no session history")))

;;;###autoload
(defun my-dsh-acp-search-transcripts ()
  "Search this project's DSH transcripts and resume the chosen session."
  (interactive)
  (if-let* ((root (my-dsh-acp--history-root)))
      (let ((files (my-dsh-acp--transcript-files root)))
        (if (null files)
            (message "DSH ACP: no session history in %s" root)
          (let ((dir (my-dsh-acp--transcripts-dir root)))
            (unless (fboundp 'consult-ripgrep)
              (user-error "DSH ACP: `consult-ripgrep' is not available"))
            (consult-ripgrep dir)
            ;; `consult-ripgrep' jumps to the selected transcript before
            ;; it returns, so the current buffer is the match.
            (let* ((file (buffer-file-name))
                   (in-project (and file (file-in-directory-p file dir)))
                   (session (and in-project
                                 (my-dsh-acp--parse-transcript-buffer))))
              (cond
               ((not in-project)
                (message "DSH ACP: selection is not a project transcript"))
               ((not session)
                (message "DSH ACP: selected transcript has no session id"))
               (t
                (my-dsh-acp--resume root (plist-get session :id))))))))
    (message "DSH ACP: no project here; no session history")))

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
    ("o" "Resume session by ID" my-dsh-acp-resume :transient nil)
    ("l" "List project sessions" my-dsh-acp-list-sessions :transient nil)
    ("s" "Search project transcripts" my-dsh-acp-search-transcripts
     :transient nil)
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
