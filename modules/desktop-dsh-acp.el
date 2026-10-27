;;; desktop-dsh-acp.el --- DSH / Claude ACP agents through agent-shell -*- lexical-binding: t; -*-

;;; Commentary:
;; ACP bridge to the sem-dev-container harness images.
;;
;; Each harness is a launcher that runs the project container and speaks
;; ACP over stdio: `run-dsh --acp' (dsh --profile acp) and
;; `run-claude --acp' (claude-agent-acp).  agent-shell owns the client
;; and the chat UI; for it the harness is only a different command, see
;; `my-dsh-acp--harnesses'.
;;
;; One agent-shell buffer per project and harness.  Opening an agent
;; focuses the existing buffer instead of starting another ACP process;
;; a new shell always starts a new session (no agent-shell session
;; prompt).  Killing the buffer closes stdin and stops the container.
;; The project root is resolved through the project perspective first,
;; then project.el, then `default-directory'.
;;
;; `C-c a d r' / `o' ask which harness to run.  `k' / `R' / `n' / `f'
;; ask among the harnesses running in this project (no prompt when only
;; one runs).  `l' lists this project's sessions of all harnesses, and
;; `s' searches the project's transcripts with Consult.  Session
;; discovery reads the transcript front matter (one file per shell
;; start, same `session_id' after a resume; `agent' names the harness),
;; so it needs no ACP `session/list' capability.
;;
;; The web/dsh-emacs integration lives in desktop-dsh.el and is no
;; longer in this menu; it stays reachable through `C-c d' /
;; `M-x my-dsh-open'.

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
       (message "ACP: agent-shell install failed: %s"
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
(declare-function agent-shell-insert "agent-shell" t)
(declare-function agent-shell--buffer-files "agent-shell" t)
(declare-function agent-shell--get-files-context "agent-shell" t)
(declare-function agent-shell--project-files "agent-shell" t)
(declare-function agent-shell-deepseek-make-agent-config "agent-shell-deepseek")
(declare-function agent-shell-anthropic-make-claude-code-config
                  "agent-shell-anthropic")
(declare-function my-project-current-root "desktop-project")
(declare-function persp-add-buffer "perspective" (buffer-or-name))
(declare-function my-desktop--warn "desktop-init" (&rest args))

;; Mark the agent-shell options special before the first `let'-binding:
;; agent-shell itself is deferred, so its `defcustom' has not run yet.
(defvar agent-shell-session-strategy)
(defvar agent-shell-command-prefix)

(defun my-dsh-acp--require-agent-shell ()
  "Load agent-shell or report a clear installation error."
  (unless (require 'agent-shell nil t)
    (user-error "ACP: `agent-shell' is not installed; \
install it from MELPA")))

;; ------------------------------------------------------------------
;; Harnesses
;; ------------------------------------------------------------------

(defconst my-dsh-acp--harnesses
  '((dsh :name "DSH"
         :identifier dsh-acp
         :mode-line "DSH ACP"
         :command-var my-desktop-dsh-acp-command
         :options-var my-desktop-dsh-acp-default-config-options
         :base agent-shell-deepseek-make-agent-config
         :launcher "run-dsh")
    (claude :name "Claude"
            :identifier claude-acp
            :mode-line "Claude ACP"
            :command-var my-desktop-claude-acp-command
            :options-var my-desktop-claude-acp-default-config-options
            :base agent-shell-anthropic-make-claude-code-config
            :launcher "run-claude"))
  "ACP harnesses, as (KEY . PLIST).
:name is shown in prompts, :identifier and :mode-line tag the
agent-shell config (:mode-line also lands in the transcript's
`agent' field), :command-var / :options-var name the settings read at
call time, and :base returns the stock agent-shell config whose
welcome text, icon and session meta are reused.")

(defvar my-dsh-acp--last-harness 'dsh
  "Harness picked last; the default of the next harness prompt.")

(defun my-dsh-acp--harness-get (harness prop)
  "Return PROP of HARNESS from `my-dsh-acp--harnesses'."
  (plist-get (alist-get harness my-dsh-acp--harnesses) prop))

(defun my-dsh-acp--harness-name (harness)
  "Return the display name of HARNESS."
  (my-dsh-acp--harness-get harness :name))

(defun my-dsh-acp--harness-command (harness)
  "Return the ACP launch command of HARNESS."
  (symbol-value (my-dsh-acp--harness-get harness :command-var)))

(defun my-dsh-acp--harness-by-mode-line (mode-line)
  "Return the harness whose :mode-line is MODE-LINE, or nil."
  (car (seq-find (lambda (entry)
                   (equal (plist-get (cdr entry) :mode-line) mode-line))
                 my-dsh-acp--harnesses)))

(defun my-dsh-acp--buffer-harness (buffer)
  "Return the harness of agent-shell BUFFER, or nil."
  (when (buffer-live-p buffer)
    (let ((identifier (ignore-errors
                        (alist-get :identifier
                                   (agent-shell-get-config buffer)))))
      (car (seq-find (lambda (entry)
                       (eq (plist-get (cdr entry) :identifier) identifier))
                     my-dsh-acp--harnesses)))))

(defun my-dsh-acp--read-harness (prompt)
  "Read a harness with PROMPT, defaulting to the last one picked."
  (let* ((choices (mapcar (lambda (entry)
                            (cons (plist-get (cdr entry) :name) (car entry)))
                          my-dsh-acp--harnesses))
         (completion-extra-properties '(:display-sort-function identity))
         (choice (completing-read
                  prompt choices nil t nil nil
                  (my-dsh-acp--harness-name my-dsh-acp--last-harness)))
         (harness (cdr (assoc choice choices))))
    (unless harness
      (user-error "ACP: unknown harness %s" choice))
    (setq my-dsh-acp--last-harness harness)))

(defun my-dsh-acp--make-client (buffer harness)
  "Create the ACP client of HARNESS for BUFFER.
The launcher owns the container, project directory and environment,
so no environment variables are injected here."
  (unless buffer
    (error "Missing required argument: :buffer"))
  (my-dsh-acp--require-agent-shell)
  (let ((command (my-dsh-acp--harness-command harness))
        ;; The launcher already owns the container; a global command
        ;; prefix would wrap it a second time.
        (agent-shell-command-prefix nil))
    (agent-shell--make-acp-client
     :command (car command)
     :command-params (cdr command)
     :context-buffer buffer)))

(defun my-dsh-acp--agent-config (harness)
  "Return the agent-shell configuration for HARNESS.
It starts from the stock agent-shell config of that agent (welcome
art, icon and session meta), but gets its own identifier, buffer
name, prompt and client maker."
  (my-dsh-acp--require-agent-shell)
  (let* ((name (my-dsh-acp--harness-name harness))
         (prompt (concat name "> "))
         (options-var (my-dsh-acp--harness-get harness :options-var))
         (config (funcall (my-dsh-acp--harness-get harness :base))))
    (setf (alist-get :identifier config)
          (my-dsh-acp--harness-get harness :identifier)
          (alist-get :mode-line-name config)
          (my-dsh-acp--harness-get harness :mode-line)
          (alist-get :buffer-name config) name
          (alist-get :shell-prompt config) prompt
          (alist-get :shell-prompt-regexp config) (regexp-quote prompt)
          (alist-get :client-maker config)
          (lambda (buffer) (my-dsh-acp--make-client buffer harness))
          (alist-get :default-config-options config)
          (lambda () (symbol-value options-var))
          (alist-get :install-instructions config)
          (format "Install %s from sem-dev-container on `exec-path'; the
image must provide `%s --acp'."
                  (my-dsh-acp--harness-get harness :launcher)
                  (my-dsh-acp--harness-get harness :launcher)))
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
        (user-error "ACP: project directory does not exist: %s" root))
      root)))

(defun my-dsh-acp--history-root ()
  "Return the current project root for session history, or nil.
Unlike `my-dsh-acp--project-root', never fall back to
`default-directory'; a missing project root means no history."
  (condition-case nil
      (my-dsh-acp--project-root 'no-fallback)
    (user-error nil)))

(defun my-dsh-acp--project-shell (root harness)
  "Return the live HARNESS shell buffer for ROOT, or nil."
  (my-dsh-acp--require-agent-shell)
  (let ((shells
         (seq-filter
          (lambda (buffer)
            (eq (my-dsh-acp--buffer-harness buffer) harness))
          (agent-shell-project-buffers root))))
    (when (> (length shells) 1)
      (message "%s ACP: %d agent shells in %s; using %s"
               (my-dsh-acp--harness-name harness)
               (length shells) root (buffer-name (car shells))))
    (car shells)))

(defun my-dsh-acp--running-harnesses (root)
  "Return the harnesses with a live shell in ROOT, in table order."
  (seq-filter (lambda (harness) (my-dsh-acp--project-shell root harness))
              (mapcar #'car my-dsh-acp--harnesses)))

(defun my-dsh-acp--read-running-harness (root prompt)
  "Return a harness running in ROOT, asking with PROMPT if needed.
From inside one of this module's shells, use that shell's harness.
Do not prompt when only one harness runs in ROOT."
  (my-dsh-acp--require-agent-shell)
  (or (and (derived-mode-p 'agent-shell-mode)
           (my-dsh-acp--buffer-harness (current-buffer)))
      (let ((running (my-dsh-acp--running-harnesses root)))
        (pcase running
          ('nil (user-error "No ACP agent in this perspective"))
          (`(,only) only)
          (_
           (let* ((choices (mapcar (lambda (harness)
                                     (cons (my-dsh-acp--harness-name harness)
                                           harness))
                                   running))
                  (completion-extra-properties
                   '(:display-sort-function identity))
                  (choice (completing-read prompt choices nil t)))
             (cdr (assoc choice choices))))))))

(defun my-dsh-acp--focus (buffer)
  "Add BUFFER to the current perspective and show it."
  (when (fboundp 'persp-add-buffer)
    (persp-add-buffer buffer))
  (pop-to-buffer buffer))

(defun my-dsh-acp--start (root harness &optional session-id)
  "Start a HARNESS agent-shell for ROOT in a new session.
With SESSION-ID, resume that session instead."
  (setq session-id (and session-id
                         (not (string-empty-p (string-trim session-id)))
                         (string-trim session-id)))
  (let ((command (my-dsh-acp--harness-command harness))
        (var (my-dsh-acp--harness-get harness :command-var)))
    (unless (and (consp command) (stringp (car command)))
      (user-error "ACP: `%s' must be a non-empty list of strings" var))
    (unless (executable-find (car command))
      (user-error "ACP: `%s' not found on `exec-path'; \
install it from sem-dev-container"
                  (car command))))
  ;; Load agent-shell before let-binding its option: its `defcustom'
  ;; must establish the global default, and it does not override an
  ;; already-bound value.
  (my-dsh-acp--require-agent-shell)
  (let ((default-directory root)
        ;; Always `new': a fresh shell starts a fresh session, and an
        ;; explicit session id must not fall into the session picker
        ;; when the resume fails.
        (agent-shell-session-strategy 'new))
    (condition-case err
        (let ((buffer (agent-shell-start
                       :config (my-dsh-acp--agent-config harness)
                       :session-id session-id)))
          (when (buffer-live-p buffer)
            (my-dsh-acp--focus buffer))
          buffer)
      (error
       (user-error "%s ACP failed to %s in %s: %s"
                   (my-dsh-acp--harness-name harness)
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
      (list (cons 'agent (my-dsh-acp--frontmatter-field "agent"))
            (cons 'session-id (my-dsh-acp--frontmatter-field "session_id"))
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
Return a plist with :id, :harness, :started, :working-directory and
:title, or nil when the buffer has no usable front matter or its
`agent' is none of `my-dsh-acp--harnesses'."
  (save-restriction
    (widen)
    (when-let* ((frontmatter (my-dsh-acp--transcript-frontmatter))
                (id (alist-get 'session-id frontmatter))
                (harness (my-dsh-acp--harness-by-mode-line
                          (alist-get 'agent frontmatter))))
      (when (and (stringp id) (not (string-empty-p id)))
        (let* ((started-raw (alist-get 'started frontmatter))
               (started (condition-case nil
                            (and started-raw (date-to-time started-raw))
                          (error nil))))
          (list :id id
                :harness harness
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
                       :harness (plist-get session :harness)
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
  "Return the `date | harness | id | title' list label for SESSION."
  (format "%s | %-6s | %s | %s"
          (format-time-string "%Y-%m-%d %H:%M"
                              (plist-get session :last-started))
          (my-dsh-acp--harness-name (plist-get session :harness))
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

(defun my-dsh-acp--read-session-id (harness)
  "Read a HARNESS session id, defaulting to one at point or in the kill ring."
  (let* ((at-point (my-dsh-acp--session-id-at-point))
         (killed (car kill-ring))
         (default (or at-point
                      (and (my-dsh-acp--looks-like-session-id-p killed)
                           (string-trim killed)))))
    (read-string (format "Resume %s session ID: "
                         (my-dsh-acp--harness-name harness))
                 default)))


(defun my-dsh-acp--resume (root harness session-id)
  "Resume SESSION-ID of HARNESS in ROOT, reusing that harness's shell."
  (let ((shell (my-dsh-acp--project-shell root harness))
        (name (my-dsh-acp--harness-name harness)))
    (cond
     ((and (buffer-live-p shell)
           (equal (ignore-errors
                    (agent-shell-session-id :shell-buffer shell))
                  session-id))
      (my-dsh-acp--focus shell)
      (message "%s ACP: focused session %s" name session-id))
     ((buffer-live-p shell)
      (with-current-buffer shell
        (agent-shell-restart :session-id session-id))
      (let ((new-shell (my-dsh-acp--project-shell root harness)))
        (when (buffer-live-p new-shell)
          (my-dsh-acp--focus new-shell)))
      (message "%s ACP: resumed session %s" name session-id))
     (t
      (my-dsh-acp--start root harness session-id)
      (message "%s ACP: resumed session %s" name session-id)))))

;;;###autoload
(defun my-dsh-acp-resume (harness session-id)
  "Resume a HARNESS ACP session by SESSION-ID."
  (interactive
   (let ((harness (my-dsh-acp--read-harness "Resume with harness: ")))
     (list harness (my-dsh-acp--read-session-id harness))))
  (let ((session-id (my-dsh-acp--clean-session-id session-id)))
    (when (string-empty-p session-id)
      (user-error "ACP: session ID cannot be empty"))
    (my-dsh-acp--resume (my-dsh-acp--project-root) harness session-id)))

;;;###autoload
(defun my-dsh-acp-list-sessions ()
  "List this project's ACP sessions and resume the chosen one.
Sessions of all harnesses are listed; each resumes in its own."
  (interactive)
  (if-let* ((root (my-dsh-acp--history-root)))
      (let ((sessions (my-dsh-acp--sessions root)))
        (if (null sessions)
            (message "ACP: no session history in %s" root)
          (let* ((choices (mapcar (lambda (session)
                                    (cons (my-dsh-acp--session-label session)
                                          session))
                                  sessions))
                 ;; Vertico's default sort would destroy the date order.
                 (completion-extra-properties
                  '(:display-sort-function identity))
                 (choice (completing-read "Resume ACP session: "
                                          choices nil t)))
            (when-let* ((session (cdr (assoc choice choices))))
              (my-dsh-acp--resume root (plist-get session :harness)
                                  (plist-get session :id))))))
    (message "ACP: no project here; no session history")))

;;;###autoload
(defun my-dsh-acp-search-transcripts ()
  "Search this project's ACP transcripts and resume the chosen session."
  (interactive)
  (if-let* ((root (my-dsh-acp--history-root)))
      (let ((files (my-dsh-acp--transcript-files root)))
        (if (null files)
            (message "ACP: no session history in %s" root)
          (let ((dir (my-dsh-acp--transcripts-dir root)))
            (unless (fboundp 'consult-ripgrep)
              (user-error "ACP: `consult-ripgrep' is not available"))
            (consult-ripgrep dir)
            ;; `consult-ripgrep' jumps to the selected transcript before
            ;; it returns, so the current buffer is the match.
            (let* ((file (buffer-file-name))
                   (in-project (and file (file-in-directory-p file dir)))
                   (session (and in-project
                                 (my-dsh-acp--parse-transcript-buffer))))
              (cond
               ((not in-project)
                (message "ACP: selection is not a project transcript"))
               ((not session)
                (message "ACP: selected transcript has no session id \
or an unknown agent"))
               (t
                (my-dsh-acp--resume root (plist-get session :harness)
                                    (plist-get session :id))))))))
    (message "ACP: no project here; no session history")))

;;;###autoload
(defun my-dsh-acp-open (harness)
  "Open or focus the HARNESS ACP agent for this project.
Interactively, ask which harness to run.  A new shell always starts a
new session."
  (interactive (list (my-dsh-acp--read-harness "Run ACP agent: ")))
  (let* ((root (my-dsh-acp--project-root))
         (buffer (my-dsh-acp--project-shell root harness)))
    (if (buffer-live-p buffer)
        (progn
          (my-dsh-acp--focus buffer)
          (message "%s ACP: focused %s"
                   (my-dsh-acp--harness-name harness) (buffer-name buffer)))
      (my-dsh-acp--start root harness))))

;;;###autoload
(defun my-dsh-acp-kill (harness)
  "Stop the HARNESS ACP agent for this project.
Killing the agent-shell buffer closes the ACP client's stdin, which
stops the container."
  (interactive (list (my-dsh-acp--read-running-harness
                      (my-dsh-acp--project-root) "Kill ACP agent: ")))
  (let* ((root (my-dsh-acp--project-root))
         (buffer (my-dsh-acp--project-shell root harness))
         (name (my-dsh-acp--harness-name harness)))
    (unless (buffer-live-p buffer)
      (user-error "No %s ACP agent in this perspective" name))
    (kill-buffer buffer)
    (if (buffer-live-p buffer)
        (message "%s ACP: kill cancelled; agent still running" name)
      (message "%s ACP: stopped the agent in %s" name root))))

;;;###autoload
(defun my-dsh-acp-restart (harness)
  "Restart the HARNESS ACP agent for this project in a new session."
  (interactive (list (my-dsh-acp--read-running-harness
                      (my-dsh-acp--project-root) "Restart ACP agent: ")))
  (let* ((root (my-dsh-acp--project-root))
         (buffer (my-dsh-acp--project-shell root harness)))
    (when (buffer-live-p buffer)
      (kill-buffer buffer)
      (when (buffer-live-p buffer)
        (user-error "%s ACP: agent is still running"
                    (my-dsh-acp--harness-name harness))))
    (my-dsh-acp--start root harness)))

;;;###autoload
(defun my-dsh-acp-clear-buffer (harness)
  "Clear the HARNESS ACP agent buffer for this project.
From inside an ACP shell, clear that buffer."
  (interactive (list (my-dsh-acp--read-running-harness
                      (my-dsh-acp--project-root) "Clear ACP agent: ")))
  (my-dsh-acp--require-agent-shell)
  (let ((buffer
         (if (eq (and (derived-mode-p 'agent-shell-mode)
                      (my-dsh-acp--buffer-harness (current-buffer)))
                 harness)
             (current-buffer)
           (my-dsh-acp--project-shell (my-dsh-acp--project-root) harness))))
    (unless (buffer-live-p buffer)
      (user-error "No %s ACP agent in this perspective"
                  (my-dsh-acp--harness-name harness)))
    (with-current-buffer buffer
      (unless (fboundp 'agent-shell-clear-buffer)
        (user-error "ACP: `agent-shell-clear-buffer' is not available"))
      (agent-shell-clear-buffer)
      (message "ACP: cleared %s" (buffer-name buffer)))))

;;;###autoload
(defun my-dsh-acp-send-file (harness &optional prompt-for-file)
  "Send a file to the HARNESS ACP agent of this project.
Send the visited file or the Dired selection; from inside a shell, or
with prefix argument PROMPT-FOR-FILE, prompt for a project file."
  (interactive (list (my-dsh-acp--read-running-harness
                      (my-dsh-acp--project-root) "Send file to: ")
                     current-prefix-arg))
  (my-dsh-acp--require-agent-shell)
  (let ((shell (if (eq (and (derived-mode-p 'agent-shell-mode)
                            (my-dsh-acp--buffer-harness (current-buffer)))
                       harness)
                   (current-buffer)
                 (my-dsh-acp--project-shell (my-dsh-acp--project-root)
                                            harness))))
    (unless (buffer-live-p shell)
      (user-error "No %s ACP agent in this perspective"
                  (my-dsh-acp--harness-name harness)))
    ;; Same file choice as `agent-shell-send-file', but aimed at the
    ;; chosen harness's shell instead of agent-shell's own guess.
    (let ((files (if (or (derived-mode-p 'agent-shell-mode) prompt-for-file)
                     (list (completing-read "Send file: "
                                            (agent-shell--project-files)))
                   (or (agent-shell--buffer-files)
                       (when (buffer-file-name)
                         (list (buffer-file-name)))
                       (list (completing-read
                              "Send file: " (agent-shell--project-files)))))))
      (agent-shell-insert :text (agent-shell--get-files-context :files files)
                          :shell-buffer shell))))

;; ------------------------------------------------------------------
;; Menu: C-c a d
;; ------------------------------------------------------------------

(transient-define-prefix my-dsh-acp-menu ()
  "ACP agents menu: DSH or Claude, chosen per command."
  [["ACP agents (DSH / Claude)"
    ("r" "Run / focus agent" my-dsh-acp-open :transient nil)
    ("o" "Resume session by ID" my-dsh-acp-resume :transient nil)
    ("l" "List project sessions" my-dsh-acp-list-sessions :transient nil)
    ("s" "Search project transcripts" my-dsh-acp-search-transcripts
     :transient nil)
    ("k" "Kill agent" my-dsh-acp-kill :transient nil)
    ("R" "Restart agent" my-dsh-acp-restart :transient nil)
    ("n" "Clear agent buffer" my-dsh-acp-clear-buffer :transient nil)
    ("f" "Send file to agent" my-dsh-acp-send-file :transient nil)]])

(provide 'desktop-dsh-acp)
;;; desktop-dsh-acp.el ends here
