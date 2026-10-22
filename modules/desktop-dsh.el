;;; desktop-dsh.el --- DeepSeek Harness (DSH) dev containers -*- lexical-binding: t; -*-

;;; Commentary:
;; Client for the sem-dev-container DSH stack (see its README.md).
;;
;; `run-dsh' (bins/run-dsh) starts the dev-dsh container in the current
;; folder and publishes the dsh web port on 127.0.0.1 in the range
;; 30780-30789 with a per-run launch token.  This module runs the
;; launcher as an Emacs subprocess and watches its log buffer.  Once
;; the server is ready, run-dsh prints three host-side lines -- the
;; complete host URL (`http://127.0.0.1:PORT/?token=TOKEN'), the
;; published port, and the raw token -- and those are the primary
;; signal.  Older launchers are still understood via their banner
;; (`dsh web (host): ...') plus the server's container-side
;; `dsh web: .../?token=...' line; the container-side URL itself is
;; never connected to (its 3080 port is dead on the host).  Parallel
;; sessions (different folders) are supported: the `C-c a d' menu's
;; `r' reconnects to a running one or starts a new one in a chosen
;; folder; its `k' stops a session in the current perspective.
;;
;; Stopping: `k' in the log buffer, `my-dsh-stop', or the `k' entry
;; of the `C-c a d' menu closes the launcher's stdin, which the
;; container wrapper treats like Ctrl-D (graceful shutdown).  The
;; menu's `k' also kills the log buffer and only considers buffers
;; in the current perspective.  The container keeps its state in
;; ~/.dsh, so model/provider setup survives restarts.
;;
;; Requires the dsh-emacs package (installed below from GitHub) and
;; the `run-dsh' launcher on `exec-path'.

;;; Code:

(require 'desktop-config-defs)
(require 'project)
(require 'transient)

;; dsh-emacs is loaded lazily (its autoloads cover the commands); the
;; declarations keep the byte compiler quiet.
(declare-function dsh-emacs "dsh-emacs")
(declare-function dsh-emacs-open-web "dsh-emacs")
(declare-function dsh-emacs--server-invalidate-alive "dsh-emacs-server")
(declare-function dsh-emacs--server-auth-reset "dsh-emacs-server")
(declare-function persp-is-current-buffer "perspective"
                  (buffer &optional include-global))
(declare-function my-desktop--warn "desktop-init")
(defvar dsh-emacs-base-url)
(defvar dsh-emacs-server-auto-start)
(defvar persp-mode)

;; dsh-emacs is GitHub-only (not on MELPA): install it as a VC
;; package on first load (package-vc needs no use-package :vc, so
;; this also works on Emacs 29).  A failed install is warned about;
;; the commands below then degrade to clear errors instead of
;; breaking startup.
(unless (locate-library "dsh-emacs")
  (require 'package-vc)
  (condition-case err
      (package-vc-install
       '(dsh-emacs :url "https://github.com/vritser/dsh-emacs"))
    (error (my-desktop--warn "dsh-emacs install failed: %s"
                             (error-message-string err)))))

;; Compatibility shim for the installed dsh-emacs 0.5.0 on Emacs <31.
;; Its `dsh-emacs-mode' assumes `yank-media-preferred-types' exists
;; whenever `yank-media-handler' is autoloaded, but that variable is
;; Emacs 31+: opening a session on Emacs 30 died with
;;   (void-variable yank-media-preferred-types)
;; Load yank-media first so a real Emacs 31 default is never shadowed;
;; where the library has no such variable, bind nil so dsh-emacs'
;; append is harmless (stock `yank-media' ignores the variable there
;; anyway).  Upstream dsh-emacs HEAD guards this with `boundp'; drop the
;; shim once the installed package catches up.
(defvar yank-media-preferred-types)
(when (require 'yank-media nil t)
  (unless (boundp 'yank-media-preferred-types)
    (setq yank-media-preferred-types nil)))

(require 'dsh-emacs nil t)
;; The server lives in a container and is managed by run-dsh; never
;; let dsh-emacs spawn a host dsh behind it.  (Set before the package
;; loads its defcustoms: defcustom never overrides a bound value.)
(setq dsh-emacs-server-auto-start nil)

;; ------------------------------------------------------------------
;; Settings (schema in desktop-config-defs.el)
;; ------------------------------------------------------------------
;; my-desktop-dsh-command     launcher to run (run-dsh)
;; my-desktop-dsh-wait-seconds startup notice timeout (first run is slow)

;; ------------------------------------------------------------------
;; Session registry
;; ------------------------------------------------------------------

(defvar my-dsh--sessions nil
  "Running and recently started DSH sessions.
Alist (DIR . PLIST) with PLIST keys :dir :proc :buffer :url :port
:token :timer.  DIR is the expanded workspace directory; the entry
is removed when the launcher process exits.")

(defconst my-dsh--ready-re
  (concat "^\\(https?://[^/[:space:]]+/\\?token=[^[:space:]]+\\)"
          "[[:space:]]*\n"
          "\\([0-9]+\\)[[:space:]]*\n"
          "\\([^[:space:]]+\\)[[:space:]]*\n")
  "The three-line ready block run-dsh prints after the server URL.
Line 1 is the complete host URL (host port + token), line 2 the
published host port, line 3 the raw token.  Every line must be
newline-terminated so a chunk split mid-line cannot compose a URL
from a truncated token.")

(defconst my-dsh--host-port-re
  "^dsh web (host): \\(http://[0-9A-Za-z._-]+\\):\\([0-9]+\\)"
  "The launcher banner line: `dsh web (host): http://127.0.0.1:30783'.
Group 1 is the host (always a loopback address today), group 2 the
HOST-side published port.  This port is the only one reachable from
Emacs; the server's own `dsh web:' line carries the container-side
port (3080) which is dead on the host.  Used only as a fallback for
launchers that predate the ready block.")

(defconst my-dsh--token-re
  "^dsh web: [^ ]*/\\?token=\\([^[:space:]]+\\)[[:space:]]*\n"
  "The server's launch-token line: `dsh web: http://.../?token=TOKEN'.
Only the token is taken from here; the URL part is container-side.
The line must be newline-terminated, so a chunk split inside the
token never stores a truncated value.  Used only as a fallback for
launchers that predate the ready block.")

(defun my-dsh--strip-ansi (text)
  "Remove ANSI CSI escape sequences from chunk TEXT."
  (replace-regexp-in-string "\e\\[[0-9;?]*[A-Za-z]" "" text))

(defun my-dsh--session-live-p (session)
  "Non-nil when SESSION's launcher process is still running."
  (and session (process-live-p (plist-get session :proc))))

;; ------------------------------------------------------------------
;; Log buffer
;; ------------------------------------------------------------------

(defvar-keymap my-dsh-log-mode-map
  :parent special-mode-map
  "k" #'my-dsh-stop-buffer-session
  "g" #'my-dsh-connect-buffer-session)

(define-derived-mode my-dsh-log-mode special-mode "DSH-log"
  "Log of a DSH container launcher (run-dsh).
\\{my-dsh-log-mode-map}")

(defun my-dsh--session-for-buffer (buffer)
  "Return the DSH session whose log buffer is BUFFER, or nil."
  (cdr (seq-find (lambda (entry)
                   (eq (plist-get (cdr entry) :buffer) buffer))
                 my-dsh--sessions)))

(defun my-dsh--buffer-session ()
  "The session shown by the current log buffer, or nil."
  (my-dsh--session-for-buffer (current-buffer)))

(defun my-dsh-stop-buffer-session ()
  "Stop the DSH session whose log buffer is current."
  (interactive)
  (if-let* ((session (my-dsh--buffer-session)))
      (my-dsh-stop (plist-get session :dir))
    (user-error "No DSH session is bound to this buffer")))

(defun my-dsh-connect-buffer-session ()
  "Connect dsh-emacs to the session whose log buffer is current.
Re-scans the whole log, so it also works after the URL appeared
while the startup notice already fired."
  (interactive)
  (if-let* ((session (my-dsh--buffer-session)))
      (progn
        (my-dsh--scan-url session)
        (my-dsh--connect session))
    (user-error "No DSH session is bound to this buffer")))

;; ------------------------------------------------------------------
;; connect: point dsh-emacs at the running container
;; ------------------------------------------------------------------

(defun my-dsh--connect (session)
  "Point dsh-emacs at SESSION's URL and open its session list.
The URL carries the launch token, which dsh-emacs reads itself
from `dsh-emacs-base-url' (see `dsh-emacs--server-auth-token-from-url')."
  (unless (require 'dsh-emacs nil t)
    (user-error "dsh-emacs is not installed; check the *Warnings* buffer"))
  (unless (plist-get session :url)
    (user-error "DSH in %s has not printed its URL yet; see `%s'"
                (plist-get session :dir)
                (buffer-name (plist-get session :buffer))))
  (setq dsh-emacs-base-url (plist-get session :url))
  ;; Switching containers invalidates the probe cache and any auth
  ;; cookie minted for the previous server (dsh-emacs re-mints from
  ;; the new URL's token on demand; internal but stable API).
  (when (fboundp 'dsh-emacs--server-invalidate-alive)
    (dsh-emacs--server-invalidate-alive))
  (when (fboundp 'dsh-emacs--server-auth-reset)
    (dsh-emacs--server-auth-reset))
  ;; Guard against the CLI-install fallback: with the server in a
  ;; container, `dsh-emacs-server-start' (which `dsh-emacs-list-sessions'
  ;; calls regardless of `dsh-emacs-server-auto-start') would otherwise
  ;; try to spawn -- and prompt to npm-install -- a HOST dsh here.
  (unless (and (fboundp 'dsh-emacs--server-alive-p)
               (dsh-emacs--server-alive-p))
    (user-error "DSH in %s is not reachable at %s (probe failed); \
check the container and the host port, see `%s'"
                (plist-get session :dir)
                (plist-get session :url)
                (buffer-name (plist-get session :buffer))))
  (message "DSH: connecting to %s" (plist-get session :url))
  (dsh-emacs))

(defun my-dsh-open-web ()
  "Open the current DSH session's web UI in a browser.
Use it for the first provider/model setup (Settings -> Models)."
  (interactive)
  (unless (require 'dsh-emacs nil t)
    (user-error "dsh-emacs is not installed; check the *Warnings* buffer"))
  (dsh-emacs-open-web))

;; ------------------------------------------------------------------
;; watcher: extract URL + token from the launcher output
;; ------------------------------------------------------------------

(defun my-dsh--scan-url (session)
  "Search SESSION's log buffer for a ready-to-use host URL.
run-dsh prints a three-line block (complete URL, port, token) once
the server is up; that is the primary signal.  Older launchers only
print the banner and the server's `dsh web: ...?token=' line, so
those are still parsed as a fallback.  Returns non-nil once :url is
stored.  Scanning covers the last 8 KiB, so lines split across
process chunks are still found."
  (when (and (not (plist-get session :url))
             (buffer-live-p (plist-get session :buffer)))
    (with-current-buffer (plist-get session :buffer)
      (save-excursion
        (let ((start (max (point-min) (- (point-max) 8192))))
          (goto-char start)
          (cond
           ;; New run-dsh: URL, port and token as three complete lines.
           ((re-search-forward my-dsh--ready-re nil t)
            (let ((url (match-string 1)))
              (plist-put session :url url)
              (plist-put session :port (match-string 2))
              (plist-put session :token (match-string 3))
              (plist-put session :host-url
                         (replace-regexp-in-string
                          "/\\?token=.*\\'" "" url)))
            t)
           (t
            ;; Fallback for older run-dsh: compose the URL from the
            ;; banner's host port and the server line's token.  Reset
            ;; point before each search: a failed `re-search-forward'
            ;; leaves it at the search limit.
            (unless (plist-get session :host-url)
              (goto-char start)
              (when (re-search-forward my-dsh--host-port-re nil t)
                (plist-put session :host-url (match-string 1))
                (plist-put session :port (match-string 2))))
            (unless (plist-get session :token)
              (goto-char start)
              (when (re-search-forward my-dsh--token-re nil t)
                (plist-put session :token (match-string 1))))
            ;; A cold start can emit many KiB of pnpm output between the
            ;; launcher banner and the token line, pushing the banner out
            ;; of the tail window; re-scan the whole buffer for it once the
            ;; token is in (cheap: at most until the banner is found).
            (when (and (not (plist-get session :host-url))
                       (plist-get session :token))
              (goto-char (point-min))
              (when (re-search-forward my-dsh--host-port-re nil t)
                (plist-put session :host-url (match-string 1))
                (plist-put session :port (match-string 2))))
            (when (and (plist-get session :host-url)
                       (plist-get session :token))
              (plist-put
               session :url
               (format "%s:%s/?token=%s"
                       (plist-get session :host-url)
                       (plist-get session :port)
                       (plist-get session :token)))
              t))))))))

(defun my-dsh--found-url (session)
  "Called once SESSION's host URL (or token line) appeared."
  (my-dsh--cancel-timer session)
  ;; Never run dsh-emacs RPCs inside the process filter: hand the
  ;; connect to a short timer (skipped when the session died meanwhile).
  (run-with-timer 0.3 nil
                  (lambda ()
                    (when (my-dsh--session-live-p session)
                      (message "DSH in %s is ready on port %s; opening dsh-emacs"
                               (plist-get session :dir)
                               (plist-get session :port))
                      (my-dsh--connect session)))))

(defun my-dsh--filter (proc chunk)
  "Log CHUNK from the run-dsh PROC and watch for the token URL."
  (let ((session (process-get proc 'my-dsh-session)))
    (when (and session (buffer-live-p (plist-get session :buffer)))
      (with-current-buffer (plist-get session :buffer)
        (let ((inhibit-read-only t)
              (follow (= (point) (process-mark proc))))
          (save-excursion
            (goto-char (process-mark proc))
            (insert (my-dsh--strip-ansi chunk))
            (set-marker (process-mark proc) (point)))
          (when follow
            (goto-char (process-mark proc))))
        (when (my-dsh--scan-url session)
          (my-dsh--found-url session))))))

(defun my-dsh--sentinel (proc _event)
  "Forget PROC's session when the launcher exits."
  (let ((session (process-get proc 'my-dsh-session)))
    (when session
      (my-dsh--cancel-timer session)
      (setq my-dsh--sessions
            (assoc-delete-all (plist-get session :dir) my-dsh--sessions))
      (message "DSH session in %s stopped" (plist-get session :dir)))))

(defun my-dsh--cancel-timer (session)
  "Cancel SESSION's pending startup-notice timer, if any."
  (when-let* ((timer (plist-get session :timer)))
    (cancel-timer timer)
    (plist-put session :timer nil)))

(defun my-dsh--startup-notice (session)
  "Fire when SESSION still has no URL after the grace period."
  (plist-put session :timer nil)
  (when (and (my-dsh--session-live-p session)
             (not (plist-get session :url)))
    (message
     "DSH in %s is still starting after %ds (first run initializes the \
dsh profile and can take minutes) -- watch `%s', press g there or run \
`my-dsh-open' again once run-dsh prints its host URL"
     (plist-get session :dir) my-desktop-dsh-wait-seconds
     (buffer-name (plist-get session :buffer)))))

;; ------------------------------------------------------------------
;; start / stop / open
;; ------------------------------------------------------------------

(defun my-dsh--start (dir)
  "Start run-dsh in DIR and watch its log buffer."
  (unless (executable-find my-desktop-dsh-command)
    (user-error "`%s' not found on `exec-path'; symlink \
sem-dev-container/bins/run-dsh into ~/.local/bin"
                my-desktop-dsh-command))
  (let* ((name (file-name-nondirectory (directory-file-name dir)))
         (buf (get-buffer-create (format "*dsh:%s*" name)))
         (session (list :dir dir :buffer buf :url nil :host-url nil
                        :port nil :token nil :proc nil :timer nil))
         proc)
    (with-current-buffer buf
      (let ((inhibit-read-only t)) (erase-buffer))
      (my-dsh-log-mode))
    ;; run-dsh resolves the workspace from its cwd, so the process
    ;; must run with default-directory = DIR.
    (setq proc
          (make-process
           :name (format "dsh-%s" name)
           :buffer buf
           :command (list my-desktop-dsh-command)
           :coding 'utf-8-unix
           :connection-type 'pipe ; stdin pipe: EOF stops the harness
           :filter #'my-dsh--filter
           :sentinel #'my-dsh--sentinel))
    (process-put proc 'my-dsh-session session)
    (plist-put session :proc proc)
    (plist-put session :timer
               (run-with-timer my-desktop-dsh-wait-seconds nil
                               #'my-dsh--startup-notice session))
    (push (cons dir session) my-dsh--sessions)
    (message "Starting DSH in %s (run-dsh picks a free port; watch `%s')"
             dir (buffer-name buf))
    (pop-to-buffer buf)
    session))

(defun my-dsh-stop (dir)
  "Gracefully stop the DSH session running in DIR.
Closes the launcher's stdin; the container wrapper treats that
like Ctrl-D and shuts the harness down.  If it is still alive
after 20s, the launcher process is killed (the container itself
may then need `podman rm -f')."
  (interactive
   (let ((running (mapcar #'car my-dsh--sessions)))
     (unless running
       (user-error "No running DSH sessions"))
     (list (if (cdr running)
               (completing-read "Stop DSH session in: " running nil t)
             (car running)))))
  (setq dir (directory-file-name (expand-file-name dir)))
  (let ((session (alist-get dir my-dsh--sessions nil nil #'equal)))
    (unless (my-dsh--session-live-p session)
      (user-error "No running DSH session in %s" dir))
    (message "Stopping DSH in %s..." dir)
    (process-send-eof (plist-get session :proc))
    (run-with-timer 20 nil
                    (lambda ()
                      (when (my-dsh--session-live-p session)
                        (message
                         "DSH in %s ignores stdin EOF; killing the launcher \
(the container may need `podman rm -f')"
                         dir)
                        (delete-process (plist-get session :proc)))))))

(defun my-dsh-open (dir)
  "Open dsh-emacs on a DSH session for DIR.
With a session already running in DIR: connect to it (switching
`dsh-emacs-base-url' there).  With a starting one: report.  With
none: start `run-dsh' in DIR, wait for its ready URL block and
connect.  Several sessions in different folders can run in
parallel; each container gets its own host port."
  (interactive
   (progn
     (require 'project)
     (list (read-directory-name
            "DSH workspace: "
            (or (when-let* ((proj (project-current)))
                  (project-root proj))
                default-directory)))))
  (setq dir (directory-file-name (expand-file-name dir)))
  (let ((session (alist-get dir my-dsh--sessions nil nil #'equal)))
    (cond
     ((and (my-dsh--session-live-p session) (plist-get session :url))
      (my-dsh--connect session))
     ((my-dsh--session-live-p session)
      (user-error "DSH in %s is still starting; watch `%s'"
                  dir (buffer-name (plist-get session :buffer))))
     (t
      (when session
        (setq my-dsh--sessions
              (assoc-delete-all dir my-dsh--sessions)))
      (my-dsh--start dir)))))

;; ------------------------------------------------------------------
;; kill: only sessions shown in the current workspace
;; ------------------------------------------------------------------

(defun my-dsh--in-current-workspace-p (buffer)
  "Non-nil when BUFFER belongs to the current perspective.
With perspective mode off there is only one workspace, so every
buffer qualifies."
  (if (and (bound-and-true-p persp-mode)
           (fboundp 'persp-is-current-buffer))
      (persp-is-current-buffer buffer)
    t))

(defun my-dsh--current-log-buffers ()
  "Return the DSH log buffers of the current perspective.
Sorted by buffer name so the completion menu is stable."
  (sort (seq-filter
         (lambda (buffer)
           (and (buffer-live-p buffer)
                (with-current-buffer buffer
                  (derived-mode-p 'my-dsh-log-mode))
                (my-dsh--in-current-workspace-p buffer)))
         (buffer-list))
        (lambda (a b)
          (string< (buffer-name a) (buffer-name b)))))

(defun my-dsh--kill-label (buffer)
  "Completion label for killing DSH log BUFFER."
  (let ((session (my-dsh--session-for-buffer buffer)))
    (format "%s [%s]%s"
            (buffer-name buffer)
            (cond ((null session) "stopped")
                  ((plist-get session :port))
                  (t "starting"))
            (if-let* ((dir (plist-get session :dir)))
                (format " %s" (abbreviate-file-name dir))
              ""))))

(defun my-dsh-kill ()
  "Kill a DSH session in the current workspace.
Only DSH log buffers of the current perspective are considered.
With one buffer, stop its launcher and kill the buffer without
prompting; with several, choose one with completion."
  (interactive)
  (let ((buffers (my-dsh--current-log-buffers)))
    (unless buffers
      (user-error "No DSH log buffer in this workspace"))
    (let* ((buffer
            (if (= (length buffers) 1)
                (car buffers)
                (let ((choices
                       (mapcar (lambda (buf)
                                 (cons (my-dsh--kill-label buf) buf))
                               buffers)))
                  (or (cdr (assoc (completing-read "Kill DSH: "
                                                  choices nil t)
                                  choices))
                      (user-error "No DSH session selected")))))
           (label (my-dsh--kill-label buffer))
           (session (my-dsh--session-for-buffer buffer)))
      (when session
        (my-dsh--cancel-timer session)
        (if (my-dsh--session-live-p session)
            (my-dsh-stop (plist-get session :dir))
          (message "DSH: removing stale log buffer %s" label)))
      (when (buffer-live-p buffer)
        (with-current-buffer buffer
          (set-buffer-modified-p nil))
        ;; The launcher process is attached to the log buffer; the
        ;; buffer is being removed deliberately, so do not ask about
        ;; the still-running process or unsaved log text.
        (let ((kill-buffer-query-functions nil))
          (kill-buffer buffer))
        (message "DSH: killed %s" label)))))

;; ------------------------------------------------------------------
;; menu: C-c a d
;; ------------------------------------------------------------------

(transient-define-prefix my-dsh-menu ()
  "DSH container menu."
  [["DSH"
    ("r" "Run / reconnect" my-dsh-open :transient nil)
    ("k" "Kill in this workspace" my-dsh-kill :transient nil)]])

(provide 'desktop-dsh)
;;; desktop-dsh.el ends here
