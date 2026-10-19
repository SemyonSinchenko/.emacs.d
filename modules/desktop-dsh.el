;;; desktop-dsh.el --- DeepSeek Harness (DSH) dev containers -*- lexical-binding: t; -*-

;;; Commentary:
;; Client for the sem-dev-container DSH stack (see its README.md).
;;
;; `run-dsh' (bins/run-dsh) starts the dev-dsh container in the current
;; folder and publishes the dsh web port on 127.0.0.1 in the range
;; 30780-30789 with a per-run launch token.  This module runs the
;; launcher as an Emacs subprocess and watches its log buffer for TWO
;; lines: the launcher banner `dsh web (host): http://127.0.0.1:PORT'
;; (the host-side port) and the server's `dsh web:
;; http://127.0.0.1:3080/?token=TOKEN' (container-side port + token).
;; The port on the server's line is only valid INSIDE the container --
;; dsh >= 0.2.0 prints no --public-url -- so the connect URL is
;; composed here as HOST-URL/?token=TOKEN; connecting to the verbatim
;; server line once probed a dead port and dropped dsh-emacs into its
;; CLI-install fallback.  Parallel sessions (different folders) are
;; supported: C-c d reconnects to a running one or starts a new one in
;; a chosen folder.
;;
;; Stopping: `k' in the log buffer or `my-dsh-stop' closes the
;; launcher's stdin, which the container wrapper treats like Ctrl-D
;; (graceful shutdown).  The container keeps its state in ~/.dsh, so
;; model/provider setup survives restarts.
;;
;; Requires the dsh-emacs package (installed below from GitHub) and
;; the `run-dsh' launcher on `exec-path'.

;;; Code:

(require 'desktop-config-defs)
(require 'project)

;; dsh-emacs is loaded lazily (its autoloads cover the commands); the
;; declarations keep the byte compiler quiet.
(declare-function dsh-emacs "dsh-emacs")
(declare-function dsh-emacs-open-web "dsh-emacs")
(declare-function dsh-emacs--server-invalidate-alive "dsh-emacs-server")
(declare-function dsh-emacs--server-auth-reset "dsh-emacs-server")
(declare-function my-desktop--warn "desktop-init")
(defvar dsh-emacs-base-url)
(defvar dsh-emacs-server-auto-start)

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

(defconst my-dsh--host-port-re
  "dsh web (host): \\(http://[0-9A-Za-z._-]+\\):\\([0-9]+\\)"
  "The launcher banner line: `dsh web (host): http://127.0.0.1:30783'.
Group 1 is the host (always a loopback address today), group 2 the
HOST-side published port.  This port is the only one reachable from
Emacs; the server's own `dsh web:' line carries the container-side
port (3080) which is dead on the host.")

(defconst my-dsh--token-re
  "dsh web: [^ ]*/\\?token=\\([A-Za-z0-9_-]+\\)"
  "The server's launch-token line: `dsh web: http://.../?token=TOKEN'.
Only the token is taken from here; the URL part is container-side.
The placeholder `.../?token=...' lines in run-dsh's banner never
match (token part is an ellipsis).")

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

(defun my-dsh--buffer-session ()
  "The session shown by the current log buffer, or nil."
  (cdr (seq-find (lambda (entry)
                   (eq (plist-get (cdr entry) :buffer) (current-buffer)))
                 my-dsh--sessions)))

(defun my-dsh-stop-buffer-session ()
  "Stop the DSH session whose log buffer is current."
  (interactive)
  (if-let* ((session (my-dsh--buffer-session)))
      (my-dsh-stop (plist-get session :dir))
    (user-error "No DSH session is bound to this buffer")))

(defun my-dsh-connect-buffer-session ()
  "Connect dsh-emacs to the session whose log buffer is current.
Re-scans the whole log, so it also works after the token appeared
while the startup notice already fired."
  (interactive)
  (if-let* ((session (my-dsh--buffer-session)))
      (my-dsh--connect session)
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
  "Search SESSION's log buffer tail for the host port and the token.
Composes the connect URL from the two independent lines: the
launcher banner gives the host-side port, the server's `dsh web:'
line gives the token.  Returns non-nil once both have been seen
and :url is stored.  Scanning covers the last 8 KiB, so lines
split across process chunks are still found; each part is captured
only once."
  (when (and (not (plist-get session :url))
             (buffer-live-p (plist-get session :buffer)))
    (with-current-buffer (plist-get session :buffer)
      (save-excursion
        (goto-char (max (point-min) (- (point-max) 8192)))
        (unless (plist-get session :host-url)
          (when (re-search-forward my-dsh--host-port-re nil t)
            (plist-put session :host-url (match-string 1))
            (plist-put session :port (match-string 2))))
        (unless (plist-get session :token)
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
        (when (and (plist-get session :host-url) (plist-get session :token))
          (plist-put
           session :url
           (format "%s:%s/?token=%s"
                   (plist-get session :host-url)
                   (plist-get session :port)
                   (plist-get session :token)))
          t)))))

(defun my-dsh--found-url (session)
  "Called once SESSION's token line appeared."
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
`my-dsh-open' again once `dsh web:' appears"
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
none: start `run-dsh' in DIR, wait for its `dsh web: ...' line and
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

(provide 'desktop-dsh)
;;; desktop-dsh.el ends here
