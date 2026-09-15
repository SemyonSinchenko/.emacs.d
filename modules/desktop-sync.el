;;; desktop-sync.el --- rclone sync jobs and the C-c m menu -*- lexical-binding: t; -*-

;;; Commentary:
;; Transient menu for the configured rclone jobs (semsync & co).
;; Jobs run asynchronously into *sync:<name>* buffers.
;;
;; Back-sync (`my-sync-backsync', menu key "b"): pushes the user-facing
;; files in `my-desktop-backsync-files' back to the remote, but never
;; over a server state we have not seen.  Per file three values are
;; compared: S (server state now), B (baseline: server state at the
;; last successful exchange, one SQLite row per file) and L (local).
;;
;;   S == B, L == B    no-op
;;   S == B, L != B    push (the normal case)
;;   S != B, S == L    refresh baseline, no write
;;   S != B, L == B    fast-forward: server-only change, copy down
;;   otherwise         conflict: loud warning + resolution session
;;                     (diff3 -m + smerge-mode; accept = CAS push)
;;
;; The baseline row also keeps the base content (base64 TEXT, since
;; Emacs sqlite decodes text on read and would mangle raw bytes) so a
;; conflict can be merged 3-way.
;; See openspec/changes/org-backsync/design.md.

;;; Code:

(require 'json)
(require 'cl-lib)
(require 'transient)
(require 'desktop-config-defs)
(require 'desktop-core)

;; Defined in desktop-init.el, loaded before the modules.
(declare-function my-desktop--warn "desktop-init.el"
                  "Print a non-fatal warning." (format &rest args))

;;;; Jobs

(defun my-sync--jobs ()
  "Configured sync jobs as an alist of (NAME . JOB-PLIST)."
  (mapcar (lambda (job)
            (cons (plist-get job :name) job))
          (append my-desktop-sync-jobs nil)))

(defun my-sync--buffer (name)
  (get-buffer-create (format "*sync:%s*" name)))

(defun my-sync--run (job &optional dry-run)
  "Run rclone for JOB asynchronously.  DRY-RUN adds -n."
  (unless (my-desktop--require-bin "rclone" "sync jobs")
    (user-error "rclone not found"))
  (let* ((name (plist-get job :name))
         (buffer (my-sync--buffer name))
         (args (append (list "sync"
                             (plist-get job :remote)
                             (my-desktop--expand (plist-get job :local)))
                       (append (plist-get job :args) nil)
                       (when dry-run '("-n")))))
    (with-current-buffer buffer
      (erase-buffer)
      (insert (format "$ rclone %s\n\n" (string-join args " "))))
    (message "[desktop] rclone %s %s..." (if dry-run "(dry-run)" name)
             (car (last args 2)))
    (set-process-sentinel
     (apply #'start-process (format "rclone-%s" name) buffer
            "rclone" args)
     (lambda (proc _event)
       (let ((status (process-status proc)))
         (message "[desktop] rclone %s: %s"
                  (process-name proc)
                  (symbol-name status))
         (when (and (eq status 'exit)
                    (zerop (process-exit-status proc))
                    (not dry-run))
           (let ((hook (plist-get job :on-success)))
             (when hook
               (condition-case err
                   (funcall hook)
                 (error (my-desktop--warn
                         "sync post-hook after %s: %s"
                         name (error-message-string err))))))))))))

(defun my-sync-run (name)
  "Run sync job NAME (see `my-desktop-sync-jobs')."
  (interactive
   (list (completing-read "Sync job: " (my-sync--jobs) nil t)))
  (my-sync--run (cdr (assoc name (my-sync--jobs)))))

(defun my-sync-dry-run (name)
  "Dry-run sync job NAME."
  (interactive
   (list (completing-read "Dry-run job: " (my-sync--jobs) nil t)))
  (my-sync--run (cdr (assoc name (my-sync--jobs))) 'dry-run))

(defun my-sync-open-log (name)
  "Open the log buffer of job NAME (or \"backsync\")."
  (interactive
   (list (completing-read "Log of job: "
                          (append (mapcar #'car (my-sync--jobs))
                                  '("backsync"))
                          nil t)))
  (pop-to-buffer (my-sync--buffer name)))

(defun my-sync-kill ()
  "Kill running rclone processes started by this module."
  (interactive)
  (let ((n 0))
    (dolist (proc (process-list))
      (when (string-prefix-p "rclone-" (process-name proc))
        (delete-process proc)
        (setq n (1+ n))))
    (message "[desktop] killed %d rclone process(es)" n)))

;;;; Back-sync helpers: paths, bytes, hashes

(defun my-sync--remote-root ()
  "Remote root path corresponding to `my-desktop-org-dir'."
  (let* ((org (directory-file-name
               (my-desktop--expand my-desktop-org-dir)))
         (job (cl-find-if
               (lambda (job)
                 (let ((local (my-desktop--expand
                               (plist-get job :local))))
                   (and local
                        (string= (directory-file-name local) org))))
               (append my-desktop-sync-jobs nil))))
    (or (and job (plist-get job :remote))
        (user-error "No sync job pulls into %s" my-desktop-org-dir))))

(defun my-sync--remote-path (remote file)
  "Remote path of FILE under REMOTE root."
  (concat (if (string-suffix-p "/" remote)
              (substring remote 0 -1)
            remote)
          "/" file))

(defun my-sync--org-path (file)
  "Local absolute path of back-sync FILE in the org dir."
  (expand-file-name file (my-desktop--expand my-desktop-org-dir)))

(defun my-sync--file-bytes (path)
  "Raw (unibyte) contents of PATH, or nil when it does not exist."
  (when (and path (file-exists-p path))
    (with-temp-buffer
      (insert-file-contents-literally path)
      (buffer-string))))

(defun my-sync--hash-bytes (bytes hash-type)
  "Hex digest of unibyte BYTES with rclone HASH-TYPE, or nil."
  (let ((algo (pcase hash-type
                ("MD5" 'md5)
                ("SHA-1" 'sha1)
                ("SHA-256" 'sha256)
                ("SHA-512" 'sha512))))
    (when algo
      (with-temp-buffer
        (set-buffer-multibyte nil)
        (insert bytes)
        (secure-hash algo (current-buffer))))))

(defun my-sync--hash-file (path hash-type)
  "Hex digest of file PATH with rclone HASH-TYPE, or nil."
  (my-sync--hash-bytes (my-sync--file-bytes path) hash-type))

;;;; Back-sync: SQLite baseline

(defconst my-sync--sqlite-ok
  (and (fboundp 'sqlite-available-p) (sqlite-available-p))
  "Non-nil when this Emacs has built-in SQLite support.")

(defconst my-sync--schema-sql
  ["CREATE TABLE IF NOT EXISTS baseline (
      file TEXT PRIMARY KEY,
      server_hash TEXT,
      hash_type TEXT,
      size INTEGER,
      server_mtime TEXT,
      content_b64 TEXT,
      updated_at TEXT,
      source TEXT)"
   "CREATE TABLE IF NOT EXISTS decision_log (
      ts TEXT, file TEXT, event TEXT,
      s_hash TEXT, b_hash TEXT, l_hash TEXT, detail TEXT)"]
  "Baseline schema: one row per file.  server_* fields and
content_b64 describe the SERVER state at the last successful
exchange (content is base64 TEXT because Emacs sqlite decodes text
values on read, which would mangle raw bytes).")

(defconst my-sync--upsert-sql
  "INSERT INTO baseline (file, server_hash, hash_type, size,
     server_mtime, content_b64, updated_at, source)
   VALUES (?, ?, ?, ?, ?, ?, ?, ?)
   ON CONFLICT(file) DO UPDATE SET
     server_hash=excluded.server_hash,
     hash_type=excluded.hash_type,
     size=excluded.size,
     server_mtime=excluded.server_mtime,
     content_b64=excluded.content_b64,
     updated_at=excluded.updated_at,
     source=excluded.source")

(defun my-sync--db-open ()
  "Open the baseline database with the schema in place.
The caller must close it (`sqlite-close'), usually via
`my-sync--with-db'."
  (unless my-sync--sqlite-ok
    (user-error "This Emacs has no SQLite support; back-sync unavailable"))
  (let* ((file (my-desktop--expand my-desktop-backsync-db-file))
         (dir (file-name-directory file)))
    (when (and dir (not (file-exists-p dir)))
      (make-directory dir t))
    (let ((db (sqlite-open file)))
      (sqlite-execute db "PRAGMA journal_mode=WAL")
      (sqlite-execute db "PRAGMA busy_timeout=3000")
      (cl-loop for sql across my-sync--schema-sql
               do (sqlite-execute db sql))
      db)))

(defmacro my-sync--with-db (db &rest body)
  "Run BODY with DB bound to an open baseline database."
  (declare (indent 1))
  `(let ((,db (my-sync--db-open)))
     (unwind-protect
         (progn ,@body)
       (sqlite-close ,db))))

(defun my-sync--row-read (db file)
  "Baseline plist for FILE from DB, or nil."
  (let ((row (car (sqlite-select
                   db
                   "SELECT server_hash, hash_type, size, server_mtime,
                           content_b64, updated_at, source
                    FROM baseline WHERE file = ?"
                   (list file)))))
    (when row
      (list :exists (not (cl-every #'null
                                   (list (nth 0 row) (nth 2 row)
                                         (nth 3 row))))
            :hash (nth 0 row)
            :hash-type (nth 1 row)
            :size (nth 2 row)
            :mtime (nth 3 row)
            :content (when (nth 4 row)
                       (base64-decode-string (nth 4 row)))
            :updated (nth 5 row)
            :source (nth 6 row)))))

(defun my-sync--row-write (db file state content source)
  "Store one baseline row: FILE, server STATE, raw CONTENT bytes."
  (sqlite-execute db my-sync--upsert-sql
                  (list file
                        (plist-get state :hash)
                        (plist-get state :hash-type)
                        (plist-get state :size)
                        (plist-get state :mtime)
                        (when content (base64-encode-string content t))
                        (format-time-string "%Y-%m-%dT%H:%M:%S")
                        (symbol-name source))))

(defun my-sync--log (db file event &optional s b l detail)
  "Append one decision-log row in DB."
  (sqlite-execute db
                  "INSERT INTO decision_log (ts, file, event, s_hash,
                     b_hash, l_hash, detail)
                   VALUES (?, ?, ?, ?, ?, ?, ?)"
                  (list (format-time-string "%Y-%m-%dT%H:%M:%S")
                        file (symbol-name event) s b l detail)))

(defun my-sync--log1 (file event &optional detail)
  "Single-shot decision log write (opens the DB itself)."
  (my-sync--with-db db (my-sync--log db file event nil nil nil detail)))

;;;; Back-sync: server state (S)

(defun my-sync--lsjson-state (entry)
  "Server state plist from one lsjson ENTRY (alist); absent if nil."
  (if (not entry)
      (list :exists nil)
    (let* ((hashes (cdr (assq 'Hashes entry)))
           (pair (cl-some (lambda (type)
                            (assq (intern type) hashes))
                          '("MD5" "SHA-1" "SHA-256" "SHA-512"))))
      (list :exists t
            :hash (and pair (cdr pair))
            :hash-type (and pair (symbol-name (car pair)))
            :size (cdr (assq 'Size entry))
            :mtime (cdr (assq 'ModTime entry))))))

(defun my-sync--parse-lsjson (json)
  "Parse lsjson output JSON into an alist (FILE . STATE-PLIST)."
  (let ((entries (json-parse-string json
                                    :array-type 'list
                                    :object-type 'alist)))
    (mapcar (lambda (file)
              (cons file
                    (my-sync--lsjson-state
                     (cl-find-if
                      (lambda (e)
                        (equal (cdr (assq 'Name e)) file))
                      entries))))
            my-desktop-backsync-files)))

(defun my-sync--fetch-state (callback)
  "Fetch the server state of the back-sync files, then call CALLBACK.
CALLBACK receives an alist (FILE . STATE-PLIST), or nil on failure."
  (let* ((remote (my-sync--remote-root))
         (buf (get-buffer-create " *sync:lsjson*")))
    (with-current-buffer buf (erase-buffer))
    (set-process-sentinel
     (apply #'start-process "rclone-backsync-lsjson" buf "rclone"
            (list "lsjson" remote "--files-only" "--hash"))
     (lambda (proc _event)
       (when (memq (process-status proc) '(exit signal))
         (if (and (eq (process-status proc) 'exit)
                  (zerop (process-exit-status proc)))
             (condition-case err
                 (with-current-buffer (process-buffer proc)
                   (funcall callback
                            (my-sync--parse-lsjson
                             (buffer-substring-no-properties
                              (point-min) (point-max)))))
               (error (my-desktop--warn
                       "backsync: bad lsjson output: %s"
                       (error-message-string err))
                      (funcall callback nil)))
           (my-desktop--warn
            "backsync: rclone lsjson failed: %s"
            (with-current-buffer (process-buffer proc)
              (buffer-substring-no-properties
               (point-min) (min (point-max) (+ (point-min) 300)))))
           (funcall callback nil)))))))

(defun my-sync--state-equal (a b)
  "Non-nil when server states A and B describe the same server file.
Hash comparison when both carry one, else size + server mtime."
  (cond ((or (not (plist-get a :exists)) (not (plist-get b :exists)))
         (eq (not (not (plist-get a :exists)))
             (not (not (plist-get b :exists)))))
        ((and (plist-get a :hash) (plist-get b :hash))
         (and (equal (plist-get a :hash) (plist-get b :hash))
              (equal (plist-get a :hash-type) (plist-get b :hash-type))))
        (t (and (equal (plist-get a :size) (plist-get b :size))
                (equal (plist-get a :mtime) (plist-get b :mtime))))))

(defun my-sync--content-match (server local)
  "Non-nil when server state SERVER and local state LOCAL match content.
Falls back to size only when no common hash type exists."
  (cond ((not (plist-get server :exists)) (not (plist-get local :exists)))
        ((not (plist-get local :exists)) nil)
        ((and (plist-get server :hash) (plist-get local :hash))
         (and (equal (plist-get server :hash) (plist-get local :hash))
              (equal (plist-get server :hash-type)
                     (plist-get local :hash-type))))
        (t (equal (plist-get server :size) (plist-get local :size)))))

;;;; Back-sync: pull post-hook

(defun my-sync-pull-refresh-baseline ()
  "Refresh the back-sync baseline after a successful pull.
Referenced by the pull job's :on-success hook in desktop-config.el.
Right after a successful pull the local copies of
`my-desktop-backsync-files' are byte-identical to the server, so
they are hashed and stored directly (no server round-trip)."
  (interactive)
  (cond
   ((not my-sync--sqlite-ok)
    (my-desktop--warn "backsync baseline: no SQLite support, skipping"))
   ((not my-desktop-backsync-files))
   (t
    (my-sync--with-db db
      (sqlite-transaction db)
      (dolist (file my-desktop-backsync-files)
        (let* ((bytes (my-sync--file-bytes (my-sync--org-path file)))
               (hash-type (or (plist-get (my-sync--row-read db file)
                                         :hash-type)
                              "MD5"))
               (state (if bytes
                          (list :exists t
                                :hash (my-sync--hash-bytes bytes hash-type)
                                :hash-type hash-type
                                :size (length bytes))
                        (list :exists nil))))
          (my-sync--row-write db file state bytes 'pull)))
      (sqlite-commit db)))))

;;;; Back-sync: transfer primitives

(defun my-sync--fetch-copies (files callback)
  "Copy FILES from the remote into a temp dir, then call CALLBACK.
CALLBACK receives the temp dir, or nil on failure."
  (let* ((remote (my-sync--remote-root))
         (tmpdir (make-temp-file "org-backsync-" t))
         (listfile (expand-file-name "files.txt" tmpdir)))
    (with-temp-file listfile
      (insert (string-join files "\n")))
    (set-process-sentinel
     (start-process "rclone-backsync-copy" nil "rclone"
                    "copy" remote tmpdir
                    "--files-from" listfile)
     (lambda (proc _event)
       (when (memq (process-status proc) '(exit signal))
         (if (and (eq (process-status proc) 'exit)
                  (zerop (process-exit-status proc)))
             (funcall callback tmpdir)
           (my-desktop--warn "backsync: fetching server copies failed")
           (funcall callback nil)))))))

(defun my-sync--upload (file on-done on-fail)
  "Upload local FILE to the remote (gateless), then verify it.
Calls ON-DONE after a verified upload, ON-FAIL otherwise.  The
caller is responsible for the gate / CAS check."
  (let* ((local (my-sync--org-path file))
         (remote (my-sync--remote-root))
         (bytes (my-sync--file-bytes local)))
    (if (not bytes)
        (progn (my-desktop--warn "backsync: %s disappeared, not pushed" file)
               (funcall on-fail))
      (set-process-sentinel
       (start-process "rclone-backsync-push" nil "rclone"
                      "copyto" local
                      (my-sync--remote-path remote file)
                      "--ignore-times")
       (lambda (proc _event)
         (when (memq (process-status proc) '(exit signal))
           (if (and (eq (process-status proc) 'exit)
                    (zerop (process-exit-status proc)))
               (my-sync--verify-upload file bytes on-done on-fail)
             (my-desktop--warn "backsync: push of %s failed" file)
             (funcall on-fail))))))))

(defun my-sync--verify-upload (file bytes on-done on-fail)
  "Re-read the server state of FILE after an upload of BYTES."
  (my-sync--fetch-state
   (lambda (states)
     (let* ((st (and states (cdr (assoc file states))))
            (ok (and st (plist-get st :exists)
                     (equal (plist-get st :size) (length bytes))
                     (or (null (plist-get st :hash))
                         (equal (plist-get st :hash)
                                (my-sync--hash-bytes
                                 bytes (plist-get st :hash-type)))))))
       (if (not ok)
           (progn (my-desktop--warn
                   "backsync: post-push verification of %s failed" file)
                  (funcall on-fail))
         (funcall on-done st))))))

(defun my-sync--download (file on-done on-fail)
  "Copy the remote FILE over its local copy, then call ON-DONE."
  (let* ((remote (my-sync--remote-root))
         (local (my-sync--org-path file)))
    (set-process-sentinel
     (start-process "rclone-backsync-ff" nil "rclone"
                    "copyto" (my-sync--remote-path remote file) local)
     (lambda (proc _event)
       (when (memq (process-status proc) '(exit signal))
         (if (and (eq (process-status proc) 'exit)
                  (zerop (process-exit-status proc)))
             (funcall on-done)
           (my-desktop--warn "backsync: download of %s failed" file)
           (funcall on-fail)))))))

;;;; Back-sync: pull-side row updates

(defun my-sync--baseline-write (file state bytes source)
  "Transactionally update the row of FILE to STATE/BYTES, log SOURCE."
  (my-sync--with-db db
    (sqlite-transaction db)
    (my-sync--row-write db file state bytes source)
    (my-sync--log db file source (plist-get state :hash) nil nil)
    (sqlite-commit db)))

;;;; Back-sync: try-push

(defvar my-sync--resolve-session nil
  "Active conflict-resolution session, a plist, or nil.
Keys: :queue, :file, :tmpdir, :states, :buffer, :local-md5.")

(defun my-sync--out (fmt &rest args)
  "Append a line to the *sync:backsync* log buffer and echo it."
  (with-current-buffer (my-sync--buffer "backsync")
    (goto-char (point-max))
    (insert (apply #'format (concat fmt "\n") args)))
  (apply #'message (concat "[desktop] " fmt) args))

(defun my-sync--classify (row state lstate bytes)
  "Decide the action for one file (see the module Commentary).
ROW is the baseline plist, STATE the server state now, LSTATE the
local state, BYTES the local file contents (nil if absent)."
  (let* ((row-state (list :exists (plist-get row :exists)
                          :hash (plist-get row :hash)
                          :hash-type (plist-get row :hash-type)
                          :size (plist-get row :size)
                          :mtime (plist-get row :mtime)))
         (s-eq-b (my-sync--state-equal state row-state))
         (l-eq-b (and bytes (plist-get row :content)
                      (string= bytes (plist-get row :content))))
         (s-eq-l (my-sync--content-match state lstate)))
    (cond
     ((and s-eq-b l-eq-b) 'noop)
     ((and s-eq-b (not l-eq-b))
      (if (plist-get lstate :exists) 'push 'skip))
     ((and (not s-eq-b) s-eq-l) 'refresh)
     ((and (not s-eq-b) l-eq-b) 'ff)
     ((not (plist-get lstate :exists)) 'ff)
     (t 'conflict))))

(defun my-sync-backsync (&optional dry-run)
  "Try to push `my-desktop-backsync-files' back to the remote.
Safe: a file is uploaded only when the server copy still matches the
baseline (the state recorded at the last successful exchange).  On a
server-side change the file is fast-forwarded when local is
unchanged; otherwise a loud warning is raised and a
conflict-resolution session opens.  With prefix argument or
DRY-RUN, only report the decisions."
  (interactive "P")
  (unless (my-desktop--require-bin "rclone" "back-sync")
    (user-error "rclone not found"))
  (when my-sync--resolve-session
    (if (buffer-live-p (plist-get my-sync--resolve-session :buffer))
        (user-error "Finish the active conflict session first (%s)"
                    (buffer-name
                     (plist-get my-sync--resolve-session :buffer)))
      (my-sync--resolve-abandon 'silent)))
  (let ((missing nil))
    (my-sync--with-db db
      (dolist (file my-desktop-backsync-files)
        (unless (my-sync--row-read db file) (push file missing))))
    (when missing
      (user-error
       "backsync: no baseline for %s - run a pull or M-x my-sync-backsync-init"
       (string-join (nreverse missing) ", "))))
  (my-sync--fetch-state
   (lambda (states)
     (if (not states)
         (my-desktop--warn "backsync: could not fetch server state")
       (my-sync--tp-plan states dry-run)))))

(defun my-sync-backsync-dry-run ()
  "Report what `my-sync-backsync' would do, without any writes."
  (interactive)
  (my-sync-backsync 'dry-run))

(defun my-sync--tp-plan (states dry-run)
  "Read baselines and local files, then run the per-file actions."
  (let ((plans nil) (missing nil))
    (my-sync--with-db db
      (dolist (file my-desktop-backsync-files)
        (let* ((row (my-sync--row-read db file))
               (bytes (my-sync--file-bytes (my-sync--org-path file)))
               (lstate (list :exists (and bytes t)
                             :hash (when bytes
                                     (my-sync--hash-bytes
                                      bytes (plist-get row :hash-type)))
                             :hash-type (plist-get row :hash-type)
                             :size (and bytes (length bytes)))))
          (if (not row)
              (push file missing)
            (push (list file row (cdr (assoc file states))
                        lstate bytes)
                  plans)))))
    (when missing
      (user-error
       "backsync: no baseline for %s - run a pull or M-x my-sync-backsync-init"
       (string-join (nreverse missing) ", ")))
    (with-current-buffer (my-sync--buffer "backsync")
      (goto-char (point-max))
      (insert (format "\n$ try-push %s(%s)\n"
                      (if dry-run "DRY-RUN " "")
                      (format-time-string "%Y-%m-%d %H:%M:%S"))))
    (setq plans (nreverse plans))
    (my-sync--tp-run
     (list :files (mapcar #'car plans)
           :plans plans
           :states states
           :dry-run dry-run
           :conflicts nil))))

(defun my-sync--tp-run (ctx)
  "Process the next file in CTX, or finish."
  (let ((file (pop (plist-get ctx :files))))
    (if (not file)
        (my-sync--tp-finish ctx)
      (let* ((entry (assoc file (plist-get ctx :plans)))
             (row (nth 1 entry))
             (state (nth 2 entry))
             (lstate (nth 3 entry))
             (bytes (nth 4 entry))
             (action (my-sync--classify row state lstate bytes))
             (next (lambda () (my-sync--tp-run ctx))))
        (pcase action
          ('noop
           (my-sync--log1 file 'noop)
           (my-sync--out "%s: unchanged, nothing to do" file)
           (funcall next))
          ('skip
           (my-sync--log1 file 'skip "local file missing")
           (my-sync--out "%s: LOCAL FILE MISSING, skipped" file)
           (funcall next))
          ('refresh
           (my-sync--baseline-write file state bytes 'refresh)
           (my-sync--out "%s: server matches local, baseline refreshed"
                         file)
           (funcall next))
          ('ff
           (if (plist-get ctx :dry-run)
               (progn (my-sync--out "%s: would fast-forward from server"
                                    file)
                      (funcall next))
             (my-sync--out "%s: server-only change, fast-forwarding..."
                           file)
             (my-sync--download
              file
              (lambda ()
                (my-sync--baseline-write
                 file state (my-sync--file-bytes
                             (my-sync--org-path file))
                 'pull)
                (my-sync--out "%s: fast-forwarded from server" file)
                (my-sync--log1 file 'fast-forward)
                (funcall next))
              (lambda () (my-sync--log1 file 'ff-failed)
                       (funcall next)))))
          ('push
           (if (plist-get ctx :dry-run)
               (progn (my-sync--out "%s: would push" file)
                      (funcall next))
             (my-sync--out "%s: pushing..." file)
             (my-sync--push-file file next 'push
                                 (lambda () (funcall next)))))
          ('conflict
           (lwarn 'desktop-sync :warning
                  "backsync: server copy of %s changed since the last \
sync; conflict resolution required" file)
           (my-sync--log1 file 'conflict)
           (my-sync--out "%s: CONFLICT (server changed), queued for \
resolution" file)
           (push file (plist-get ctx :conflicts))
           (funcall next))
          (_ (my-sync--out "%s: unknown action %s" file action)
             (funcall next)))))))

(defun my-sync--push-file (file cont source &optional on-fail)
  "Upload local FILE (gateless), verify it, update the baseline row.
The caller is responsible for the gate / CAS check.  CONT runs after
a verified upload + row update (SOURCE names the update); ON-FAIL
defaults to just CONT."
  (my-sync--upload
   file
   (lambda (st)
     (my-sync--baseline-write
      file st (my-sync--file-bytes (my-sync--org-path file)) source)
     (my-sync--out "%s: pushed and verified" file)
     (funcall cont))
   (lambda ()
     (my-sync--log1 file 'push-failed)
     (funcall (or on-fail cont)))))

(defun my-sync--tp-finish (ctx)
  "All files processed: summarize, then resolve conflicts if any."
  (let ((conflicts (nreverse (plist-get ctx :conflicts))))
    (if (null conflicts)
        (my-sync--out "try-push done%s"
                      (if (plist-get ctx :dry-run) " (dry-run)" ""))
      (my-sync--out "%d conflict(s): %s" (length conflicts)
                    (string-join conflicts ", "))
      (unless (plist-get ctx :dry-run)
        (my-sync--resolve-start conflicts ctx)))))

;;;; Conflict resolution session

(defun my-sync--resolve-start (conflicts ctx)
  "Fetch the server copies of CONFLICTS and open the first merge."
  (my-sync--fetch-copies
   conflicts
   (lambda (tmpdir)
     (if (not tmpdir)
         (my-sync--out "conflict resolution aborted (fetch failed)")
       (setq my-sync--resolve-session
             (list :queue conflicts
                   :file nil
                   :buffer nil
                   :local-md5 nil
                   :tmpdir tmpdir
                   :states (plist-get ctx :states)))
       (my-sync--out "conflict resolution: %d file(s) to merge"
                     (length conflicts))
       (my-sync--resolve-open-next)))))

(defun my-sync--resolve-open-next ()
  "Open the merge buffer of the next conflicted file."
  (let ((session my-sync--resolve-session))
    (unless session (user-error "No active conflict session"))
    (let ((file (car (plist-get session :queue))))
      (setq my-sync--resolve-session
            (plist-put session :queue (cdr (plist-get session :queue)))
            my-sync--resolve-session
            (plist-put my-sync--resolve-session :file file))
      (cond
       ((not file) (my-sync--resolve-finish))
       ((not (file-exists-p (my-sync--org-path file)))
        (my-sync--out "%s: local file missing, skipping" file)
        (my-sync--resolve-open-next))
       (t
        (let* ((local (my-sync--org-path file))
               (tmpdir (plist-get session :tmpdir))
               (base (expand-file-name (concat "base-" file) tmpdir))
               (theirs (expand-file-name file tmpdir))
               (row (my-sync--with-db db (my-sync--row-read db file)))
               (buf (get-buffer-create
                     (format "*sync:resolve:%s*" file)))
               clean
               status)
          (let ((coding-system-for-write 'no-conversion))
            (write-region (or (plist-get row :content) "")
                          nil base nil 'quiet))
          (setq my-sync--resolve-session
                (plist-put session :local-md5
                           (my-sync--hash-file local 'md5)))
          (with-current-buffer buf
            (let ((inhibit-read-only t))
              (erase-buffer)
              (let ((coding-system-for-read 'utf-8-unix))
                (setq status
                      (call-process "diff3" nil buf nil
                                    "-m" "-L" "LOCAL" "-L" "BASE"
                                    "-L" "SERVER" local base theirs)))
              (goto-char (point-min))
              (setq clean (not (re-search-forward
                                "^<<<<<<< " nil t)))
              (goto-char (point-min))))
          (if (eq status 2)          ; diff3 trouble: report and skip
              (progn
                (my-desktop--warn "backsync: diff3 failed on %s" file)
                (my-sync--log1 file 'diff3-failed)
                (kill-buffer buf)
                (my-sync--resolve-open-next))
            (with-current-buffer buf
              (smerge-mode 1)
              (my-sync-conflict-mode 1)
              (setq-local my-sync--conflict-file file))
            (setq my-sync--resolve-session
                  (plist-put session :buffer buf))
            (my-sync--out "resolving %s (%s)" file
                          (if clean
                              "clean auto-merge, review and accept"
                            "conflict markers, C-c ^ n / u / l / a"))
            (unless noninteractive
              (pop-to-buffer buf)))))))))

(defun my-sync--resolve-next ()
  "Continue the session with the next file, or finish."
  (let ((session my-sync--resolve-session))
    (when session
      (let ((buf (plist-get session :buffer)))
        (when (and buf (buffer-live-p buf)) (kill-buffer buf)))
      (my-sync--resolve-open-next))))

(defun my-sync--resolve-finish ()
  "Session complete: clean up and report."
  (let ((tmpdir (plist-get my-sync--resolve-session :tmpdir)))
    (setq my-sync--resolve-session nil)
    (when tmpdir (ignore-errors (delete-directory tmpdir t))))
  (my-sync--out "conflict resolution done"))

(defun my-sync--resolve-abandon (&optional silent)
  "Give up the active session, keeping every side unchanged."
  (when my-sync--resolve-session
    (let ((tmpdir (plist-get my-sync--resolve-session :tmpdir)))
      (dolist (buf (buffer-list))
        (when (string-prefix-p "*sync:resolve:" (buffer-name buf))
          (kill-buffer buf)))
      (setq my-sync--resolve-session nil)
      (when tmpdir (ignore-errors (delete-directory tmpdir t))))
    (unless silent
      (message "[desktop] backsync: conflict session abandoned"))))

(defun my-sync-conflict-skip ()
  "Skip the current conflict: change nothing, open the next one."
  (interactive)
  (unless my-sync--resolve-session
    (user-error "No active conflict session"))
  (my-sync--log1 (plist-get my-sync--resolve-session :file) 'skip
                 "conflict skipped")
  (my-sync--out "%s: conflict skipped"
                (plist-get my-sync--resolve-session :file))
  (my-sync--resolve-next))

(defun my-sync-conflict-accept ()
  "Accept the merge buffer: write it to the org dir and push it.
The push is a compare-and-swap: the server must still be in the
state we merged against, otherwise the resolution is NOT uploaded
and a loud warning is raised."
  (interactive)
  (unless my-sync--resolve-session
    (user-error "No active conflict session"))
  (let* ((session my-sync--resolve-session)
         (file (plist-get session :file))
         (local (my-sync--org-path file)))
    (goto-char (point-min))
    (when (re-search-forward "^<<<<<<< " nil t)
      (user-error "Unresolved conflict markers remain (C-c ^ n / u / l / a)"))
    (unless (equal (my-sync--hash-file local 'md5)
                   (plist-get session :local-md5))
      (unless (y-or-n-p
               (format "%s changed on disk since the conflict was \
detected; overwrite it with this resolution? " file))
        (user-error "Aborted")))
    (let ((coding-system-for-write 'utf-8-unix))
      (write-region (point-min) (point-max) local nil 'quiet))
    (my-sync--log1 file 'resolved-locally)
    ;; compare-and-swap: server must still be in the merged-against state
    (my-sync--fetch-state
     (lambda (states)
       (cond
        ((not states)
         (lwarn 'desktop-sync :error
                "backsync: could not re-check the server for %s - \
resolution NOT pushed" file)
         (my-sync--log1 file 'cas-error)
         (my-sync--resolve-next))
        ((not (my-sync--state-equal
               (cdr (assoc file states))
               (cdr (assoc file (plist-get session :states)))))
         (lwarn 'desktop-sync :error
                "backsync: server changed while you were resolving %s - \
resolution NOT pushed; re-run try-push" file)
         (my-sync--log1 file 'cas-aborted)
         (my-sync--out "%s: CAS failed, resolution kept locally only"
                       file)
         (my-sync--resolve-next))
        (t
         (my-sync--push-file
          file (lambda () (my-sync--resolve-next)) 'resolve)))))))

(defvar my-sync-conflict-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") #'my-sync-conflict-accept)
    (define-key map (kbd "C-c C-k") #'my-sync-conflict-skip)
    map)
  "Keymap for `my-sync-conflict-mode' buffers.")

(define-minor-mode my-sync-conflict-mode
  "Minor mode of a back-sync conflict merge buffer.
Navigate conflicts with `smerge-mode' bindings (C-c ^ n / p, keep a
side with C-c ^ u / l / a).  Then:
  \\[my-sync-conflict-accept]  accept: write to the org dir, CAS push, next conflict
  \\[my-sync-conflict-skip]  skip: change nothing, next conflict"
  :init-value nil
  :lighter " SyncConflict"
  :keymap my-sync-conflict-mode-map)

;;;; Back-sync: baseline bootstrap

(defun my-sync-backsync-init ()
  "Create baseline rows from the CURRENT server state, pushing nothing.
One-time bootstrap (or recovery after deleting the database): after
this, `my-sync-backsync' trusts the server state as of now."
  (interactive)
  (unless (my-desktop--require-bin "rclone" "back-sync")
    (user-error "rclone not found"))
  (my-sync--fetch-state
   (lambda (states)
     (if (not states)
         (my-desktop--warn "backsync init: could not fetch server state")
       (my-sync--fetch-copies
        (mapcar #'car states)
        (lambda (tmpdir)
          (my-sync--with-db db
            (sqlite-transaction db)
            (dolist (file my-desktop-backsync-files)
              (let ((state (cdr (assoc file states)))
                    (bytes (when tmpdir
                             (my-sync--file-bytes
                              (expand-file-name file tmpdir)))))
                (my-sync--row-write db file state bytes 'init)
                (my-sync--log db file 'init
                              (plist-get state :hash) nil nil)))
            (sqlite-commit db))
          (when tmpdir (ignore-errors (delete-directory tmpdir t)))
          (message "[desktop] backsync: baseline initialized (%s)"
                   (string-join my-desktop-backsync-files ", "))))))))

;;;; Menu

(transient-define-prefix my-sync-menu ()
  "Rclone sync jobs."
  [["Jobs"
    ("r" "Run job" my-sync-run)
    ("d" "Dry-run job" my-sync-dry-run)]
   ["Back-sync"
    ("b" "Try-push files" my-sync-backsync)
    ("B" "Try-push (dry-run)" my-sync-backsync-dry-run)
    ("i" "Init baseline" my-sync-backsync-init)]
   ["Logs"
    ("l" "Open job log" my-sync-open-log)
    ("k" "Kill running jobs" my-sync-kill)]])

(provide 'desktop-sync)
;;; desktop-sync.el ends here
