;;; desktop-ai.el --- gptel-based assistant, hard-restricted -*- lexical-binding: t; -*-

;;; Commentary:
;; AnythingLLM replacement on top of gptel.  The assistant has exactly
;; these tools, nothing else (no shell, no arbitrary filesystem):
;;   list_attachments - list files attached to THIS chat
;;   read_attachment  - read a file from this chat's folder
;;   memory_save      - store a durable fact (shared by all chats)
;;   memory_search    - search stored facts
;;   history_search   - search the user's opted-in past chats
;;   history_read     - read an opted-in past chat
;;   web_search       - SearXNG query (only when configured)
;;   web_fetch        - read a web page
;;
;; State (session titles and config, attachments, memory, the history
;; index) lives in one sqlite database, `my-desktop-ai-db-file'.
;; Transcripts stay org files in `my-desktop-ai-sessions-dir' (gptel
;; is file-based); they are indexed into the history only when the
;; user opts a chat in (C-c a h).  Every chat owns a private
;; attachments folder under `my-desktop-ai-attachments-dir' -- other
;; chats' folders are unreachable by construction.  Attached documents
;; are converted to clean markdown text with markitdown before they
;; reach the model; the folder never stores raw containers.
;;
;; A chat is identified by a permanent random uuid: it names the org
;; file and the attachments folder and never changes.  Renaming a
;; chat (C-c a r) only updates the display title in the database.
;;
;; Backends come from `my-desktop-ai-backends'; API keys ALWAYS come
;; from environment variables (:key-env), never from the config file.
;; :type is 'openai (default, any OpenAI-compatible API) or 'deepseek
;; (gptel's native backend, with built-in host/endpoint/models).

;;; Code:

(require 'json)
(require 'mailcap)
(require 'url)
(require 'seq)
(require 'map)
(require 'desktop-config-defs)
(require 'desktop-core)

;; gptel internals used here (loaded with gptel at runtime).
(declare-function gptel--file-binary-p "gptel-request")
(declare-function gptel--model-capable-p "gptel-request")
(declare-function gptel--model-mime-capable-p "gptel-request")
(declare-function gptel-add-file "gptel-context")
(declare-function gptel-backend-name "gptel")
(declare-function gptel-tool-name "gptel")
(defvar gptel-backend)
(defvar gptel-context)
(defvar gptel-directives)
(defvar gptel-model)
(defvar gptel--system-message)
(defvar gptel-tools)
(defvar gptel-use-tools)

;; Per-chat state (buffer-local by design: gptel runs tool calls and
;; builds requests with the chat buffer current, so these are in
;; scope inside tools and copied into request construction).
(defvar-local my-ai-session-file nil
  "File this gptel buffer persists to.  Set at chat creation.")

(defvar-local my-ai-session-id nil
  "Row id of this chat in the chats database.")

(defvar-local my-ai-session-uuid nil
  "Permanent random identifier of this chat.  Names the transcript
file and the attachments folder and never changes; renames only
update the display title in the database.")

(defvar-local my-ai-chat-dir nil
  "This chat's private attachments folder.  Other chats' folders
are unreachable: read_attachment resolves paths inside this
directory only.")

(defvar-local my-ai-in-history nil
  "Whether this chat is indexed into the shared history.  Toggled
with `my-ai-history-toggle'; mirrored in the session row.")

;; ------------------------------------------------------------------
;; State database: sessions, attachments, memory, history
;; ------------------------------------------------------------------
;; One sqlite file (my-desktop-ai-db-file) holds everything except
;; the org transcripts: session titles and config, attachment
;; records, agent memory and the opt-in full-text history index.
;; FTS5 is used for history search, with LIKE over history_text as
;; the conceptual fallback (FTS5 ships in every common build).

(defvar my-ai--db nil
  "Open connection to the chats database.")

(defvar my-ai--backends nil
  "Alist of (NAME . backend) built at startup, for session restore.")

(defconst my-ai--db-ddl
  '("CREATE TABLE IF NOT EXISTS meta (key TEXT PRIMARY KEY, value TEXT)"
    "CREATE TABLE IF NOT EXISTS session (
       id INTEGER PRIMARY KEY,
       uuid TEXT NOT NULL UNIQUE,
       title TEXT NOT NULL DEFAULT '',
       file TEXT NOT NULL,
       created_at TEXT NOT NULL,
       updated_at TEXT,
       backend TEXT,
       model TEXT,
       tools TEXT,
       directive TEXT,
       in_history INTEGER NOT NULL DEFAULT 0,
       status TEXT NOT NULL DEFAULT 'active')"
    "CREATE TABLE IF NOT EXISTS attachment (
       id INTEGER PRIMARY KEY,
       session_id INTEGER NOT NULL REFERENCES session(id) ON DELETE CASCADE,
       orig_name TEXT NOT NULL,
       stored_name TEXT NOT NULL,
       kind TEXT NOT NULL,
       chars INTEGER,
       added_at TEXT NOT NULL,
       UNIQUE (session_id, stored_name))"
    "CREATE TABLE IF NOT EXISTS memory (
       id INTEGER PRIMARY KEY,
       content TEXT NOT NULL,
       session_id INTEGER REFERENCES session(id) ON DELETE SET NULL,
       created_at TEXT NOT NULL,
       updated_at TEXT)"
    "CREATE TABLE IF NOT EXISTS history_text (
       session_id INTEGER PRIMARY KEY REFERENCES session(id) ON DELETE CASCADE,
       body TEXT NOT NULL,
       updated_at TEXT)"
    "CREATE VIRTUAL TABLE IF NOT EXISTS history_fts USING fts5(body)"))

(defun my-ai--db ()
  "Return the open chats database, opening and initializing it."
  (unless (sqlite-available-p)
    (user-error "This Emacs has no sqlite support (required for AI chats)"))
  (unless my-ai--db
    (let ((file (expand-file-name my-desktop-ai-db-file)))
      (make-directory (file-name-directory file) t)
      (setq my-ai--db (sqlite-open file))
      (sqlite-execute my-ai--db "PRAGMA foreign_keys=ON")
      (dolist (ddl my-ai--db-ddl)
        (sqlite-execute my-ai--db ddl))))
  my-ai--db)

(defun my-ai--db-execute (sql &rest args)
  "Run SQL with ARGS against the chats database."
  (sqlite-execute (my-ai--db) sql args))

(defun my-ai--db-select (sql &rest args)
  "Query the chats database with ARGS, return the rows."
  (sqlite-select (my-ai--db) sql args))

(defun my-ai--now ()
  "Local ISO-8601 timestamp."
  (format-time-string "%Y-%m-%dT%H:%M:%S"))

(defun my-ai--fts-query (query)
  "Turn QUERY into a safe FTS5 MATCH expression: terms are
double-quoted (punctuation stays literal) and ANDed."
  (string-join
   (mapcar (lambda (word)
             (concat "\"" (string-replace "\"" "\"\"" word) "\""))
           (split-string query "[[:space:]]+" t))
   " "))

(defun my-ai--session-create (uuid file)
  "Insert a session row for UUID and FILE; return its database id."
  (my-ai--db-execute
   "INSERT INTO session (uuid, title, file, created_at, in_history)
    VALUES (?, '', ?, ?, ?)"
   uuid file (my-ai--now)
   (if my-desktop-ai-history-default 1 0))
  (caar (my-ai--db-select "SELECT id FROM session WHERE uuid = ?" uuid)))

(defun my-ai--attachment-upsert (orig stored kind chars)
  "Record an attachment of the current chat; same stored name
overwrites the previous record."
  (my-ai--db-execute
   "INSERT INTO attachment
      (session_id, orig_name, stored_name, kind, chars, added_at)
    VALUES (?, ?, ?, ?, ?, ?)
    ON CONFLICT(session_id, stored_name) DO UPDATE SET
      orig_name = excluded.orig_name, kind = excluded.kind,
      chars = excluded.chars, added_at = excluded.added_at"
   my-ai-session-id orig stored kind chars (my-ai--now)))

(defun my-ai--history-sync ()
  "Mirror the current chat into (or out of) the history index."
  (when my-ai-session-id
    (if my-ai-in-history
        (let ((body (my-ai--org-plain-text)))
          (my-ai--db-execute
           "INSERT INTO history_text (session_id, body, updated_at)
            VALUES (?, ?, ?)
            ON CONFLICT(session_id) DO UPDATE SET
              body = excluded.body, updated_at = excluded.updated_at"
           my-ai-session-id body (my-ai--now))
          (my-ai--db-execute "DELETE FROM history_fts WHERE rowid = ?"
                             my-ai-session-id)
          (my-ai--db-execute
           "INSERT INTO history_fts (rowid, body) VALUES (?, ?)"
           my-ai-session-id body))
      (my-ai--db-execute "DELETE FROM history_fts WHERE rowid = ?"
                         my-ai-session-id)
      (my-ai--db-execute "DELETE FROM history_text WHERE session_id = ?"
                         my-ai-session-id))))

(defun my-ai--org-plain-text ()
  "Return a plain-text projection of the current org transcript.
Drops property drawers, \"#+\" lines and heading stars; good
enough for full-text search."
  (let ((in-drawer nil) lines)
    (save-restriction
      (widen)
      (dolist (line (split-string
                     (buffer-substring-no-properties
                      (point-min) (point-max))
                     "\n" t))
        (cond
         ((string-match-p "^[ \t]*:PROPERTIES:" line)
          (setq in-drawer t))
         ((and in-drawer (string-match-p "^[ \t]*:END:" line))
          (setq in-drawer nil))
         (in-drawer)
         ((string-prefix-p "#+" line))
         (t (push (if (string-match "^\*+ " line)
                      (substring line (match-end 0))
                    line)
                  lines)))))
    (string-join (nreverse lines) "\n")))

;; ------------------------------------------------------------------
;; Tools (the allowlist IS the sandbox)
;; ------------------------------------------------------------------

(defun my-ai--http-body (url)
  "Fetch URL synchronously, return (HTTP-STATUS . BODY) or nil."
  (let ((buffer (url-retrieve-synchronously url)))
    (when buffer
      (with-current-buffer buffer
        (let ((status
               (progn (goto-char (point-min))
                      (and (looking-at "HTTP/[0-9.]+ +\\([0-9]+\\)")
                           (string-to-number (match-string 1)))))
              (body
               (progn (goto-char (point-min))
                      (when (re-search-forward "^$" nil t)
                        (forward-char 1)
                        (decode-coding-string
                         (buffer-substring (point) (point-max))
                         'utf-8)))))
          (when body
            (cons (or status 0) body)))))))

(defun my-ai--searxng (query)
  "Search the configured SearXNG instance for QUERY."
  (condition-case err
      (let* ((url (concat my-desktop-ai-searxng-url
                          "?format=json&q="
                          (url-hexify-string query)))
             (resp (my-ai--http-body url)))
        (cond
         ((not resp)
          "ERROR: search request failed (no response)")
         ((/= (car resp) 200)
          (if (= (car resp) 403)
              (concat
               "ERROR: SearXNG returned 403 Forbidden.  The 'json' \
output format is disabled in this instance -- add \"json\" to \
search.formats in SearXNG settings.yml and restart it.")
            (format "ERROR: SearXNG returned HTTP %d" (car resp))))
         (t
          (let* ((data (json-parse-string (cdr resp)
                                          :object-type 'alist
                                          :array-type 'list))
                 (results (alist-get 'results data)))
            (if (not results)
                "No results."
              (string-join
               (mapcar
                (lambda (r)
                  (format "- %s\n  %s\n  %s"
                          (alist-get 'title r)
                          (alist-get 'url r)
                          (or (alist-get 'content r) "")))
                (seq-take results my-desktop-ai-search-limit))
               "\n"))))))
    (error (format "ERROR: %s" (error-message-string err)))))

(defun my-ai--read-attachment (path)
  "Read PATH from the current chat's attachments folder.
gptel runs tool calls with the chat buffer current, so the
buffer-local folder is in scope.  Binary files are refused: the
folder stores clean text and images only."
  (let ((dir my-ai-chat-dir))
    (cond
     ((not (and dir (file-directory-p dir)))
      "ERROR: this chat has no attachments folder yet")
     (t
      (let ((file (expand-file-name path dir)))
        (cond
         ((not (file-in-directory-p file dir))
          "ERROR: path escapes this chat's folder")
         ((not (file-readable-p file))
          (format "ERROR: no file %s (see list_attachments)" path))
         ((file-directory-p file)
          "ERROR: not a regular file")
         ((> (nth 7 (file-attributes file))
             my-desktop-ai-attachment-max-size)
          "ERROR: file too large")
         ((gptel--file-binary-p file)
          (let ((md (concat (file-name-sans-extension file) ".md")))
            (if (file-exists-p md)
                (format "ERROR: %s is binary -- read %s instead"
                        path (file-name-nondirectory md))
              "ERROR: binary file -- the folder stores clean text
and images only; ask the user to attach a text version")))
         (t
          (with-temp-buffer
            (insert-file-contents file)
            (buffer-string)))))))))

(defun my-ai--list-attachments ()
  "List the files attached to the current chat."
  (if (not my-ai-session-id)
      "ERROR: not in a chat buffer"
    (let ((rows (my-ai--db-select
                 "SELECT orig_name, stored_name, kind, chars, added_at
                  FROM attachment WHERE session_id = ? ORDER BY id"
                 my-ai-session-id)))
      (if (not rows)
          "No attachments in this chat yet."
        (string-join
         (mapcar
          (lambda (row)
            (pcase-let ((`(,orig ,stored ,kind ,chars ,added) row))
              (format "%s <- %s (%s%s, added %s)"
                      stored orig kind
                      (if chars (format ", %d chars" chars) "")
                      (or added "?"))))
          rows)
         "\n")))))

(defun my-ai--history-search (query)
  "Search opted-in past chats for QUERY: session ids, titles,
snippets, attachment names and transcript paths.  The current
chat is excluded.  Links are informational for the user: other
chats' files are not readable."
  (let ((match (my-ai--fts-query query)))
    (if (string-empty-p match)
        "ERROR: empty query"
      (condition-case err
          (let ((rows (my-ai--db-select
                       "SELECT s.id, s.title, s.uuid,
                               substr(s.updated_at, 1, 10),
                               snippet(history_fts, 0, '[', ']',
                                       ' ... ', 20)
                        FROM history_fts JOIN session s
                          ON s.id = history_fts.rowid
                        WHERE history_fts MATCH ? AND s.id != ?
                        ORDER BY rank LIMIT 5"
                       match (or my-ai-session-id -1))))
            (if (not rows)
                "No history matches."
              (string-join
               (mapcar
                (lambda (row)
                  (pcase-let ((`(,id ,title ,uuid ,day ,snippet) row))
                    (let ((atts (my-ai--db-select
                                 "SELECT stored_name FROM attachment
                                  WHERE session_id = ?" id))
                          (file (caar (my-ai--db-select
                                       "SELECT file FROM session
                                        WHERE id = ?" id))))
                      (concat
                       (format "[%d] %s (%s): %s"
                               id
                               (if (string-empty-p title) uuid title)
                               (or day "?")
                               (string-trim
                                (replace-regexp-in-string
                                 "\n" " " (or snippet ""))))
                       (if atts
                           (format "\nfiles: %s"
                                   (string-join
                                    (mapcar #'car atts) ", "))
                         "")
                       (format "\ntranscript: %s" (or file "?"))))))
                rows)
               "\n\n")))
        (error (format "ERROR: %s" (error-message-string err)))))))

(defun my-ai--history-read (session-id &optional offset)
  "Read the indexed transcript of SESSION-ID (from
history_search) in chunks of `my-desktop-ai-fetch-limit'
characters; OFFSET continues a previous chunk."
  (condition-case err
      (let* ((id (if (stringp session-id)
                     (string-to-number session-id)
                   session-id))
             (row (car (my-ai--db-select
                        "SELECT s.title, s.uuid, h.body
                         FROM history_text h JOIN session s
                           ON s.id = h.session_id
                         WHERE h.session_id = ?" id)))
             (offset (max 0 (or offset 0)))
             (limit (or my-desktop-ai-fetch-limit 10000)))
        (cond
         ((not row)
          (format "ERROR: session %s is not indexed (only opted-in
chats are readable; use history_search)" id))
         (t
          (pcase-let ((`(,title ,uuid ,body) row))
            (if (>= offset (length body))
                (format "ERROR: offset %d is beyond the transcript
(%d characters)" offset (length body))
              (let* ((end (min (+ offset limit) (length body)))
                     (more-p (< end (length body))))
                (concat
                 (format "[history %s: showing %d-%d of %d characters%s]\n"
                         (if (string-empty-p title) uuid title)
                         offset end (length body)
                         (if more-p
                             (format " -- to continue, call again
with offset %d" end)
                           " -- end of transcript"))
                 (substring body offset end))))))))
    (error (format "ERROR: %s" (error-message-string err)))))

(defvar my-ai--fetch-cache nil
  "Per-session cache of (URL . EXTRACTED-TEXT) for web_fetch.")

(defun my-ai--trafilatura-binary ()
  "Locate the trafilatura executable, or nil."
  (or (executable-find "trafilatura")
      (let ((p (expand-file-name "~/.local/bin/trafilatura")))
        (and (file-executable-p p) p))))

(defun my-ai--trafilatura-extract (url)
  "Run trafilatura on URL; return extracted markdown text or nil."
  (let ((bin (my-ai--trafilatura-binary)))
    (when bin
      (let ((out (generate-new-buffer " *trafilatura out*"))
            (errs (generate-new-buffer " *trafilatura err*"))
            (text nil))
        (unwind-protect
            (progn
              (call-process bin nil (list out errs) nil
                            "-u" url "--markdown")
              (with-current-buffer out
                (setq text (string-trim (buffer-string)))))
          (kill-buffer out)
          (kill-buffer errs))
        (and (stringp text) (not (string-empty-p text)) text)))))

(defun my-ai--html-to-text (body)
  "Render HTML BODY to readable text (shr), with fallback."
  (if (and (fboundp 'libxml-parse-html-region)
           (string-match-p "\\`[ \\t\\r\\n]*<[a-zA-Z!]" body))
      (with-temp-buffer
        (insert body)
        (let ((dom (libxml-parse-html-region (point-min) (point-max))))
          (erase-buffer)
          (condition-case nil
              (progn (require 'shr)
                     (let ((shr-inhibit-images t))
                       (shr-insert-document dom)))
            (error (erase-buffer) (insert body))))
        (buffer-string))
    body))

(defun my-ai--fetch-internal (url)
  "Fetch URL with url-retrieve; return (HTTP-STATUS . TEXT) or nil.
Text is the rendered body (empty when there is none)."
  (let ((resp (my-ai--http-body url)))
    (when resp
      (cons (car resp) (my-ai--html-to-text (cdr resp))))))

(defun my-ai--fetch-text (url)
  "Return the readable text of URL (cached), or an ERROR string.
trafilatura extracts the main content first; Emacs' own fetch and
render is the fallback and reports HTTP errors."
  (or (cdr (assoc url my-ai--fetch-cache))
      (let (text)
        ;; 1. trafilatura: clean main-content extraction.
        (condition-case nil
            (setq text (my-ai--trafilatura-extract url))
          (error nil))
        ;; 2. Fallback: fetch ourselves (also reports HTTP errors).
        (unless (and text (not (string-empty-p text)))
          (let ((resp (my-ai--fetch-internal url)))
            (cond
             ((not resp)
              (unless text
                (setq text (format "ERROR: fetch failed (no response) \
for %s" url))))
             ((/= (car resp) 200)
              (setq text (format "ERROR: HTTP %d for %s"
                                 (car resp) url)))
             ((string-empty-p (string-trim (cdr resp)))
              (setq text (format "ERROR: %s returned no readable \
text" url)))
             (t (setq text (cdr resp)))))))
        (when (stringp text)
          (setq my-ai--fetch-cache
                (cons (cons url text) my-ai--fetch-cache))
          text)))

(defun my-ai--web-fetch (url &optional offset)
  "Fetch URL, return readable text in chunks of FETCH-LIMIT chars.
OFFSET continues a previous chunk (the tool result states the
offset to continue from)."
  (condition-case err
      (let* ((url (string-trim url))
             (offset (max 0 (or offset 0)))
             (limit (or my-desktop-ai-fetch-limit 10000)))
        (cond
         ((not (string-match-p "\\`https?://" url))
          "ERROR: only http:// and https:// URLs are supported")
         (t
          (let ((text (my-ai--fetch-text url)))
            (cond
             ((and text (string-prefix-p "ERROR:" text)) text)
             ((not text) (format "ERROR: fetch failed for %s" url))
             ((>= offset (length text))
              (format "ERROR: offset %d is beyond the document \
(%d characters)" offset (length text)))
             (t
              (let* ((end (min (+ offset limit) (length text)))
                     (more-p (< end (length text))))
                (concat
                 (format "[web_fetch: showing %d-%d of %d characters%s]\n"
                         offset end (length text)
                         (if more-p
                             (format " -- to continue, call again with \
offset %d" end)
                           " -- end of document"))
                 (substring text offset end)))))))))
    (error (format "ERROR: %s" (error-message-string err)))))

(defun my-ai--memory-save (text)
  "Store TEXT as a durable fact in the memory table, tagged with
the current chat for provenance."
  (my-ai--db-execute
   "INSERT INTO memory (content, session_id, created_at, updated_at)
    VALUES (?, ?, ?, ?)"
   text my-ai-session-id (my-ai--now) (my-ai--now))
  "Saved to memory.")

(defun my-ai--memory-search (query)
  "Return memory entries containing QUERY."
  (let ((rows (my-ai--db-select
               "SELECT content FROM memory WHERE content LIKE ?
                ORDER BY id"
               (concat "%" query "%"))))
    (if rows
        (string-join (mapcar #'car rows) "\n")
      "No memory entries match.")))

;; ------------------------------------------------------------------
;; Backend construction from the schema
;; ------------------------------------------------------------------

(defun my-ai--make-backend (name type host endpoint key-env
                                 models stream)
  "Build a gptel backend from a `my-desktop-ai-backends' spec.
NAME TYPE HOST ENDPOINT KEY-ENV MODELS STREAM are the schema
fields.  TYPE 'deepseek uses gptel's native backend (host,
endpoint and model defaults are built in).  TYPE 'openai
(default) targets any OpenAI-compatible API.  The key is read
from the KEY-ENV environment variable at request time."
  (let ((key-fn `(lambda () (getenv ,key-env))))
    (pcase type
      ('deepseek
       (apply #'gptel-make-deepseek name
              (append (list :key key-fn)
                      (when models (list :models models))
                      (when stream (list :stream t)))))
      (_
       (apply #'gptel-make-openai name
              (append (list :host host
                            :key key-fn
                            :models models)
                      (when endpoint (list :endpoint endpoint))
                      (when stream (list :stream t))))))))

;; ------------------------------------------------------------------
;; Sessions: org files, auto-saved after each response, resumable
;; ------------------------------------------------------------------

(defun my-ai--sessions-dir ()
  "Session directory, expanded."
  (expand-file-name my-desktop-ai-sessions-dir))

(defun my-ai--attachments-root ()
  "Parent directory of per-chat attachment folders, expanded."
  (expand-file-name my-desktop-ai-attachments-dir))

(defun my-ai--chat-dir ()
  "Create (if needed) and return this chat's attachments folder."
  (unless my-ai-chat-dir
    (setq my-ai-chat-dir
          (expand-file-name (or my-ai-session-uuid
                                (my-ai--session-uuid))
                            (my-ai--attachments-root))))
  (make-directory my-ai-chat-dir t)
  my-ai-chat-dir)

(defun my-ai--session-uuid ()
  "Return a short random hex identifier for a session file."
  (substring (md5 (format "%s%s" (current-time) (random t))) 0 8))

(defun my-ai-session-save ()
  "Save the current gptel buffer to its session file."
  (interactive)
  (unless (and (derived-mode-p 'org-mode) my-ai-session-file)
    (user-error "Save AI chats from their chat buffers"))
  (make-directory (file-name-directory my-ai-session-file) t)
  (write-region (point-min) (point-max) my-ai-session-file nil 'silent)
  (message "[desktop] AI chat saved: %s"
           (file-name-nondirectory my-ai-session-file)))

(defun my-ai--session-sync ()
  "Update the database row and the history index of the current
chat: backend, model, active tools and system message are
snapshotted so `my-ai-session-open' can restore them."
  (let* ((backend (bound-and-true-p gptel-backend))
         (model (bound-and-true-p gptel-model))
         (message (bound-and-true-p gptel--system-message))
         (tools (mapcar #'gptel-tool-name
                        (or (bound-and-true-p gptel-tools)
                            (default-value 'gptel-tools)))))
    (my-ai--db-execute
     "UPDATE session SET updated_at = ?, backend = ?, model = ?,
       tools = ?, directive = ? WHERE id = ?"
     (my-ai--now)
     (and backend (gptel-backend-name backend))
     (and model (symbol-name (if (consp model) (car model) model)))
     (json-encode tools)
     message
     my-ai-session-id)
    (my-ai--history-sync)))

(defun my-ai--session-autosave (&optional _beg _end)
  "Auto-save the gptel buffer and sync its database row after
each response."
  (when (and my-ai-session-id
             (derived-mode-p 'org-mode)
             (bound-and-true-p gptel-mode))
    (ignore-errors
      (my-ai-session-save)
      (my-ai--session-sync))))

(add-hook 'gptel-post-response-functions #'my-ai--session-autosave)

(defun my-ai-session-rename (name)
  "Rename the current AI chat to NAME.
Only the display title in the database changes; the transcript
file and the attachments folder keep their permanent names."
  (interactive
   (list (read-string "Rename AI chat to: "
                      (when my-ai-session-uuid
                        my-ai-session-uuid))))
  (unless my-ai-session-id
    (user-error "Not in an AI chat buffer"))
  (let ((title (string-trim name)))
    (when (string-empty-p title)
      (user-error "Empty chat name"))
    (my-ai--db-execute "UPDATE session SET title = ? WHERE id = ?"
                       title my-ai-session-id)
    (rename-buffer (format "*AI: %s*" title) t)
    (message "[desktop] AI chat renamed: %s" title)))

(defun my-ai-session-open ()
  "Resume a saved AI chat by name.
Restores the session's backend, model, tools and system message
from the database, and reconnects it to its attachments folder."
  (interactive)
  (require 'gptel)
  (let* ((rows (my-ai--db-select
                "SELECT id, uuid, title, file, backend, model, tools,
                        directive, in_history, updated_at
                 FROM session WHERE status = 'active'
                 ORDER BY COALESCE(updated_at, created_at) DESC"))
         (labels
          (mapcar
           (lambda (row)
             (pcase-let ((`(,_id ,uuid ,title ,_file ,_backend ,_model
                                 ,_tools ,_directive ,_hist ,updated) row))
               (cons (format "%s  [%s]"
                             (if (string-empty-p title) uuid title)
                             (or (and updated (substring updated 0 10))
                                 "?"))
                     row)))
           rows)))
    (unless rows
      (user-error "No AI chats yet (C-c a a starts one)"))
    (let* ((label (completing-read "Resume AI chat: "
                                   (mapcar #'car labels) nil t))
           (row (cdr (assoc label labels))))
      (pcase-let ((`(,id ,uuid ,title ,file ,backend-name ,model
                           ,tools-json ,directive ,in-history ,_upd) row))
        (find-file file)
        (gptel-mode 1)
        (setq my-ai-session-id id
              my-ai-session-uuid uuid
              my-ai-session-file (expand-file-name file)
              my-ai-chat-dir (expand-file-name uuid
                                               (my-ai--attachments-root))
              my-ai-in-history (= in-history 1))
        (setq-local gptel-context nil)
        (when-let* ((backend (and backend-name
                                  (cdr (assoc backend-name
                                              my-ai--backends)))))
          (setq-local gptel-backend backend))
        (when model
          (setq-local gptel-model (intern model)))
        (when (and tools-json (not (string-empty-p tools-json)))
          (let ((names (json-parse-string tools-json
                                          :array-type 'list)))
            (setq-local gptel-use-tools t)
            (setq-local gptel-tools
                        (seq-filter
                         (lambda (tool)
                           (member (gptel-tool-name tool) names))
                         (default-value 'gptel-tools)))))
        (when (and directive (not (string-empty-p directive)))
          (setq-local gptel--system-message directive))
        (rename-buffer (format "*AI: %s*"
                               (if (string-empty-p title) uuid title))
                       t)
        (message "[desktop] AI chat resumed (history: %s)"
                 (if my-ai-in-history "on" "off"))))))

(defun my-ai-session-search (query)
  "Search all saved AI sessions for QUERY (grep)."
  (interactive "sSearch AI sessions: ")
  (require 'grep)
  (grep (format "grep -nH --color=auto -i -e %s %s"
                (shell-quote-argument query)
                (shell-quote-argument
                 (concat (file-name-as-directory (my-ai--sessions-dir))
                         "*.org")))))

;; ------------------------------------------------------------------
;; Attachments: pick a file, convert to clean text, add to context
;; ------------------------------------------------------------------

(defun my-ai--markitdown ()
  "Locate the markitdown executable, or nil."
  (or (executable-find my-desktop-ai-markitdown)
      (let ((p (expand-file-name
                (concat "~/.local/bin/" my-desktop-ai-markitdown))))
        (and (file-executable-p p) p))))

(defun my-ai--convert-to-text (file out)
  "Convert FILE to markdown text with markitdown and write OUT.
Return the character count."
  (let ((bin (my-ai--markitdown))
        (outbuf (generate-new-buffer " *markitdown out*"))
        (errbuf (generate-new-buffer " *markitdown err*"))
        text)
    (unless bin
      (user-error "markitdown not found -- install it with: \
uv tool install \"markitdown[all]\""))
    (unwind-protect
        (progn
          (call-process bin nil (list outbuf errbuf) nil file)
          (with-current-buffer outbuf
            (setq text (string-trim (buffer-string)))))
      (kill-buffer outbuf)
      (kill-buffer errbuf))
    (when (string-empty-p text)
      (user-error "markitdown produced no text for %s" file))
    (when (> (length text) my-desktop-ai-attachment-max-size)
      (user-error "Converted text of %s is too large (%d characters)"
                  file (length text)))
    (write-region text nil out nil 'silent)
    (length text)))

(defun my-ai-attach-file (file)
  "Attach FILE to the current AI chat.
Plain text files are copied into this chat's folder as-is;
binary documents (pdf, docx, xlsx, ...) are converted to clean
markdown text with markitdown -- raw containers are never stored;
images are attached as media when the model can see them.  The
stored file is added to gptel context, so its content is sent
with every message.  Attaching a file under an existing name
overwrites the previous copy."
  (interactive
   (list (read-file-name "Attach file to chat: " nil nil t)))
  (unless my-ai-session-id
    (user-error "Not in an AI chat buffer (C-c a a or C-c a o)"))
  (setq file (expand-file-name file))
  (cond
   ((not (file-readable-p file))
    (user-error "Cannot read %s" file))
   ((file-directory-p file)
    (user-error "%s is a directory" file))
   ((> (nth 7 (file-attributes file))
       my-desktop-ai-attachment-max-size)
    (user-error "%s is larger than my-desktop-ai-attachment-max-size"
                file)))
  (let* ((dir (my-ai--chat-dir))
         (mime (mailcap-file-name-to-mime-type file))
         (image-p (string-prefix-p "image/" mime))
         (binary-p (gptel--file-binary-p file))
         (orig (file-name-nondirectory file))
         stored kind chars)
    (cond
     ;; Images: attach raw as media (needs a vision-capable model).
     ((and image-p binary-p)
      (unless (and (gptel--model-capable-p 'media)
                   (gptel--model-mime-capable-p mime))
        (user-error "Model cannot see images (%s)" mime))
      (setq stored (expand-file-name orig dir) kind "media")
      (copy-file file stored t))
     ;; Binary documents: markitdown -> clean markdown, original dropped.
     (binary-p
      (setq stored (expand-file-name
                    (concat (file-name-sans-extension orig) ".md")
                    dir)
            kind "text")
      (setq chars (my-ai--convert-to-text file stored)))
     ;; Plain text: store as-is.
     (t
      (setq stored (expand-file-name orig dir) kind "text")
      (copy-file file stored t)
      (setq chars (with-temp-buffer
                    (insert-file-contents stored)
                    (length (buffer-string))))))
    (my-ai--attachment-upsert orig (file-name-nondirectory stored)
                              kind chars)
    (gptel-add-file stored)
    (message "[desktop] AI attached %s -> %s%s"
             orig (file-name-nondirectory stored)
             (if chars (format " (%d chars)" chars) ""))))

(defun my-ai-history-toggle ()
  "Toggle whether the current chat is indexed into shared history.
Indexed transcripts are searchable from any chat with
history_search / history_read; toggling off removes the chat from
the index."
  (interactive)
  (unless my-ai-session-id
    (user-error "Not in an AI chat buffer"))
  (setq my-ai-in-history (not my-ai-in-history))
  (my-ai--db-execute "UPDATE session SET in_history = ? WHERE id = ?"
                     (if my-ai-in-history 1 0)
                     my-ai-session-id)
  (my-ai--history-sync)
  (message "[desktop] AI history: %s"
           (if my-ai-in-history "ON" "OFF")))

;; ------------------------------------------------------------------
;; gptel wiring
;; ------------------------------------------------------------------

(use-package gptel
  :ensure t
  :commands (gptel gptel-send gptel-menu)
  :config
  ;; Attachment sandbox root (per-chat folders live below it).
  (condition-case nil
      (make-directory my-desktop-ai-attachments-dir t)
    (error nil))
  (unless (sqlite-available-p)
    (my-desktop--warn
     "AI: Emacs lacks sqlite support -- chats, memory and history are disabled"))

  ;; Backends: the first spec with a present API key wins.
  (let (first-backend first-model)
    (dolist (spec (append my-desktop-ai-backends nil))
      (let* ((name (plist-get spec :name))
             (type (or (plist-get spec :type) 'openai))
             (host (plist-get spec :host))
             (endpoint (plist-get spec :endpoint))
             (key-env (plist-get spec :key-env))
             (models (append (plist-get spec :models) nil))
             (stream (plist-get spec :stream)))
        (cond
         ((and name (not (memq type '(openai deepseek))))
          (my-desktop--warn "AI backend %s: unknown :type %S" name type))
         ((and name key-env (not (getenv key-env)))
          (my-desktop--warn
           "AI backend %s skipped: $%s is not set" name key-env))
         ((and name key-env
               (or (eq type 'deepseek)
                   (and host models)))
          (condition-case err
              (let ((backend (my-ai--make-backend
                              name type host endpoint key-env
                              models stream)))
                (when backend
                  (push (cons name backend) my-ai--backends))
                (unless first-backend
                  (setq first-backend backend
                        first-model
                        (or (car models)
                            (let ((m (car (gptel-backend-models backend))))
                              (if (consp m) (car m) m))))))
            (error (my-desktop--warn "AI backend %s failed: %s"
                                     name
                                     (error-message-string err)))))
         (t (my-desktop--warn
             "AI backend ignored: needs :name :key-env and (:host :models for :type openai)")))))
    (when first-backend
      (setq gptel-backend first-backend
            gptel-model first-model
            gptel-default-mode 'org-mode)))

  ;; Personas.
  (setq gptel-directives
        (append (copy-alist my-desktop-ai-directives) nil))

  ;; Tools: the allowlist IS the sandbox.
  (setq gptel-tools
        (append
         (list
          (gptel-make-tool
           :function #'my-ai--read-attachment
           :name "read_attachment"
           :description
           "Read a text file attached to THIS chat.  Only the files
of the current chat's attachments folder exist for you; other
chats' files are unreachable.  Use list_attachments to see what
is available."
           :args (list '(:name "path" :type string
                               :description
                               "file name in this chat's folder"))
           :category "desktop")
          (gptel-make-tool
           :function #'my-ai--list-attachments
           :name "list_attachments"
           :description
           "List the files attached to THIS chat: stored name,
original name, kind (text or media), size and time added."
           :args nil
           :category "desktop")
          (gptel-make-tool
           :function #'my-ai--memory-save
           :name "memory_save"
           :description
           "Save a durable fact or preference to long-term memory,
shared across all chats.  Use it for distilled facts like 'user
prefers X' or 'never mention Y' -- not for chit-chat."
           :args (list '(:name "text" :type string
                               :description "the fact to remember"))
           :category "desktop")
          (gptel-make-tool
           :function #'my-ai--memory-search
           :name "memory_search"
           :description
           "Search long-term memory for stored facts containing the
query.  Check it whenever a user preference or a past decision
might matter."
           :args (list '(:name "query" :type string
                               :description "text to search for"))
           :category "desktop")
          (gptel-make-tool
           :function #'my-ai--history-search
           :name "history_search"
           :description
           "Search the user's opted-in past chats (shared history)
and return session ids, titles, dates, matching snippets,
attached file names and transcript paths.  Use it when the user
refers to earlier conversations or when prior context may exist.
Links are informational: other chats' files cannot be read, only
their indexed transcript text (via history_read)."
           :args (list '(:name "query" :type string
                               :description "text to search for"))
           :category "history")
          (gptel-make-tool
           :function #'my-ai--history-read
           :name "history_read"
           :description
           "Read the indexed transcript of a past chat by its
session id (from history_search).  Long transcripts are returned
in chunks: when the result says 'to continue, call again with
offset N', call this tool again with the same session id and
:offset N to read the next part."
           :args (list '(:name "session-id" :type integer
                               :description
                               "session id from history_search")
                       '(:name "offset" :type integer :optional t
                               :description "character offset to \
continue reading a previously opened transcript"))
           :category "history")
          ;; Fetch a known URL: readable main content, paged.
          (gptel-make-tool
           :function #'my-ai--web-fetch
           :name "web_fetch"
           :description
           "Fetch a web page by exact URL and return its readable \
main text (extracted with trafilatura).  ALWAYS use this when a \
URL is known (given by the user or found via web_search); use \
web_search only to discover URLs.  Long pages are returned in \
chunks of ~10k characters: when the result says 'to continue, \
call again with offset N', call this tool again with the same \
URL and :offset N to read the next part."
           :args (list '(:name "url" :type string
                               :description "http(s) URL to fetch")
                       '(:name "offset" :type integer :optional t
                               :description "character offset to \
continue reading a previously fetched page"))
           :category "web"))
         (when (and (eq my-desktop-ai-search 'searxng)
                    my-desktop-ai-searxng-url)
           (list
            (gptel-make-tool
             :function #'my-ai--searxng
             :name "web_search"
             :description
             "Search the web and return titles, URLs and snippets."
             :args (list '(:name "query" :type string
                                 :description "search query"))
             :category "web"))))))

  ;; "Attach chat file" next to gptel's own context commands.
  (with-eval-after-load 'gptel-transient
    (transient-append-suffix 'gptel-menu 'gptel--infix-context-add-file
      '("A" "Attach chat file" my-ai-attach-file)))

(defun my-ai-chat ()
  "Open a new gptel chat buffer.
Each call starts a fresh session; existing chat buffers are left
alone.  Resume saved sessions with `my-ai-session-open'."
  (interactive)
  (unless (fboundp 'gptel)
    (user-error "gptel is disabled or not installed"))
  ;; Loading gptel runs its :config, which builds the backends from
  ;; `my-desktop-ai-backends' -- the check below is only meaningful
  ;; after that.
  (require 'gptel)
  (unless (bound-and-true-p gptel-backend)
    (let ((keys (mapconcat
                 (lambda (spec)
                   (format "$%s (%s)"
                           (plist-get spec :key-env)
                           (if (getenv (plist-get spec :key-env))
                               "set" "NOT SET")))
                 (append my-desktop-ai-backends nil)
                 ", ")))
      (user-error
       (concat
        "No AI backend available.  Keys: "
        (if (string-empty-p keys)
            "none configured -- fill my-desktop-ai-backends in "
          keys)
        ".  If a variable is NOT SET: GNOME-launched Emacs does not "
        "read ~/.bashrc -- use ~/.config/environment.d/ or launch "
        "from a terminal"))))
  ;; A unique buffer name per invocation: C-c a a always starts a
  ;; fresh concurrent chat instead of switching to an existing one.
  ;; Resume saved chats with `my-ai-session-open' (C-c a o).
  ;; interactivep t makes gptel display the new buffer.  (A bare
  ;; (gptel) would pass no buffer name and do nothing.)
  (let* ((uuid (my-ai--session-uuid))
         (file (expand-file-name (concat uuid ".org")
                                 (my-ai--sessions-dir)))
         (buf (gptel (generate-new-buffer-name
                      (format "*gptel-%s %s*"
                              (gptel-backend-name gptel-backend)
                              (format-time-string "%Y-%m-%d %H:%M")))
                     nil nil t)))
    (with-current-buffer buf
      (setq my-ai-session-file file
            my-ai-session-uuid uuid
            my-ai-session-id (my-ai--session-create uuid file)
            my-ai-in-history my-desktop-ai-history-default)
      ;; Context (attached files) is per chat, never global.
      (setq-local gptel-context nil))
    buf))

;; ------------------------------------------------------------------
;; AI usage dashboard (C-c a z): Z-AI plan + DeepSeek account,
;; Claude subscription limits in a column on the right
;; ------------------------------------------------------------------
;; https://api.z.ai/api/monitor/usage/quota/limit ->
;;   limits: (type "TOKENS_LIMIT" unit 3 number 5) = 5-hour window,
;;           (type "TOKENS_LIMIT" unit 6 number 1) = weekly window;
;;   nextResetTime is epoch-milliseconds, so countdowns are exact in
;;   any timezone.
;; https://api.z.ai/api/monitor/usage/model-usage?startTime=... ->
;;   hourly buckets whose labels live in the SERVER's frame (UTC+8)
;;   and whose tail is clamped at server "now", regardless of the
;;   timestamps sent.  So: ask for a generous span and keep only the
;;   trailing 24 hourly buckets, comparing label differences.  No
;;   absolute wall-clock times are ever displayed.

(defun my-ai-zai--request (path)
  "GET PATH from the Z-AI monitor API; return its `data' object."
  (let* ((key (or (getenv my-desktop-ai-zai-api-key-env)
                  (user-error "Env var %s is not set (Z-AI API key)"
                              my-desktop-ai-zai-api-key-env)))
         (url-request-extra-headers
          `(("Authorization" . ,(concat "Bearer " key))))
         (resp (my-ai--http-body
                (concat my-desktop-ai-zai-api-url path))))
    (unless resp
      (user-error "Z-AI usage: request failed (no response)"))
    (unless (= (car resp) 200)
      (user-error "Z-AI usage: HTTP %d" (car resp)))
    (let* ((json (json-parse-string (cdr resp)
                                    :object-type 'alist
                                    :array-type 'list))
           (code (alist-get 'code json)))
      (unless (equal code 200)
        (user-error "Z-AI usage: API error %s (%s)"
                    code (or (alist-get 'msg json) "?")))
      (alist-get 'data json))))

(defun my-ai-zai--tokens-limit (limits unit number)
  "Return the TOKENS_LIMIT entry with UNIT and NUMBER from LIMITS."
  (seq-find (lambda (limit)
              (and (string= (alist-get 'type limit) "TOKENS_LIMIT")
                   (equal (alist-get 'unit limit) unit)
                   (equal (alist-get 'number limit) number)))
            limits))

(defun my-ai-zai--bar (pct)
  "Return a 10-slot filled bar for PCT (0..100)."
  (let* ((pct (min 100 (max 0 (round (or pct 0)))))
         (filled (round (/ pct 10.0))))
    (concat (make-string filled ?\u2588)
            (make-string (- 10 filled) ?\u2591))))

(defun my-ai-zai--human-tokens (n)
  "Format token count N compactly (K/M/B)."
  (cond ((>= n 1.0e9) (format "%.1fB" (/ n 1.0e9)))
        ((>= n 1.0e6) (format "%.1fM" (/ n 1.0e6)))
        ((>= n 1.0e3) (format "%.1fK" (/ n 1.0e3)))
        (t (format "%d" n))))

(defun my-ai-zai--reset-in (ms)
  "Humanize epoch-milliseconds MS into a reset countdown.
Deltas between now and MS are timezone-independent."
  (let* ((secs (max 0 (floor (- (/ ms 1000.0) (float-time)))))
         (days (/ secs 86400))
         (hours (% (/ secs 3600) 24))
         (mins (% (/ secs 60) 60)))
    (if (>= days 1)
        (format "%dd %dh" days hours)
      (format "%dh %02dm" hours mins))))

(defun my-ai-zai--quota-line (label entry)
  "Format one quota row: LABEL, bar, percent and reset countdown."
  (if (not entry)
      (format "%-4s  (not reported)" label)
    (format "%-4s  %s %3d%%  resets in %s"
            label
            (my-ai-zai--bar (alist-get 'percentage entry))
            (round (or (alist-get 'percentage entry) 0))
            (my-ai-zai--reset-in
             (or (alist-get 'nextResetTime entry) 0)))))

(defun my-ai-zai--label-time (label)
  "Parse an x_time LABEL (like 2026-09-03 19:00) to a time value.
The label frame is unknown (server-local), so it is decoded as
UTC; only differences between labels are ever used."
  (let ((parts (split-string label "[-: ]" t)))
    (encode-time 0
                 (string-to-number (nth 4 parts))
                 (string-to-number (nth 3 parts))
                 (string-to-number (nth 2 parts))
                 (string-to-number (nth 1 parts))
                 (string-to-number (nth 0 parts))
                 t)))

(defun my-ai-zai--last-24h ()
  "Fetch usage for the trailing 24 hours (24 hourly buckets).
Return (TOKENS CALLS MODEL-PAIRS); MODEL-PAIRS is an alist
((MODEL-NAME . TOKENS-IN-24H) ...)."
  (let* ((stamp (lambda (time)
                  (replace-regexp-in-string
                   " " "%20"
                   (format-time-string "%Y-%m-%d %H:%M:%S" time t))))
         (query (concat
                 "?startTime="
                 (funcall stamp (time-subtract (current-time)
                                               (seconds-to-time
                                                (* 36 3600))))
                 "&endTime="
                 (funcall stamp (time-add (current-time)
                                          (seconds-to-time
                                           (* 12 3600))))))
         (data (my-ai-zai--request
                (concat "/api/monitor/usage/model-usage" query)))
         (labels (append (alist-get 'x_time data) nil))
         (calls-arr (append (alist-get 'modelCallCount data) nil))
         (tokens-arr (append (alist-get 'tokensUsage data) nil))
         (last (and labels (my-ai-zai--label-time (car (last labels)))))
         ;; keep 24 buckets ending at the newest one (inclusive)
         (cutoff (and last (time-subtract last
                                          (seconds-to-time
                                           (* 23 3600)))))
         (keep (when cutoff
                 (seq-filter
                  (lambda (idx)
                    (not (time-less-p
                          (my-ai-zai--label-time (nth idx labels))
                          cutoff)))
                  (number-sequence 0 (1- (length labels))))))
         (pick (lambda (arr)
                 (mapcar (lambda (idx) (or (nth idx arr) 0)) keep)))
         (models (mapcar
                  (lambda (model)
                    (cons (alist-get 'modelName model)
                          (apply #'+ (funcall pick
                                              (append (alist-get
                                                       'tokensUsage
                                                       model)
                                                      nil)))))
                  (append (alist-get 'modelDataList data) nil))))
    (list (apply #'+ (funcall pick tokens-arr))
          (apply #'+ (funcall pick calls-arr))
          models)))

(defun my-ai-zai--model-names (models)
  "Return up to 3 MODEL-PAIRS as \"name (tokens)\", largest first."
  (mapconcat
   (lambda (model)
     (format "%s (%s)" (car model)
             (my-ai-zai--human-tokens (cdr model))))
   (seq-take (seq-sort (lambda (a b) (> (cdr a) (cdr b))) models)
             3)
   ", "))

(defun my-ai-zai-usage--dashboard ()
  "Build the Z-AI coding-plan usage dashboard text."
  (let* ((quota (my-ai-zai--request
                 "/api/monitor/usage/quota/limit"))
         (level (alist-get 'level quota))
         (limits (alist-get 'limits quota))
         (hourly (my-ai-zai--last-24h)))
    (string-join
     (delq nil
           (list
            (concat "Z-AI coding plan"
                    (if level (format " (%s)" level) ""))
            (my-ai-zai--quota-line
             "5h" (my-ai-zai--tokens-limit limits 3 5))
            (my-ai-zai--quota-line
             "week" (my-ai-zai--tokens-limit limits 6 1))
            (format "24h  %s tokens, %d calls%s"
                    (my-ai-zai--human-tokens (nth 0 hourly))
                    (nth 1 hourly)
                    (if (nth 2 hourly)
                        (concat "  "
                                (my-ai-zai--model-names
                                 (nth 2 hourly)))
                      ""))))
     "\n")))

;; https://api-docs.deepseek.com/api/get-user-balance ->
;;   {is_available: bool, balance_infos: [{currency, total_balance,
;;                                        granted_balance, topped_up_balance}]}
;; Peak windows are UTC, Monday-Friday, except Chinese public holidays
;; (off-peak all day).  Ranges are half-open [start, end).

(defconst my-ai-deepseek--currency-symbols
  '(("USD" . "$") ("CNY" . "\u00a5"))
  "Currency signs for the DeepSeek balance line.")

(defun my-ai-deepseek--request ()
  "GET /user/balance from DeepSeek; return the parsed response."
  (let* ((key (or (getenv my-desktop-ai-deepseek-api-key-env)
                  (user-error "Env var %s is not set (DeepSeek API key)"
                              my-desktop-ai-deepseek-api-key-env)))
         (url-request-extra-headers
          `(("Authorization" . ,(concat "Bearer " key))
            ("Accept" . "application/json")))
         (resp (my-ai--http-body
                (concat my-desktop-ai-deepseek-api-url "/user/balance"))))
    (unless resp
      (user-error "DeepSeek balance: request failed (no response)"))
    (unless (= (car resp) 200)
      (user-error "DeepSeek balance: HTTP %d" (car resp)))
    (json-parse-string (cdr resp)
                       :object-type 'alist
                       :array-type 'list)))

(defun my-ai-deepseek--money (amount currency)
  "Format AMOUNT with the sign for CURRENCY."
  (format "%s%s"
          (or (cdr (assoc-string currency
                                 my-ai-deepseek--currency-symbols))
              (concat currency " "))
          amount))

(defun my-ai-deepseek--balance-line (data)
  "Return the total-balance line from the balance API response DATA."
  (let ((infos (alist-get 'balance_infos data)))
    (if (null infos)
        "balance  (not reported)"
      (concat "balance  "
              (mapconcat (lambda (info)
                           (my-ai-deepseek--money
                            (alist-get 'total_balance info)
                            (alist-get 'currency info)))
                         infos " + ")))))

(defun my-ai-deepseek-peak-p (&optional time)
  "Non-nil when TIME is inside a DeepSeek peak window.
TIME defaults to now and is interpreted in UTC."
  (let* ((time (or time (current-time)))
         (decoded (decode-time time t))
         (dow (nth 6 decoded))
         (hour (nth 2 decoded))
         (date (format-time-string "%Y-%m-%d" time t)))
    (and (>= dow 1) (<= dow 5)
         (not (member date my-desktop-ai-deepseek-cn-holidays))
         (seq-some (lambda (window)
                     (and (>= hour (car window))
                          (< hour (cdr window))))
                   my-desktop-ai-deepseek-peak-windows))))

(defun my-ai-deepseek--pricing-line (&optional time)
  "Return the peak/off-peak line for TIME (default: now)."
  (if (my-ai-deepseek-peak-p time)
      "pricing  peak now (full rate)"
    "pricing  off-peak now (50% off)"))

(defun my-ai-deepseek--dashboard ()
  "Build the DeepSeek account dashboard text."
  (let ((data (my-ai-deepseek--request)))
    (string-join
     (list "DeepSeek account"
           (my-ai-deepseek--balance-line data)
           (my-ai-deepseek--pricing-line))
     "\n")))

;; https://api.anthropic.com/api/oauth/usage (undocumented; the endpoint
;; behind Claude Code /usage).  Auth is the subscription OAuth token
;; plus `anthropic-beta: oauth-2025-04-20'; a Console API key does not
;; work.  Response ->
;;   limits: [{kind "session"|"weekly_all"|"weekly_scoped", percent,
;;             resets_at ISO-8601, scope {model {display_name}}}],
;;   five_hour / seven_day / seven_day_opus / seven_day_sonnet:
;;             {utilization, resets_at} (older shape, used as fallback),
;;   extra_usage: {is_enabled, used_credits, monthly_limit,
;;                 decimal_places, currency}.
;; The token is only read: Claude Code (host or run-claude container)
;; refreshes ~/.claude/.credentials.json whenever it runs.

(defconst my-ai-claude--refresh-hint
  "OAuth token expired; run claude once to refresh it"
  "Message shown when the Claude subscription token is stale.")

(defun my-ai-claude--credentials ()
  "Return the `claudeAiOauth' alist of the Claude Code credentials."
  (let ((file (expand-file-name my-desktop-ai-claude-credentials-file)))
    (unless (file-readable-p file)
      (user-error "Claude usage: no %s; run claude /login" file))
    (or (alist-get 'claudeAiOauth
                   (json-parse-string
                    (with-temp-buffer
                      (insert-file-contents file)
                      (buffer-string))
                    :object-type 'alist :array-type 'list
                    :null-object nil :false-object nil))
        (user-error "Claude usage: no OAuth login in %s" file))))

(defun my-ai-claude--token ()
  "Return (TOKEN . CREDENTIALS) for the Claude usage endpoint.
CREDENTIALS is nil when the token comes from
`my-desktop-ai-claude-oauth-token-env'."
  (let ((env (getenv my-desktop-ai-claude-oauth-token-env)))
    (if (and env (not (string-empty-p env)))
        (cons env nil)
      (let* ((creds (my-ai-claude--credentials))
             (token (alist-get 'accessToken creds))
             (expires (alist-get 'expiresAt creds)))
        (unless (stringp token)
          (user-error "Claude usage: no access token; run claude /login"))
        (when (and (numberp expires) (< (/ expires 1000.0) (float-time)))
          (user-error "Claude usage: %s" my-ai-claude--refresh-hint))
        (cons token creds)))))

(defun my-ai-claude--request (token)
  "GET the Claude subscription usage with TOKEN; return the parsed JSON."
  (let* ((url-request-extra-headers
          `(("Authorization" . ,(concat "Bearer " token))
            ("anthropic-beta" . "oauth-2025-04-20")
            ("Accept" . "application/json")))
         (resp (my-ai--http-body my-desktop-ai-claude-usage-url)))
    (unless resp
      (user-error "Claude usage: request failed (no response)"))
    (when (= (car resp) 401)
      (user-error "Claude usage: %s" my-ai-claude--refresh-hint))
    (unless (= (car resp) 200)
      (user-error "Claude usage: HTTP %d" (car resp)))
    (json-parse-string (cdr resp) :object-type 'alist :array-type 'list
                       :null-object nil :false-object nil)))

(defun my-ai-claude--reset-in (iso)
  "Humanize the ISO-8601 time ISO into a reset countdown, or nil."
  (when-let* ((time (and (stringp iso)
                         (ignore-errors (date-to-time iso)))))
    (my-ai-zai--reset-in (* 1000 (float-time time)))))

(defun my-ai-claude--limit-rows (data)
  "Return the limit rows of usage DATA as (LABEL PERCENT RESETS-AT)."
  (if-let* ((limits (alist-get 'limits data)))
      (mapcar
       (lambda (limit)
         (let ((kind (alist-get 'kind limit))
               (scope (alist-get 'scope limit)))
           (list (pcase kind
                   ("session" "5h")
                   ("weekly_all" "week")
                   (_ (truncate-string-to-width
                       (format "%s"
                               (or (map-nested-elt scope
                                                   '(model display_name))
                                   (alist-get 'surface scope)
                                   kind))
                       5)))
                 (alist-get 'percent limit)
                 (alist-get 'resets_at limit))))
       limits)
    (delq nil
          (mapcar (lambda (pair)
                    (when-let* ((entry (alist-get (car pair) data))
                                (pct (alist-get 'utilization entry)))
                      (list (cdr pair) pct (alist-get 'resets_at entry))))
                  '((five_hour . "5h") (seven_day . "week")
                    (seven_day_opus . "opus") (seven_day_sonnet . "son."))))))

(defun my-ai-claude--limit-line (row)
  "Format one limit ROW like the Z-AI quota rows."
  (pcase-let ((`(,label ,pct ,resets) row))
    (format "%-5s %s %3d%%%s"
            label
            (my-ai-zai--bar pct)
            (round (or pct 0))
            (if-let* ((in (my-ai-claude--reset-in resets)))
                (concat "  resets in " in)
              ""))))

(defun my-ai-claude--extra-line (data)
  "Return the extra-usage (paid overflow) line of usage DATA."
  (let* ((extra (alist-get 'extra_usage data))
         (scale (expt 10.0 (or (alist-get 'decimal_places extra) 0)))
         (used (alist-get 'used_credits extra))
         (limit (alist-get 'monthly_limit extra)))
    (cond
     ((not (eq (alist-get 'is_enabled extra) t)) "extra usage  off")
     ((and (numberp used) (numberp limit))
      (format "extra usage  on, %.2f of %.2f %s"
              (/ used scale) (/ limit scale)
              (or (alist-get 'currency extra) "")))
     (t "extra usage  on"))))

(defun my-ai-claude--dashboard ()
  "Build the Claude subscription limits dashboard text."
  (let* ((auth (my-ai-claude--token))
         (creds (cdr auth))
         (plan (alist-get 'subscriptionType creds))
         (tier (alist-get 'rateLimitTier creds))
         (mult (and (stringp tier)
                    (string-match "_\\([0-9]+x\\)\\'" tier)
                    (match-string 1 tier)))
         (data (my-ai-claude--request (car auth)))
         (rows (my-ai-claude--limit-rows data)))
    (string-join
     (append
      (list (concat "Claude subscription"
                    (if plan
                        (format " (%s%s)" plan (if mult (concat " " mult) ""))
                      "")))
      (if rows
          (mapcar #'my-ai-claude--limit-line rows)
        (list "limits  (not reported)"))
      (list (my-ai-claude--extra-line data)))
     "\n")))

(defun my-ai-usage--section (fn)
  "Call FN and return its text; on error return an ERROR line."
  (condition-case err
      (funcall fn)
    (error (format "ERROR: %s" (error-message-string err)))))

(defconst my-ai-usage--separator
  "========================================"
  "Line between the stacked dashboard sections.")

(defun my-ai-usage--columns (left right)
  "Put the RIGHT text block beside the LEFT one, or nil if too wide.
Each LEFT line is padded to the widest one; the frame width decides
whether the result fits without wrapping."
  (let* ((left-lines (split-string left "\n"))
         (right-lines (split-string right "\n"))
         (width (apply #'max (mapcar #'string-width left-lines)))
         (right-width (apply #'max (mapcar #'string-width right-lines)))
         (rows (max (length left-lines) (length right-lines))))
    (when (< (+ width 3 right-width) (frame-width))
      (mapconcat
       (lambda (idx)
         (string-trim-right
          (concat (string-pad (or (nth idx left-lines) "") width)
                  " │ "
                  (or (nth idx right-lines) ""))))
       (number-sequence 0 (1- rows))
       "\n"))))

(defun my-ai-usage--dashboard ()
  "Build the usage dashboard: Z-AI and DeepSeek, Claude on the right.
The Claude column falls back to a stacked section when the frame is
too narrow; `my-desktop-ai-enable-claude-usage' turns it off."
  (let ((left (string-join
               (list (my-ai-usage--section #'my-ai-zai-usage--dashboard)
                     ""
                     my-ai-usage--separator
                     ""
                     (my-ai-usage--section #'my-ai-deepseek--dashboard))
               "\n")))
    (if (not my-desktop-ai-enable-claude-usage)
        left
      (let ((right (my-ai-usage--section #'my-ai-claude--dashboard)))
        (or (my-ai-usage--columns left right)
            (string-join (list left "" my-ai-usage--separator "" right)
                         "\n"))))))

(defun my-ai-usage ()
  "Show Z-AI, DeepSeek and Claude subscription usage in the minibuffer."
  (interactive)
  (message "%s" (my-ai-usage--dashboard)))

(provide 'desktop-ai)
;;; desktop-ai.el ends here
