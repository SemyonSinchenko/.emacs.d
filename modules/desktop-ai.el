;;; desktop-ai.el --- gptel-based assistant, hard-restricted -*- lexical-binding: t; -*-

;;; Commentary:
;; AnythingLLM replacement on top of gptel.  The assistant has exactly
;; these tools, nothing else (no shell, no arbitrary filesystem):
;;   read_attachment - read a file from my-desktop-ai-attachments-dir
;;   memory_save     - append a fact to the memory Org file
;;   memory_search   - search the memory Org file
;;   web_search      - SearXNG query (only when configured)
;; Backends come from `my-desktop-ai-backends'; API keys ALWAYS come
;; from environment variables (:key-env), never from the config file.
;; :type is 'openai (default, any OpenAI-compatible API) or 'deepseek
;; (gptel's native backend, with built-in host/endpoint/models).

;;; Code:

(require 'json)
(require 'url)
(require 'seq)
(require 'desktop-config-defs)
(require 'desktop-core)

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
  "Read PATH from the attachments directory, with guards."
  (let* ((dir (expand-file-name my-desktop-ai-attachments-dir))
         (file (expand-file-name path)))
    (cond
     ((not (file-in-directory-p file dir))
      (format "ERROR: file must be inside %s" dir))
     ((not (file-readable-p file))
      (format "ERROR: cannot read %s" file))
     ((> (nth 7 (file-attributes file))
         my-desktop-ai-attachment-max-size)
      "ERROR: file too large")
     (t
      (with-temp-buffer
        (insert-file-contents file)
        (buffer-string))))))

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
  "Append TEXT to the memory Org file."
  (let ((file (expand-file-name my-desktop-ai-memory-file)))
    (make-directory (file-name-directory file) t)
    (with-temp-buffer
      (goto-char (point-max))
      (insert (format "* [%s] %s\n"
                      (format-time-string "%Y-%m-%d %a")
                      text))
      (write-region (point-min) (point-max) file 'append))
    "Saved to memory."))

(defun my-ai--memory-search (query)
  "Return memory lines containing QUERY."
  (let* ((file (expand-file-name my-desktop-ai-memory-file))
         (matches
          (when (file-exists-p file)
            (with-temp-buffer
              (insert-file-contents file)
              (let ((case-fold-search t) out)
                (goto-char (point-min))
                (while (re-search-forward
                        (regexp-quote query) nil t)
                  (forward-line 0)
                  (push (buffer-substring (line-beginning-position)
                                          (line-end-position))
                        out)
                  (forward-line 1))
                (nreverse out))))))
    (if matches
        (string-join matches "\n")
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

(defvar-local my-ai-session-file nil
  "File this gptel buffer persists to.  Set on first save.")

(defun my-ai--sessions-dir ()
  "Session directory, expanded."
  (expand-file-name my-desktop-ai-sessions-dir))

(defun my-ai--session-uuid ()
  "Return a short random hex identifier for a session file."
  (substring (md5 (format "%s%s" (current-time) (random t))) 0 8))

(defun my-ai-session-save ()
  "Save the current gptel buffer to its session file."
  (interactive)
  (unless (derived-mode-p 'org-mode)
    (user-error "Save AI sessions from their org-mode chat buffers"))
  (unless my-ai-session-file
    (setq my-ai-session-file
          (expand-file-name
           (concat (format-time-string "%Y-%m-%dT%H-%M-%S")
                   "-" (my-ai--session-uuid) ".org")
           (my-ai--sessions-dir))))
  (make-directory (file-name-directory my-ai-session-file) t)
  (write-region (point-min) (point-max) my-ai-session-file nil 'silent)
  (message "[desktop] AI session saved: %s"
           (file-name-nondirectory my-ai-session-file)))

(defun my-ai--session-autosave (&optional _beg _end)
  "Auto-save the gptel buffer after each response."
  (when (and my-desktop-ai-sessions-dir
             (derived-mode-p 'org-mode)
             (bound-and-true-p gptel-mode))
    (ignore-errors (my-ai-session-save))))

(add-hook 'gptel-post-response-functions #'my-ai--session-autosave)

(defun my-ai-session-rename (name)
  "Rename the current buffer's AI session file to NAME.org.
New replies keep auto-saving to the renamed file."
  (interactive
   (list (read-string "Rename AI session to: "
                      (when my-ai-session-file
                        (file-name-base my-ai-session-file)))))
  (unless my-ai-session-file
    (user-error "No session file yet: save it first (C-c a s)"))
  (let* ((name (string-trim (file-name-sans-extension name)))
         (new-file (expand-file-name (concat name ".org")
                                     (my-ai--sessions-dir))))
    (when (string-empty-p name)
      (user-error "Empty session name"))
    (when (and (file-exists-p new-file)
               (not (yes-or-no-p (format "%s exists; overwrite? "
                                         new-file))))
      (user-error "Canceled"))
    (make-directory (file-name-directory new-file) t)
    (if (buffer-file-name)
        ;; Buffer visits the session file (resumed session).
        (progn
          (rename-file (buffer-file-name) new-file t)
          (set-visited-file-name new-file t t)
          (set-buffer-modified-p nil))
      ;; Transient chat buffer: move the file, keep writing to it.
      (when (file-exists-p my-ai-session-file)
        (rename-file my-ai-session-file new-file t))
      (setq my-ai-session-file new-file))
    (message "[desktop] AI session renamed: %s"
             (file-name-nondirectory new-file))))

(defun my-ai-session-open ()
  "Resume a saved AI session by name."
  (interactive)
  (let* ((files (directory-files (my-ai--sessions-dir) t "\\.org\\'"))
         (names (mapcar #'file-name-base files)))
    (unless files
      (user-error "No saved AI sessions in %s" (my-ai--sessions-dir)))
    (let* ((name (completing-read "Resume AI session: " names nil t))
           (file (seq-find (lambda (f)
                             (equal (file-name-base f) name))
                           files)))
      (find-file file)
      (gptel-mode 1)
      (setq my-ai-session-file (expand-file-name file))
      (message "[desktop] AI session resumed (new replies auto-save)"))))

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
;; gptel wiring
;; ------------------------------------------------------------------

(use-package gptel
  :ensure t
  :commands (gptel gptel-send gptel-menu)
  :config
  ;; Attachment sandbox directory.
  (condition-case nil
      (make-directory my-desktop-ai-attachments-dir t)
    (error nil))

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
           "Read a text file from the attachments directory. \
The user must place files there first."
           :args (list '(:name "path" :type string
                               :description
                               "path relative to the attachments dir"))
           :category "desktop")
          (gptel-make-tool
           :function #'my-ai--memory-save
           :name "memory_save"
           :description "Save a fact or preference to long-term memory."
           :args (list '(:name "text" :type string
                               :description "the fact to remember"))
           :category "desktop")
          (gptel-make-tool
           :function #'my-ai--memory-search
           :name "memory_search"
           :description
           "Search long-term memory for entries containing the query."
           :args (list '(:name "query" :type string
                               :description "text to search for"))
           :category "desktop")
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

(defun my-ai-chat ()
  "Open a gptel chat buffer."
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
  ;; One chat buffer per backend; interactivep t makes gptel display
  ;; it.  (A bare (gptel) would pass no buffer name and do nothing.)
  (gptel (format "*%s*" (gptel-backend-name gptel-backend))
         nil nil t))

(provide 'desktop-ai)
;;; desktop-ai.el ends here
