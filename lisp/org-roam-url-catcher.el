;;; org-roam-url-catcher.el --- URL capture for org-roam with LLM assistance -*- lexical-binding: t; -*-

;;; Commentary:
;; This module provides automated web article capture and integration into
;; `org-roam` personal knowledge management system.
;;
;; The pipeline:
;; 1. Fetch URL content using trafilatura CLI
;; 2. Sanitize text for token efficiency
;; 3. Extract metadata (umbrella nodes) from org-roam database
;; 4. Build prompts for LLM
;; 5. Request LLM to generate org-roam node content
;; 6. Validate and save the generated content
;;
;; Usage: M-x my/collect-url

;;; Code:

(require 'org)
(require 'org-roam)
(require 'org-id)
(require 'gptel)

;;; Configuration Variables

(defcustom my/url-catcher-max-chars 40000
  "Maximum number of characters to send to LLM after sanitization.
Text is truncated to this limit as the final step of sanitization."
  :type 'integer
  :group 'org-roam)

(defcustom my/url-catcher-umbrella-tag "umbrella"
  "Tag used to identify umbrella (semantic hub) nodes in org-roam.
Nodes with this tag are queried and provided to the LLM for linking."
  :type 'string
  :group 'org-roam)

(defconst my/url-catcher-error-buffer "*URL Catcher Error*"
  "Buffer name for displaying validation errors and raw LLM output.")

;;; URL Fetching

(defun my/url-catcher--fetch-url (url)
  "Fetch content from URL using trafilatura CLI.

Uses `trafilatura -u URL --markdown --no-comments --fast` to extract
readable content. Returns the raw markdown string on success, or nil
on failure.

Signals an error if trafilatura binary is not found."
  (unless (executable-find "trafilatura")
    (error "URL Catcher: trafilatura binary not found in PATH. Please install it via `pip install trafilatura`"))
  
  (let ((temp-buffer (generate-new-buffer " *trafilatura-temp*")))
    (unwind-protect
        (let ((exit-code
               (with-current-buffer temp-buffer
                 (call-process "trafilatura" nil temp-buffer nil
                               "-u" url "--markdown" "--no-comments" "--fast"))))
          (if (and (numberp exit-code) (= exit-code 0))
              (with-current-buffer temp-buffer
                (buffer-substring-no-properties (point-min) (point-max)))
            (message "URL Catcher: trafilatura failed with exit code %s" exit-code)
            nil))
      (when (buffer-live-p temp-buffer)
        (kill-buffer temp-buffer)))))

;;; Text Sanitization

(defun my/url-catcher--sanitize-text (raw-text)
  "Aggressively sanitize RAW-TEXT for maximum token efficiency.

Applies the following pipeline in order:
1. Remove lines containing only digits (e.g., code block line numbers).
2. Remove lines containing exactly one non-whitespace character (e.g., `}`).
3. Replace all newlines with a single space.
4. Replace multiple spaces/tabs with a single space.
5. Trim leading and trailing whitespace.
6. Truncate to `my/url-catcher-max-chars` limit.

Returns the sanitized string."
  (if (not raw-text)
      ""
    (let ((text raw-text))
      
      (setq text (replace-regexp-in-string "^[ \t]*[0-9]+[ \t]*\n" "" text))
      (setq text (replace-regexp-in-string "^[ \t]*[^ \t\n][ \t]*\n" "" text))
      (setq text (replace-regexp-in-string "\n" " " text))
      (setq text (replace-regexp-in-string "[ \t]+" " " text))
      (setq text (string-trim text))
      (when (and (boundp 'my/url-catcher-max-chars)
                 (> (length text) my/url-catcher-max-chars))
        (setq text (substring text 0 my/url-catcher-max-chars)))
      
      text)))

;;; Metadata Extraction

(defun my/url-catcher--get-umbrella-nodes ()
  "Query org-roam database for nodes tagged with `my/url-catcher-umbrella-tag`.

Returns an alist mapping node titles to their UUIDs (IDs).
Returns nil if no umbrella nodes exist or on database error."
  (condition-case err
      (let ((rows (org-roam-db-query
                   [:select [nodes:title nodes:id]
			    :from nodes
			    :inner-join tags :on (= nodes:id tags:node_id)
			    :where (= tags:tag $s1)]
                   my/url-catcher-umbrella-tag)))
        
        (when rows
          (mapcar (lambda (row) (cons (car row) (cadr row))) rows)))
    (error
     (message "URL Catcher: Failed to query umbrella nodes: %s" (error-message-string err))
     nil)))

;;; Prompt Generation

(defun my/url-catcher--build-system-prompt ()
  "Build the system prompt for LLM org-roam node generation.
Includes a comprehensive org-mode syntax cheat sheet to prevent markdown hallucinations."
  "You are a specialized Knowledge Management assistant. Your ONLY task is to output valid, raw `org-roam` node text based on the provided article.

CRITICAL REQUIREMENT: YOU MUST USE STRICT ORG-MODE SYNTAX. ABSOLUTELY NO MARKDOWN.

=== ORG-MODE SYNTAX CHEAT SHEET ===
- Headings: Use asterisks `* Heading 1`, `** Heading 2` (NEVER use `# Heading`).
- Bold: `*bold text*` (NEVER use `**bold**`).
- Italic: `/italic text/` (NEVER use `*italic*` or `_italic_`).
- Underline: `_underlined text_`.
- Strikethrough: `+strikethrough+` (NEVER use `~~strike~~`).
- Inline code: `=code=` or `~verbatim~` (NEVER use backticks ` ` `).
- Code blocks:
  #+begin_src language
  // code here
  #+end_src
  (NEVER use ```language ... ```).
- Blockquotes:
  #+begin_quote
  Quoted text here.
  #+end_quote
  (NEVER use `> quote`).
- External Links: `[[https://example.com][Link description]]` (NEVER use `[desc](url)`).
- Lists: Use `-` or `+` for unordered, and `1.` for ordered.

=== RULES FOR THIS TASK ===
1. NEVER wrap your overall response in markdown code blocks (e.g., do NOT start with ```org). Output raw text only.
2. At the very top, include a property drawer with the provided ID.
3. Below the property drawer, include:
   `#+title: <Article Title>`
   `#+ROAM_REFS: <Original URL>`
   `#+filetags: :article:`
4. Write a brief summary and bullet points extracting the core value of the article.
5. Link to the provided Umbrella Nodes IF AND ONLY IF highly relevant. Use exact IDs provided: `[[id:GIVEN-ID][Title]]`.
6. Structure the note with `* Summary` and `* Key Takeaways` sections.")

(defun my/url-catcher--make-slug (title)
  "Generate a URL-safe slug from TITLE for filename generation.

1. Downcase the title
2. Strip non-ASCII and non-alphanumeric characters (replace with `-`)
3. Trim leading/trailing hyphens
4. Truncate to 50 characters maximum

Returns the slug string."
  (let ((slug (downcase title)))
    ;; Strip non-ASCII and non-alphanumeric, replace with hyphen
    (setq slug (replace-regexp-in-string "[^a-z0-9]+" "-" slug))
    ;; Trim leading/trailing hyphens
    (setq slug (string-trim slug "-"))
    ;; Truncate to 50 characters
    (when (> (length slug) 50)
      (setq slug (substring slug 0 50)))
    slug))

(defun my/url-catcher--build-user-prompt (url sanitized-text umbrella-nodes-alist)
  "Build the user prompt for LLM org-roam node generation.

URL is the original article URL.
SANITIZED-TEXT is the cleaned article content.
UMBRELLA-NODES-ALIST is an alist of (title . id) for semantic hubs.

Generates a new org-roam ID and includes it in the expected format section."
  (let ((new-id (org-id-new))
        (umbrella-section ""))
    
    ;; Build umbrella nodes mapping section if available
    (when (and umbrella-nodes-alist (not (null umbrella-nodes-alist)))
      (setq umbrella-section
            (concat "UMBRELLA NODES MAP:\n"
                    (mapconcat
                     (lambda (node)
                       (format "  - %s: [[id:%s][%s]]" (car node) (cdr node) (car node)))
                     umbrella-nodes-alist
                     "\n")
                    "\n\n")))
    
    (concat umbrella-section
            "ARTICLE URL: " url "\n\n"
            "ARTICLE CONTENT:\n" sanitized-text "\n\n"
            "EXPECTED OUTPUT FORMAT:\n"
            ":PROPERTIES:\n"
            ":ID:          " new-id "\n"
            ":END:\n"
            "#+title: <Article Title>\n"
            "#+ROAM_REFS: " url "\n"
            "#+filetags: :article:\n\n"
            "* Summary\n"
            "<Brief summary of the article>\n\n"
            "* Key Takeaways\n"
            "- <Key point 1>\n"
            "- <Key point 2>\n"
            "- <Key point 3>\n\n"
            "* Notes\n"
            "<Detailed notes with links to umbrella nodes if relevant>\n\n"
            "Generate the complete org-roam node following this format.")))

;;; LLM Processing

(defun my/url-catcher--request-llm (system-prompt user-prompt callback)
  "Request LLM to generate org-roam node content.

SYSTEM-PROMPT is the system instruction.
USER-PROMPT is the user message with article content.
CALLBACK is a function accepting (response info) for async handling.

Uses `gptel-request` API for async processing."
  (gptel-request
      user-prompt
    :system system-prompt
    :callback callback
    :stream nil))

;;; Validation and Save

(defun my/url-catcher--validate-and-save (llm-response url)
  "Validate LLM-RESPONSE and save as org-roam node.

URL is the original article URL for error reporting.

Validation steps:
1. Write to temporary buffer
2. Strip hallucinated markdown code blocks
3. Validate presence of :PROPERTIES:, :ID:, and #+title:
4. Extract title and generate slug
5. Write to org-roam-directory with timestamp-slug.org name
6. Invoke org-roam-db-sync
7. Kill temporary buffer

On validation failure, display raw output in `my/url-catcher-error-buffer`."
  (let ((temp-buf (generate-new-buffer " *url-catcher-validate*")))
    (unwind-protect
        (progn
          ;; Write response to temporary buffer
          (with-current-buffer temp-buf
            (insert llm-response)
            (goto-char (point-min))
            
            ;; Strip hallucinated markdown code blocks
            (while (re-search-forward "```org\n\\|```\n\\|```$" nil t)
              (replace-match ""))
            (goto-char (point-min))
            
            ;; Validate required elements
            (let ((has-properties (re-search-forward "^:PROPERTIES:" nil t))
                  (has-id (re-search-forward "^:ID:" nil t))
                  (has-title (progn
                               (goto-char (point-min))
                               (re-search-forward "^#\\+title:\\s-+" nil t))))
              (unless (and has-properties has-id has-title)
                (error "Missing required elements: properties=%s, id=%s, title=%s"
                       has-properties has-id has-title)))
            
            ;; Extract title for slug generation
            (goto-char (point-min))
            (if (re-search-forward "^#\\+title:\\s-+\\(.+\\)$" nil t)
                (let* ((title (match-string 1))
                       (slug (my/url-catcher--make-slug title))
                       (timestamp (format-time-string "%Y%m%d%H%M%S"))
                       (filename (format "%s-%s.org" timestamp slug))
                       (filepath (expand-file-name filename org-roam-directory))
                       (content (buffer-string)))
                  
                  ;; Write to org-roam-directory
                  (with-temp-file filepath
                    (insert content))
                  
                  ;; Sync database
                  (org-roam-db-sync)
                  
                  (message "URL Catcher: Successfully saved node to %s" filepath)
                  filepath)
              (error "Could not extract title from #+title:"))))
      (when (buffer-live-p temp-buf)
        (kill-buffer temp-buf)))))

(defun my/url-catcher--show-error (raw-output &optional error-msg)
  "Display RAW-OUTPUT in error buffer for debugging.

ERROR-MSG is an optional additional error message to display."
  (with-current-buffer (get-buffer-create my/url-catcher-error-buffer)
    (erase-buffer)
    (insert "URL Catcher Error\n")
    (insert "=================\n\n")
    (when error-msg
      (insert "Error: " error-msg "\n\n"))
    (insert "Raw LLM Output:\n")
    (insert "---------------\n")
    (insert raw-output))
  (display-buffer my/url-catcher-error-buffer)
  (message "URL Catcher: Error - see %s buffer for details" my/url-catcher-error-buffer))

;;; Interactive Workflow

(defun my/url-catcher--pipeline-callback (response info url)
  "Callback for the async LLM request pipeline.

RESPONSE is the LLM response text (or nil on error).
INFO is the gptel response info plist.
URL is the original article URL for error reporting."
  (if (and response (not (string-empty-p response)))
      ;; Success - validate and save
      (condition-case err
          (my/url-catcher--validate-and-save response url)
        (error
         (my/url-catcher--show-error response (error-message-string err))))
    ;; Error - handle based on info plist
    (let ((error-msg (or (plist-get info :error)
                         (plist-get info :status)
                         "Unknown error")))
      (my/url-catcher--show-error
       (or response "")
       (format "LLM request failed: %s" error-msg)))))

;;;###autoload
(defun my/collect-url (url)
  "Fetch URL content and create an org-roam node via LLM.

Prompts for a URL, then orchestrates the full pipeline:
1. Fetch content using trafilatura
2. Sanitize text for token efficiency
3. Extract umbrella nodes from org-roam database
4. Build prompts for LLM
5. Request LLM to generate org-roam node
6. Validate and save the result

All operations are asynchronous to keep the UI responsive."
  (interactive "sURL: ")
  (message "URL Catcher: Fetching and processing URL...")
  
  ;; Start the async pipeline
  (let ((system-prompt (my/url-catcher--build-system-prompt)))
    ;; Fetch URL content (synchronous, but fast with timeout)
    (let ((raw-content (my/url-catcher--fetch-url url)))
      (if raw-content
          (let* ((sanitized (my/url-catcher--sanitize-text raw-content))
                 (umbrella-nodes (my/url-catcher--get-umbrella-nodes))
                 (user-prompt (my/url-catcher--build-user-prompt url sanitized umbrella-nodes)))
            ;; Request LLM asynchronously
            (my/url-catcher--request-llm
             system-prompt
             user-prompt
             (lambda (response info)
               (my/url-catcher--pipeline-callback response info url))))
        (message "URL Catcher: Failed to fetch content from %s" url)))))

(provide 'org-roam-url-catcher)
;;; org-roam-url-catcher.el ends here
