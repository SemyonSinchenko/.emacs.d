## 1. Core Library Setup

- [x] 1.1 Create `lisp/org-roam-url-catcher.el` with proper file header and lexical binding
- [x] 1.2 Define namespace prefix `my/url-catcher--` for all private functions
- [x] 1.3 Add `provide` statement at end of file
- [x] 1.4 Add `(require 'org-roam-url-catcher)` to `modules/tools-org.el`

## 2. Configuration Variables

- [x] 2.1 Define `my/url-catcher-max-chars` (default 40000) for text truncation limit
- [x] 2.2 Define `my/url-catcher-umbrella-tag` (default "umbrella") for configurable tag query
- [x] 2.3 Create error buffer name constant `*URL Catcher Error*`

## 3. URL Fetching Implementation

- [x] 3.1 Implement `my/url-catcher--fetch-url` function using `trafilatura` CLI
- [x] 3.2 Add check for `trafilatura` binary using `executable-find`
- [x] 3.3 Implement 10-second timeout for fetch operation
- [x] 3.4 Handle fetch failures by returning `nil` or signaling descriptive error

## 4. Text Sanitization Implementation

- [x] 4.1 Implement `my/url-catcher--sanitize-text` function with sequential pipeline
- [x] 4.2 Squash 3+ consecutive newlines to exactly 2 newlines
- [x] 4.3 Replace multiple spaces/tabs with single space
- [x] 4.4 Trim leading and trailing whitespace
- [x] 4.5 Truncate to `my/url-catcher-max-chars` limit as final step

## 5. Metadata Extraction Implementation

- [x] 5.1 Implement `my/url-catcher--get-umbrella-nodes` function using `emacsql`
- [x] 5.2 Query `org-roam` database for nodes with `my/url-catcher-umbrella-tag`
- [x] 5.3 Return alist mapping node titles to UUIDs
- [x] 5.4 Handle empty results and database errors gracefully (return `nil`)

## 6. Prompt Generation Implementation

- [x] 6.1 Implement `my/url-catcher--build-system-prompt` function
- [x] 6.2 Include rules: no markdown wrapping, property drawer with ID, `#+title`, `#+ROAM_REFS`
- [x] 6.3 Implement `my/url-catcher--build-user-prompt` function
- [x] 6.4 Generate valid org-roam ID using `org-id-new` and inject into prompt
- [x] 6.5 Include umbrella nodes mapping section when available (omit when empty)
- [x] 6.6 Structure user prompt with ARTICLE URL, ARTICLE CONTENT, and expected format

## 7. LLM Processing Implementation

- [x] 7.1 Implement `my/url-catcher--request-llm` function using `gptel-request` API
- [x] 7.2 Create callback function accepting `response` and `info` arguments
- [x] 7.3 Handle successful response by forwarding text to validation stage
- [x] 7.4 Handle network/API errors via `info` plist inspection
- [x] 7.5 Handle timeout or empty response gracefully with user notification

## 8. Validation and Save Implementation

- [x] 8.1 Implement `my/url-catcher--validate-and-save` function
- [x] 8.2 Write LLM response to temporary buffer first (not directly to disk)
- [x] 8.3 Strip hallucinated markdown code blocks (```org wrappers)
- [x] 8.4 Validate presence of `:PROPERTIES:` drawer, `:ID:` property, and `#+title:`
- [x] 8.5 Extract title value from `#+title:` for slug generation
- [x] 8.6 Implement `my/url-catcher--make-slug` helper function:
  - Downcase title
  - Strip non-ASCII and non-alphanumeric characters (replace with `-`)
  - Trim leading/trailing hyphens
  - Truncate to 50 characters maximum
- [x] 8.7 Generate timestamp using `format-time-string "%Y%m%d%H%M%S"`
- [x] 8.8 Write valid content to `<timestamp>-<slug>.org` in `org-roam-directory`
- [x] 8.9 Invoke `org-roam-db-sync` to register new node
- [x] 8.10 Kill temporary buffer after successful save
- [x] 8.11 Handle validation failure: display raw output in `*URL Catcher Error*` buffer

## 9. Interactive Workflow Implementation

- [x] 9.1 Implement `my/collect-url` interactive command
- [x] 9.2 Prompt for URL via minibuffer using `(interactive "sURL: ")`
- [x] 9.3 Display status message "Fetching and processing URL..." on invocation
- [x] 9.4 Orchestrate full async pipeline: fetch → sanitize → metadata → prompt → LLM → validate → save
- [x] 9.5 Handle pipeline errors at each stage with safe abort and user notification

## 10. Integration and Keybinding

- [x] 10.1 Add keybinding for `my/collect-url` in `modules/tools-org.el` using `C-c n u`
- [x] 10.2 Follow existing configuration style in `tools-org.el` (e.g., `define-key`, `bind-key`, or `map!`)
- [x] 10.3 Verify load order: `org-roam-url-catcher` loaded after `org-roam` and `gptel`

## 11. Testing and Verification

- [x] 11.1 Test with valid URL returning readable content
- [x] 11.2 Test with invalid URL to verify error handling
- [x] 11.3 Test with missing `trafilatura` binary
- [x] 11.4 Test with empty umbrella nodes database
- [x] 11.5 Verify generated org file has correct structure and properties
- [x] 11.6 Verify `org-roam-db-sync` registers new node correctly
- [x] 11.7 Test LLM error handling (API timeout, network failure)
