## Why
Personal Knowledge Management (PKM) within Emacs `org-roam` requires a streamlined way to capture web articles. Currently, manually copying content, stripping formatting, summarizing, and linking to existing semantic hubs ("umbrella nodes") is highly manual and context-switching. 

This change automates the entire pipeline: fetching content, sanitizing it for LLM token efficiency, querying existing database knowledge, and safely generating a fully linked `org-roam` node via AI. By implementing strict functional decomposition and validation guardrails, we ensure that the LLM does not hallucinate structure, corrupt the database, or over-consume API tokens.

## What Changes
A new, fully isolated Emacs Lisp module will be introduced to handle asynchronous URL capture, processing, and `org-roam` integration. 
- A new file `lisp/org-roam-url-catcher.el` will be created containing all domain logic.
- The existing `modules/tools-org.el` will be modified solely to require this new module and assign a keybinding to the main interactive command `my/collect-url`.
- The system will rely on pre-existing configurations of `gptel` and `org-roam`, and a pre-installed system binary of `trafilatura`.

## Capabilities

### New Capabilities
- `<url-fetching>`: Retrieves readable web content using a strict shell command.
  - **Function Signature:** `(my/url-catcher--fetch-content url)`
  - **Requirements:** MUST strictly use `trafilatura -u "<url>" --markdown --no-comments --fast`. MUST execute synchronously or via a promise with a strict timeout (e.g., 10 seconds). MUST return the raw markdown string, or `nil`/error if the command fails. MUST NOT attempt to install `trafilatura` (assume it is in `$PATH`).
  
- `<text-sanitization>`: Cleans the raw extracted text to maximize token density before LLM processing.
  - **Function Signature:** `(my/url-catcher--sanitize-text raw-text)`
  - **Requirements:** 1) Squash blank lines (`\n{3,}` -> `\n\n`). 2) Squash horizontal whitespace (`[ \t]+` -> ` `). 3) Trim leading/trailing whitespace. 4) ONLY AFTER these steps, truncate the result to a customizable variable `my/url-catcher-max-chars` (default `40000`).

- `<metadata-extraction>`: Queries the `org-roam` SQLite database for semantic hubs.
  - **Function Signature:** `(my/url-catcher--get-umbrella-nodes)`
  - **Requirements:** Uses `emacsql` to fetch nodes explicitly tagged with `:umbrella:`. MUST handle cases where no such tags exist by returning `nil` or an empty list without crashing. Returns an alist or hash-table mapping `("Title" . "ID")`.

- `<prompt-generation>`: Constructs the final instructions and context for the LLM.
  - **Function Signature:** `(my/url-catcher--build-user-prompt url sanitized-text umbrella-nodes-alist)`
  - **Requirements:** - **System Prompt:** "You are a specialized Knowledge Management assistant. Your ONLY task is to output valid, raw `org-roam` node text based on the provided article. RULES: 1. Never wrap your response in markdown code blocks. Output raw text. 2. Include a property drawer with the provided ID. 3. Include `#+title: <Article Title>`. 4. Include `#+ROAM_REFS: <Original URL>`. 5. Write a brief summary and bullet points. 6. Link to the provided Umbrella Nodes IF AND ONLY IF highly relevant. Use exact IDs provided: `[[id:GIVEN-ID][Title]]`."
    - **User Prompt:** MUST assemble the `UMBRELLA NODES MAP:` (omit if empty), `ARTICLE URL:`, `ARTICLE CONTENT:` (sanitized text), and `EXPECTED OUTPUT FORMAT:` showing proper `org-roam` property drawers, `#+title`, `#+filetags: :article:`, `* Summary`, and `* Key Takeaways` sections.

- `<llm-processing>`: Manages the asynchronous request to the LLM.
  - **Requirements:** MUST use the `gptel-request` API. MUST NOT implement custom HTTP requests or redefine `gptel` core variables (keys, models). MUST define a `:callback` that handles network errors, timeouts, and forwards the response to the validation stage.

- `<node-validation-and-save>`: Protects the `org-roam` database from malformed LLM outputs.
  - **Function Signature:** `(my/url-catcher--validate-and-save llm-response-text)`
  - **Requirements:** 1. Writes the LLM response to a temporary buffer first.
    2. Strips hallucinated markdown code blocks (e.g., removes ` ```org ` and ` ``` `).
    3. Validates the presence of `:PROPERTIES:`, `:ID:` (generated via Emacs), and `#+title:`.
    4. Upon successful validation, writes to `org-roam-directory` with filename `YYYYMMDDHHMMSS-slug.org` and invokes `org-roam-db-sync`.
    5. On failure, notifies the user and displays raw output in `*URL Catcher Error*` buffer.

- `<interactive-workflow>`: The entry point tying the pipeline together.
  - **Function Signature:** `(defun my/collect-url (url) (interactive "sURL: ") ...)`
  - **Requirements:** Orchestrates fetching, sanitizing, metadata extraction, prompt building, and LLM processing in a non-blocking UI manner.

### Modified Capabilities
- `<org-workflow>`: Integration of the new command into the existing user configuration.
  - **Requirements:** The file `modules/tools-org.el` must be updated to include `(require 'org-roam-url-catcher)` and map `my/collect-url` to a suitable keybinding.

## Impact
- **Code:** - CREATES: `lisp/org-roam-url-catcher.el` (MUST use namespace prefix `my/url-catcher--` for private functions).
  - MODIFIES: `modules/tools-org.el`.
  - RESTRICTION: MUST NOT modify `init.el`, `tools-ai.el`, or any other core files.
- **APIs & Dependencies:** - Heavily relies on `emacsql` and `org-roam` API for database access.
  - Relies on `gptel` for LLM connectivity.
  - Relies on external system executable `trafilatura`.
- **Systems/Data:** - Safely creates `.org` files within the configured `org-roam-directory` and triggers SQLite DB synchronization. The two-stage temporary buffer validation completely mitigates the risk of database corruption from LLM hallucinations.
