## Context

This design addresses the need for automated web article capture and integration into an `org-roam` personal knowledge management system. The current workflow requires manual copying, formatting, summarizing, and linking of web content—a highly contextual and time-consuming process.

**Current State:**
- User manually copies web article content
- Manual text sanitization and formatting
- Manual summarization and extraction of key points
- Manual creation of `org-roam` nodes with proper structure
- Manual linking to existing semantic hubs (umbrella nodes)

**Constraints:**
- Must use existing `gptel` configuration for LLM interactions (no custom HTTP)
- Must use existing `org-roam` and `emacsql` setup
- Must rely on system-installed `trafilatura` binary (no installation logic)
- Strict token efficiency requirements for LLM processing
- Zero tolerance for database corruption from LLM hallucinations

**Stakeholders:**
- Primary: Emacs user managing personal knowledge base
- Secondary: Future maintainers of the URL capture module

## Goals / Non-Goals

**Goals:**
- Automate end-to-end web article capture pipeline
- Provide strict functional decomposition with clear separation of concerns
- Implement validation guardrails to prevent LLM hallucination from corrupting the database
- Maximize token efficiency through text sanitization before LLM processing
- Enable semantic linking to existing knowledge base structure (umbrella nodes)
- Maintain non-blocking UI during async operations
- Use namespace prefix `my/url-catcher--` for all private functions

**Non-Goals:**
- Installing or managing `trafilatura` dependency (assumes pre-installed)
- Modifying `gptel` core configuration or API keys
- Replacing or modifying existing `org-roam` or `gptel` configurations
- Creating custom HTTP request handlers (uses `gptel-request` API)
- Modifying core files like `init.el` or `tools-ai.el`
- Real-time preview or editing of captured content before save

## Decisions

### 1. Pipeline Architecture: Sequential Async Stages

**Decision:** Implement the capture workflow as a sequence of discrete, composable functions executed asynchronously.

**Rationale:**
- Each stage (fetch → sanitize → extract metadata → build prompt → LLM → validate → save) has a single responsibility
- Easier to test, debug, and maintain individual functions
- Failures at any stage can be caught and reported without affecting others
- Aligns with Emacs Lisp best practices for async operations

**Alternatives Considered:**
- *Monolithic function:* Would be harder to test and debug. Rejected.
- *Class-based OOP approach:* Overkill for this use case; Emacs Lisp functional style is more idiomatic. Rejected.

### 2. Text Sanitization: Pre-LLM Processing

**Decision:** Apply aggressive text sanitization (blank line squashing, whitespace normalization, truncation) BEFORE sending to LLM.

**Rationale:**
- Reduces token consumption significantly
- Removes noise that doesn't contribute to understanding
- Truncation at 40000 chars (configurable) provides hard limit for API costs

**Alternatives Considered:**
- *Sanitize after LLM:* Would waste tokens on unnecessary content. Rejected.
- *No truncation:* Risk of excessive API costs on long articles. Rejected.

### 3. Database Query: Pre-fetch Umbrella Nodes

**Decision:** Query `org-roam` database upfront for nodes tagged `:umbrella:` and pass as context to LLM.

**Rationale:**
- Enables semantic linking to existing knowledge structure
- Query is fast (SQLite via `emacsql`)
- Provides LLM with explicit ID mappings for valid links

**Alternatives Considered:**
- *Query during LLM generation:* Would require sync database access during async callback. Rejected.
- *No umbrella node linking:* Loses valuable knowledge graph connectivity. Rejected.

### 4. LLM Integration: Use `gptel-request` API

**Decision:** Use the existing `gptel-request` function with callback pattern for LLM interactions.

**Rationale:**
- Leverages existing `gptel` configuration (model, API keys, etc.)
- Built-in error handling for network issues
- Async callback pattern fits Emacs event loop
- No need to reinvent HTTP client logic

**Alternatives Considered:**
- *Custom `url-retrieve` or `request.el`:* Would duplicate `gptel` functionality. Rejected.
- *Synchronous LLM call:* Would freeze Emacs UI. Rejected.

### 5. Validation: Two-Stage Buffer Approach

**Decision:** Write LLM response to temporary buffer first, validate structure, then write to final location.

**Rationale:**
- Prevents malformed content from reaching `org-roam-directory`
- Allows stripping of hallucinated markdown code blocks before save
- Provides clear error buffer (`*URL Catcher Error*`) for debugging

**Alternatives Considered:**
- *Direct write with rollback:* More complex; risk of partial corruption. Rejected.
- *No validation:* High risk of database corruption. Rejected.

### 6. File Naming: Timestamp-Based Slug

**Decision:** Use `YYYYMMDDHHMMSS-slug.org` pattern for generated files.

**Rationale:**
- Ensures unique filenames even for same article captured multiple times
- Chronological sorting in file browser
- Follows `org-roam` conventions

**Alternatives Considered:**
- *Hash-based naming:* Less human-readable. Rejected.
- *Title-based naming:* Risk of collisions and filesystem incompatibilities. Rejected.

### 7. Module Structure: Isolated Library + Simple Integration

**Decision:** Create `lisp/org-roam-url-catcher.el` as self-contained library; modify only `modules/tools-org.el` for integration.

**Rationale:**
- Clear separation between domain logic and configuration
- Easy to test library independently
- Minimal impact on existing configuration
- Follows project structure conventions (library code in `lisp/`)

**Alternatives Considered:**
- *Inline in `tools-org.el`:* Would bloat configuration file. Rejected.
- *Separate module in `modules/`:* This is library code, not configuration. Rejected.

## Risks / Trade-offs

| Risk | Mitigation |
|------|------------|
| `trafilatura` not in PATH or fails | Function returns `nil`/error; workflow notifies user gracefully |
| LLM API timeout or network error | `gptel-request` callback handles errors; user notified |
| LLM hallucinates malformed output | Two-stage validation catches issues; error buffer preserves raw output |
| No umbrella nodes exist in database | Function returns empty list; LLM prompted to omit linking section |
| Text truncation loses important content | Configurable `my/url-catcher-max-chars` allows user adjustment |
| `emacsql` query fails or schema changes | Error handling returns empty list; doesn't crash workflow |
| Async callback complexity | Clear function boundaries; each callback stage documented |

**Known Limitations:**
- Requires manual installation of `trafilatura` (Python package)
- Depends on `gptel` being properly configured with valid API credentials
- No support for paywalled or JavaScript-rendered content
- Single URL processing only (no batch mode in initial implementation)

## Migration Plan

**Deployment Steps:**
1. Create `lisp/org-roam-url-catcher.el` with all pipeline functions
2. Add `(require 'org-roam-url-catcher)` to `modules/tools-org.el`
3. Add keybinding for `my/collect-url` command in `tools-org.el`
4. Ensure `trafilatura` is installed system-wide (`pip install trafilatura`)
5. Verify `org-roam` database contains nodes (umbrella tagging optional)
6. Test workflow with sample URLs

**Rollback Strategy:**
1. Remove `(require 'org-roam-url-catcher)` from `tools-org.el`
2. Remove keybinding from `tools-org.el`
3. Delete `lisp/org-roam-url-catcher.el`
4. Restart Emacs or evaluate buffer

No database migration required; created files are additive to existing `org-roam` structure.

## Open Questions

1. **Default keybinding for `my/collect-url`:** What key combination avoids conflicts with existing bindings? (Candidate: `C-c c u` or `C-c n u`)

2. **LLM model selection:** Should the workflow use the default `gptel` model or allow per-request model override? (Trade-off: cost vs. quality)

3. **Umbrella node tag name:** Is `:umbrella:` the established convention, or should this be configurable?

4. **Error buffer cleanup:** Should `*URL Catcher Error*` buffer be auto-killed after N seconds, or persist for debugging?

5. **Slug generation:** How to handle non-ASCII characters in article titles for filename slugs? (URL-encode vs. transliterate vs. strip)

### Answers to Open Questions (Decisions)

1. **Default keybinding for `my/collect-url`:** **Decision:** Use `C-c n u` (aligning with the standard `org-roam` `C-c n` prefix). The binding should be added to `modules/tools-org.el` using the configuration style already present in that file (e.g., `define-key`, `bind-key`, or `map!`).

2. **LLM model selection:** **Decision:** Strictly use the default `gptel-model` currently active in the user's global configuration. Do NOT implement per-request model overrides. Keep the scope strictly focused on the pipeline logic.

3. **Umbrella node tag name:** **Decision:** Make it configurable. Introduce a custom variable `my/url-catcher-umbrella-tag` with a default value of `"umbrella"`. The `emacsql` query MUST use this variable dynamically.

4. **Error buffer cleanup:** **Decision:** The `*URL Catcher Error*` buffer MUST persist (do not auto-kill). It is critical for debugging LLM hallucinations, and the user must manually review and close it.

5. **Slug generation:** **Decision:** Implement a robust `my/url-catcher--make-slug` helper function. It must:
   - Downcase the title.
   - Aggressively strip non-ASCII and non-alphanumeric characters (e.g., replace `[^a-z0-9]+` with a single hyphen `-`).
   - Trim leading and trailing hyphens.
   - Truncate the resulting slug to a safe length (maximum 50 characters) to avoid filesystem limits.
