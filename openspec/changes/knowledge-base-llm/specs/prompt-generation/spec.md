## ADDED Requirements

### Requirement: Build LLM prompt for org-roam node generation
The system SHALL construct system and user prompts that instruct the LLM to generate valid `org-roam` node content, injecting a pre-generated Emacs ID.

#### Scenario: System prompt construction
- **WHEN** building the system prompt
- **THEN** the system explicitly includes rules: output raw text (strictly no markdown wrapping like ```org), include a property drawer with the provided ID, include `#+title`, include `#+ROAM_REFS`, write a summary and bullet points, and link to umbrella nodes (if provided) using exact IDs.

#### Scenario: User prompt with umbrella nodes
- **WHEN** the umbrella nodes mapping is provided and non-empty
- **THEN** the user prompt includes an `UMBRELLA NODES MAP:` section clearly listing the Title-to-ID pairs.

#### Scenario: User prompt without umbrella nodes
- **WHEN** the umbrella nodes mapping is `nil` or empty
- **THEN** the user prompt completely omits the `UMBRELLA NODES MAP:` section.

#### Scenario: User prompt structure and ID injection
- **WHEN** assembling the final user prompt
- **THEN** the system FIRST generates a valid `org-roam` ID (e.g., via `org-id-new`)
- **AND** explicitly injects this ID into the `:PROPERTIES:` drawer of the `EXPECTED OUTPUT FORMAT:` section
- **AND** assembles the rest of the prompt: `ARTICLE URL:`, `ARTICLE CONTENT:` (sanitized text), `#+title:`, `#+filetags: :article:`, and the required structural headers (`* Summary`, `* Key Takeaways`).
