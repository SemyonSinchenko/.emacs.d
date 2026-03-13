## ADDED Requirements

### Requirement: Validate LLM output and save org-roam node
The system SHALL protect the `org-roam` database by rigorously validating the LLM output before committing it to the filesystem.

#### Scenario: Write to temporary buffer
- **WHEN** the LLM response is received
- **THEN** the system MUST write the response to a temporary buffer (not directly to disk) to begin processing.

#### Scenario: Strip hallucinated markdown code blocks
- **WHEN** the LLM response incorrectly wraps the org-mode text in markdown code blocks (e.g., ` ```org ` at the start and ` ``` ` at the end)
- **THEN** the system MUST proactively strip these wrappers from the buffer content before performing strict validation.

#### Scenario: Validate required elements and extract title
- **WHEN** validating the cleaned buffer content
- **THEN** the system MUST check for the presence of the `:PROPERTIES:` drawer, the `:ID:` property, and the `#+title:` keyword.
- **AND** the system MUST extract the string value of the `#+title:` to be used for the filename slug.

#### Scenario: Validation success & Save
- **WHEN** all required elements are present
- **THEN** the system generates a timestamp (e.g., `format-time-string "%Y%m%d%H%M%S"`)
- **AND** processes the extracted title through the `my/url-catcher--make-slug` function
- **AND** writes the valid content to a new file in `org-roam-directory` named `<timestamp>-<slug>.org`
- **AND** invokes `org-roam-db-sync` to register the new node
- **AND** kills the temporary buffer.

#### Scenario: Validation failure
- **WHEN** any required elements are missing or malformed
- **THEN** the system MUST NOT write anything to the `org-roam-directory`
- **AND** the system MUST notify the user of the failure
- **AND** display the raw, unedited LLM output in a persistent `*URL Catcher Error*` buffer for manual review.
