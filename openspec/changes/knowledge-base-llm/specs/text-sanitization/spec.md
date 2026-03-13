## ADDED Requirements

### Requirement: Sanitize extracted text for token efficiency
The system SHALL clean raw markdown text to maximize token density before LLM processing. The sanitization MUST occur in a strict sequential pipeline: squash blank lines -> squash horizontal whitespace -> trim -> truncate.

#### Scenario: Squash excessive blank lines
- **WHEN** the text contains sequences of 3 or more consecutive newlines (e.g., `\n{3,}`)
- **THEN** the system replaces them with exactly 2 newlines (`\n\n`) to preserve paragraph separation

#### Scenario: Squash horizontal whitespace
- **WHEN** the text contains sequences of multiple spaces or tabs
- **THEN** the system replaces them with a single space

#### Scenario: Trim whitespace
- **WHEN** the text has leading or trailing whitespace
- **THEN** the system removes it completely

#### Scenario: Truncate to max length (Final Step)
- **WHEN** the fully sanitized text exceeds `my/url-catcher-max-chars` (which MUST default to 40000)
- **THEN** the system truncates the text to exactly that character limit

#### Scenario: Text within limits (Final Step)
- **WHEN** the fully sanitized text is within the `my/url-catcher-max-chars` limit
- **THEN** the system returns the text without truncation
