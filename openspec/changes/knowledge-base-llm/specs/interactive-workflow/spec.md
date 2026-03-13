## ADDED Requirements

### Requirement: Provide interactive workflow entry point
The system SHALL provide a single interactive command (`my/collect-url`) that orchestrates the entire asynchronous URL capture pipeline while maintaining a responsive UI.

#### Scenario: User invokes command
- **WHEN** the user calls `M-x my/collect-url` or uses the bound keybinding
- **THEN** the system prompts for a URL via the minibuffer (e.g., using `(interactive "sURL: ")`)
- **AND** immediately displays a status message (e.g., "Fetching and processing URL...") so the user knows the background task has started.

#### Scenario: Full pipeline orchestration
- **WHEN** a valid URL is provided
- **THEN** the system orchestrates the flow: fetching -> sanitizing -> metadata extraction -> prompt building -> LLM processing.
- **AND** the system MUST ensure this orchestration is non-blocking to the Emacs UI, utilizing callbacks or async sequences where necessary (especially for `gptel-request`).

#### Scenario: Pipeline error handling
- **WHEN** any stage of the pipeline fails (e.g., fetch timeout, LLM error, validation failure)
- **THEN** the system safely aborts the remaining steps without crashing Emacs
- **AND** notifies the user of the specific failure stage via `message` or a dedicated error buffer.
