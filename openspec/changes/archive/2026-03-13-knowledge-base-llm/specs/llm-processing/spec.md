## ADDED Requirements

### Requirement: Manage asynchronous LLM requests via gptel
The system SHALL use the `gptel` package API (`gptel-request`) to send prompts and receive responses entirely asynchronously, preventing UI blocking.

#### Scenario: Successful LLM request
- **WHEN** the `gptel-request` completes successfully
- **THEN** the callback function (which MUST accept both `response` and `info` arguments) receives the generated text
- **AND** forwards the `response` string to the validation and save stage

#### Scenario: Network or API error
- **WHEN** the network request fails (e.g., API key issue, connectivity loss)
- **THEN** the callback inspects the `info` plist for errors (e.g., checking `:error` or `:status`)
- **AND** handles the error gracefully by notifying the user without crashing Emacs

#### Scenario: Request timeout
- **WHEN** the LLM does not respond within the configured timeout or returns an empty response
- **THEN** the callback gracefully aborts the pipeline and notifies the user via Emacs `message` or `error`

#### Scenario: Strict API compliance
- **WHEN** implementing the LLM interaction
- **THEN** the system MUST use exclusively the `gptel-request` API
- **AND** MUST NOT implement custom HTTP/curl requests, nor redefine core `gptel` variables (like `gptel-api-key` or `gptel-model`)
