## ADDED Requirements

### Requirement: Fetch URL content via trafilatura
The system SHALL retrieve readable web content from a given URL using the `trafilatura` command-line tool with strict optimization flags.

#### Scenario: Successful content fetch
- **WHEN** a valid URL is provided that returns readable content
- **THEN** the system executes `trafilatura -u "<url>" --markdown --no-comments --fast`
- **AND** returns the raw markdown string extracted from the page

#### Scenario: URL fetch failure
- **WHEN** the URL is invalid or the command fails (non-zero exit code)
- **THEN** the system returns `nil` or signals a descriptive error

#### Scenario: Fetch timeout
- **WHEN** the `trafilatura` command execution exceeds 10 seconds
- **THEN** the system strictly terminates the process and returns `nil` or signals a timeout error

#### Scenario: Missing trafilatura binary
- **WHEN** the `trafilatura` executable is not found in the system `$PATH` (e.g., using `executable-find`)
- **THEN** the system does NOT attempt to install it and immediately returns `nil` or signals a "dependency missing" error
