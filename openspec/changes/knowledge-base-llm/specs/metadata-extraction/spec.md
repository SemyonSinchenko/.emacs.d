## ADDED Requirements

### Requirement: Query org-roam database for umbrella nodes
The system SHALL query the `org-roam` SQLite database (via `emacsql`) to find nodes tagged as semantic hubs, using a configurable tag variable.

#### Scenario: Umbrella nodes exist
- **WHEN** the database contains nodes with the tag specified in `my/url-catcher-umbrella-tag` (default `"umbrella"`)
- **THEN** the system returns an alist mapping exact node Titles to their generated UUIDs (IDs)

#### Scenario: No umbrella nodes exist
- **WHEN** no nodes in the database match the tag specified in `my/url-catcher-umbrella-tag`
- **THEN** the system returns `nil` or an empty list without crashing

#### Scenario: Database query error
- **WHEN** the `emacsql` query fails due to database corruption, schema mismatch, or connection issues
- **THEN** the system handles the error gracefully, logs a warning if possible, and returns `nil` (allowing the rest of the URL capture process to continue without semantic links)
