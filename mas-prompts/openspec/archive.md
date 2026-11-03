## Archive: change `{{change}}`

The change passed review. Archive it with the `openspec-archive-change`
skill: merge the spec deltas into `openspec/specs/` and move the change to
the archive. Then run `openspec validate --all` if available and fix only
archive-related problems.

Do not commit: the operator reviews and commits the result.

Result: `verdict` is `done` (or `blocked`). The `summary` is the final
report for the operator: what was implemented, what was archived, and
anything to look at before committing.

{{schema}}
