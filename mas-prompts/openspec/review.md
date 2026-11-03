## Review round {{n}} of {{max}}: change `{{change}}`

The implementer's notes for this round: `{{input}}`.

1. Run the `openspec-verify-change` skill for `{{change}}`.
2. Review the actual changes (`git status`, `git diff`, new files) against
   the proposal, design, tasks and spec deltas: correctness, missing or
   unmarked tasks, tests, error handling, scope creep.
3. Do **not** edit project files in this step; only write the result file.
   The orchestrator compares the worktree before and after your turn.

Result:

- `verdict`: `approve` when the change is correct and ready to archive;
  `changes_requested` when the implementer must fix something;
  `blocked` when the operator must decide.
- Each required fix is a finding with status `open`. Severity: `blocking`
  = must fix, `major` = should fix, `minor` = nit (minor findings alone do
  not justify `changes_requested`).
- Findings from earlier rounds that are now fixed: same id, status
  `resolved`. If the implementer rejected a finding and you accept the
  reason: status `resolved` with that in `reason`.

{{schema}}
