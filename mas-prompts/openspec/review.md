## Review round {{n}} of {{max}}: change `{{change}}`

The implementer's notes for this round: `{{input}}`.

1. Run the `openspec-verify-change` skill for `{{change}}`.
2. Review the actual changes (`git status`, `git diff`, new files) against
   the proposal, design, tasks and spec deltas: correctness, missing or
   unmarked tasks, tests, error handling, scope creep.
3. Decide every open finding of the implementer's notes. The implementer
   never blocks: when it hit a contradiction or needed a decision, it
   stopped and reported a finding (often severity `blocking`) with a
   recommendation. That decision is yours. If the change's artifacts are
   wrong, fix them in `openspec/changes/{{change}}/` and run
   `openspec validate {{change}}`; then answer the finding with the same
   id, status `open`, and your decision in `reason`, so the implementer
   continues from it.
4. Edit no project files except `openspec/changes/{{change}}/`. The
   orchestrator compares the rest of the worktree before and after your
   turn.

Result:

- `verdict`: `approve` when the change is correct and ready to archive;
  `changes_requested` when the implementer must fix or continue something
  (also after a blocker you decided); `blocked` only when you cannot
  decide without the operator (say exactly what is needed in `summary`);
  this should be rare.
- Each required fix is a finding with status `open`. Severity: `blocking`
  = must fix, `major` = should fix, `minor` = nit (minor findings alone do
  not justify `changes_requested`).
- Findings from earlier rounds that are now fixed: same id, status
  `resolved`. If the implementer rejected a finding and you accept the
  reason: status `resolved` with that in `reason`.

{{schema}}
