## Review round {{n}} of {{max}}: change `{{change}}`

The implementer's notes for this round: `{{input}}`.

Measured by the orchestrator (not the implementer's claim):

Task list: {{tasks}}

Last implementer turn:
{{progress}}

Every fix round starts the implementer in a new session: it knows only
the project, the change's artifacts and your review file. Write the
review so that it is enough.

1. Run the `openspec-verify-change` skill for `{{change}}`.
2. Review the actual changes (`git status`, `git diff`, new files) against
   the proposal, design, tasks and spec deltas: correctness, missing or
   unmarked tasks, tests, error handling, scope creep.
3. Run the focused tests of the modules changed in this round yourself;
   do not rely on the implementer's report of them. Record each command
   and its result in a field `"tests": [{"command": "...", "result":
   "pass | fail | not run (why)"}]`.
4. Watch the critical path. If the same task group stayed untouched for
   two rounds while easier items closed, or a round closed no task, do
   not re-sequence the work again: make the next critical-path task the
   one `blocking` finding of this round, with a concrete first step.
5. Decide every open finding of the implementer's notes. The implementer
   never blocks: when it hit a contradiction or needed a decision, it
   stopped and reported a finding (often severity `blocking`) with a
   recommendation. That decision is yours. If the change's artifacts are
   wrong, fix them in `openspec/changes/{{change}}/` and run
   `openspec validate {{change}}`; then answer the finding with the same
   id, status `open`, and your decision in `reason`, so the implementer
   continues from it.
6. Edit no project files except `openspec/changes/{{change}}/`. The
   orchestrator compares the rest of the worktree before and after your
   turn.

Result:

- `verdict`: `approve` when the change is correct and ready to archive;
  `changes_requested` when the implementer must fix or continue something
  (also after a blocker you decided); `blocked` only when you cannot
  decide without the operator (say exactly what is needed in `summary`);
  this should be rare. One such case: the change is too big to finish,
  even with fresh implementer sessions; then propose a split.
- Each required fix is a finding with status `open`. Severity: `blocking`
  = must fix, `major` = should fix, `minor` = nit (minor findings alone do
  not justify `changes_requested`).
- List only findings that are open, or whose status changes in this
  round; drop those resolved in earlier rounds. New findings get ids
  `R{{n}}-1`, `R{{n}}-2`, ...; an existing issue keeps its id.
- The implementer answers each open finding with `fixed`, `not_done`
  (not done yet) or `disputed` (it disagrees, see `reason`). Fixed and
  verified: same id, status `resolved`. Not done, or a dispute you do not
  accept: keep it `open` with your answer in `reason`. A dispute you
  accept: `resolved`, with that in `reason`.

{{schema}}
