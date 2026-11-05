## Review the code, round {{n}} of {{max}}

The implementer implemented the approved plan; its notes: `{{input}}`.
Requirements: `{{run_dir}}/brief.md`; plan: `{{run_dir}}/plan.md`.

Roast the actual changes (`git status`, `git diff`, new files; read the
surrounding code too): correctness and edge cases, requirements of the
brief that are missing or wrong, deviations from the plan (judge the ones
the implementer reported), tests (missing, weak, not run), error
handling, naming and style of the surrounding code, dead code, scope
creep. Build and run the tests and linters yourself: your project mount is
an overlay, your writes never reach the project.

Do not fix anything yourself; write only the result file.

Result:

- One finding per issue, status `open`, ids `C1`, `C2`, ...; severity
  `blocking` (wrong or broken), `major` (should fix) or `minor` (a nit
  worth fixing). Every finding, minor ones included, must be fixed or
  argued away before you approve, so do not pad the list.
- Findings of the implementer's notes: decide each one; agreed: status
  `resolved` with your reason; not agreed: status `open`, what to do in
  `reason`.
- `verdict`: `approve` only when no finding is `open`; otherwise
  `changes_requested`. `blocked` only when the work needs the operator's
  decision (say what in `summary`).

{{schema}}
