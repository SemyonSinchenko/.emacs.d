## Implement the plan

The operator approved the plan. Implement `{{run_dir}}/plan.md` in the
project; `{{run_dir}}/brief.md` holds the requirements.

- Follow the plan; if the code proves a part of it wrong, choose the
  sensible fix and record the deviation as a finding (status `open`,
  severity `major`, the reason in `detail`) so the reviewer can judge it.
- Add or update the tests the plan names, and run the project's tests,
  linters or build checks that apply.
- Keep to the plan's scope.

Result: `verdict` is `done` (or `blocked` if you cannot continue without
the operator: say exactly what you need in `summary`). `summary`: what
you changed and how you checked it. `findings`: deviations from the plan
and anything the reviewer should look at closely, or `[]`.

{{schema}}
