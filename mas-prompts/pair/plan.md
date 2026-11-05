## Write the brief and the plan

The operator ended the discussion. Write two files in the run folder
`{{run_dir}}`:

1. `{{run_dir}}/brief.md`: what you and the operator agreed, for a
   reviewer who never saw the chat. Goal, context, decisions (with the
   operator's reasons), constraints, acceptance criteria, out of scope.
   Record the operator's decisions faithfully; they are not up for debate.
2. `{{run_dir}}/plan.md`: how you will build it, grounded in the current
   code: the files and functions to change, the approach and why,
   alternatives you dropped, tests to add or run, risks, the order of the
   work.

Change no project files: coding starts only after the operator approves
the plan. The orchestrator compares the worktree before and after your
turn.

Result: `verdict` is `done` (or `blocked` if you cannot plan without the
operator: say what you need in `summary`). `summary`: the plan in two or
three sentences. `findings` may be `[]`.

{{schema}}
