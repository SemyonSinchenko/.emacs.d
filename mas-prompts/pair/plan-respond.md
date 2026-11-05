## Answer the plan roast, round {{n}} of {{max}}

The reviewer critiqued your plan: `{{input}}`.

For every finding with status `open`, minor ones included, decide:

- **accept** it: revise `{{run_dir}}/plan.md` accordingly; or
- **reject** it: keep the plan and give a concrete reason. Push back when
  the reviewer is wrong or argues against a decision of the operator
  recorded in `{{run_dir}}/brief.md`; do not give in to weak arguments
  just to finish.

Change no project files; only the plan (and the brief, if it misstated
what the operator said) and the result file.

Result: `verdict` is `done`. In `findings`, answer **every** open finding
of the roast, reusing its id, with status `accepted` (say what changed in
`reason`) or `rejected` (the reason). Missing ids make the orchestrator
ask you again.

{{schema}}
