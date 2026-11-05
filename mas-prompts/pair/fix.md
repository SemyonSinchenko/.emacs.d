## Answer the code review, round {{n}} of {{max}}

The reviewer reviewed your code: `{{input}}`.

For every finding with status `open`, minor ones included, decide:

- **accept** it: fix the code (and its tests); or
- **reject** it: keep the code and give a concrete reason. Push back when
  the reviewer is wrong or asks for something the brief or the approved
  plan rules out; do not give in to weak arguments just to finish.

Run the tests and checks that apply after your fixes.

Result: `verdict` is `done` (or `blocked` if a finding needs the
operator: say what in `summary`). In `findings`, answer **every** open
finding of the review, reusing its id, with status `accepted` (say what
you changed in `reason`) or `rejected` (the reason). Missing ids make the
orchestrator ask you again.

{{schema}}
