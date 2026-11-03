## Answer the roast, round {{n}} of {{max}}

The critic reviewed your design `{{design_file}}`: `{{input}}`.

For every finding with status `open`, decide:

- **accept** it: revise the design file accordingly; or
- **reject** it: keep the design and give a concrete reason. Push back when
  the critic is wrong; do not give in to weak arguments just to finish.

Change no files other than the design file and the result file.

Result: `verdict` is `done`. In `findings`, answer **every** open finding
of the roast, reusing its id, with status `accepted` (say what changed in
`reason`) or `rejected` (the reason). Missing ids make the orchestrator ask
you again.

{{schema}}
