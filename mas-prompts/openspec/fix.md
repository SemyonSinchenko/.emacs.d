## Follow-ups, round {{n}} of {{max}}: change `{{change}}`

The lead reviewed your implementation and requested changes:
`{{input}}`.

- Address every finding with status `open`: fix it, or explain why it
  should not be changed. A finding may be the lead's decision on a
  blocker you reported (and the change's artifacts may have been amended):
  continue the implementation from there, through the rest of `tasks.md`.
- Build and run the relevant tests again.
- New blockers: as in the apply step, implement what you can, then report
  them as new findings (severity `blocking`, status `open`, with your
  recommendation); never wait for anyone.

Result: `verdict` is always `done` (the orchestrator accepts nothing
else). In `findings`, answer **every** open finding of the review, reusing
its id, with status `accepted` (done; say how in `reason`) or `rejected`
(not done; give the reason), and add your new findings with new ids.
Missing ids make the orchestrator ask you again.

{{schema}}
