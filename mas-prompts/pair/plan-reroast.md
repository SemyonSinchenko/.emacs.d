## Re-check the plan, round {{n}} of {{max}}

The implementer answered your previous findings (`{{input}}`) and revised
`{{run_dir}}/plan.md`. If the answer says the operator adjusted the plan,
those adjustments are the operator's decisions: check that the plan is
still consistent and correct, but do not argue the decisions themselves.

1. Each `accepted` finding: check the revision. Fixed: status `resolved`.
   Not really fixed: status `open`, saying what is still missing.
2. Each `rejected` finding: judge the reason. Convinced: status
   `conceded`. Not convinced: status `open`, with your counter-argument in
   `reason` (do not just repeat the finding).
3. Add new findings only for real new problems (new ids).

Result: `verdict` is `approve` only when no finding is still `open`,
minor ones included; otherwise `changes_requested`. List all findings from
this round with their current status.

{{schema}}
