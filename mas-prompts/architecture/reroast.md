## Re-check, round {{n}} of {{max}}

The architect answered your previous roast (`{{input}}`) and revised
`{{design_file}}`.

1. Each `accepted` finding: check the revision. Fixed: status `resolved`.
   Not really fixed: status `open`, saying what is still missing.
2. Each `rejected` finding: judge the reason. Convinced: status `conceded`.
   Not convinced: status `open`, with your counter-argument in `reason`
   (do not just repeat the finding).
3. Add new findings only for real new problems (new ids).

Result: `verdict` is `approve` when no `blocking` or `major` finding is
still open, meaning you agree with the design as it stands; otherwise
`changes_requested`. List all findings from this round with their current
status.

{{schema}}
