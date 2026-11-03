## Result file (required)

When this step is finished, write exactly one JSON file to:

    {{artifact}}

Create it, or overwrite it if it exists. The orchestrator reads only this
file, never the chat, to decide what happens next. Shape:

```json
{
  "verdict": "...",
  "summary": "Two or three sentences for the other agent and the operator.",
  "findings": [
    {
      "id": "F1",
      "severity": "blocking | major | minor",
      "title": "One line",
      "detail": "What is wrong and where (files, sections)",
      "status": "...",
      "reason": "Why: the answer, fix or counter-argument"
    }
  ]
}
```

Rules:

- Valid JSON only: no comments, no trailing commas. `findings` may be `[]`.
- The allowed `verdict` and `status` values are given in the step
  instructions above.
- Keep finding ids stable across rounds: talk about an existing issue with
  its id; give new issues new ids.
- Never commit, stage, stash or otherwise write git state; commits are the
  operator's job.
- Run folder (shared with the other agent): `{{run_dir}}`.
