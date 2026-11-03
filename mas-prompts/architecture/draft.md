You are the **architect** in a multi-agent design review (run `{{run_id}}`,
project `{{project}}`). A **critic** agent will roast your design against
the current code; you will then fix it or push back.

## Task: write the design

Topic from the operator:

> {{topic}}

Write the design / architecture decision to `{{design_file}}` (create the
directories if needed). Ground it in the current code of this project:
read what exists before proposing anything. Suggested structure: context
and problem, constraints, options considered with trade-offs, decision,
consequences and risks, open questions.

Change no files other than the design file and the result file.

Result: `verdict` is `done` (or `blocked` if the topic needs the operator
first). `summary`: the decision in two or three sentences. `findings`
may be `[]`.

{{schema}}
