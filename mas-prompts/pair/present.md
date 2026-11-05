## Present the plan to the operator

The reviewer approved your plan. Now the operator decides.

Walk the operator through `{{run_dir}}/plan.md` in this chat: the
approach, the main changes, the tests, the risks, and any reviewer finding
you rejected or the reviewer conceded (briefly, with the reason). Keep it
short; the operator can read the file.

Answer the operator's questions. If the operator asks for adjustments,
apply them to `{{run_dir}}/plan.md` (and `{{run_dir}}/brief.md` when they
change the requirements) and list what you changed. Do not start coding
and change no project files.

End by telling the operator to press `C-c a m c` and choose **approve**
(you start coding) or **re-review** (the reviewer checks the adjusted
plan first).
