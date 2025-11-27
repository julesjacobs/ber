# Agent guidelines

After completing a task, notify the user about what you completed using the macos `say` command with a descriptive message.

Keep track of your tasks using TODO.md. For complex tasks, first make a plan in TODO.md and then check off the tasks one by one. Periodically review if your plan is still good, and potentially update it to improve it.

Never git commit unless the user asks you to.

## Dev loop we follow
- After making code changes, run `./ber.sh`. This walks all `.ber` files (recursively under `ber/`) and rewrites them with fresh `>` outputs (parse ASTs or inferred types/type errors depending on `#type` pragma). It also prints each file so you can inspect the new outputs.
- Review the git diff after `./ber.sh` to spot behavioral changes or regressions in the fixtures. If you notice known issues, add comments like `(* CR codex: ... *)` in the `.ber` files (and remove these CR notes once the issues are fixed).
- Keep `.ber` fixtures up to date; add new examples under `ber/typing/` when expanding coverage. Track planned/needed tests in TODO.md.
- Before committing, ensure `dune build` succeeds, `./ber.sh` has been run, and diffs look intentional.
