# Agent guidelines

After completing a task, notify the user about what you completed using the macos `say` command with a descriptive message.

Keep track of your tasks using TODO.md. For complex tasks, first make a plan in TODO.md and then check off the tasks one by one. Periodically review if your plan is still good, and potentially update it to improve it.

## Dev loop we follow
- Run `./ber.sh` to process all `.ber` files (parsing or `#type` depending on pragmas) and print the rewritten files for review.
- Keep `.ber` fixtures up to date after changes; add comments like `(* CR codex: ... *)` to flag known issues.
- Use TODO.md to list coverage gaps and planned tests; add new fixtures under `ber/typing/` as needed.
- Before committing, ensure `dune build` succeeds and `./ber.sh` outputs are sane.
