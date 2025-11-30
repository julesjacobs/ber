- [x] Fix mutual `let rec` so polymorphism is preserved (no weak vars leak).
- [x] Update fixtures after the fix.
- [x] Run `./ber.sh` and review diffs.
- [x] Make paths in type errors relative.
- [x] Add tests for inner let generalization where the generalized type will still have some outer type variables that shouldn't be generalized, but also some inner variables that should be generalized. This will test the level based polymorphism.
- [ ] Simplify inference return types (e.g. have `infer_expr_with_expected` return unit) and adjust callers.
- [ ] Clean up any now-dead env threading in inference helpers.
- [ ] Run `./ber.sh` after the refactor and review diffs.


> Type mismatch:
> 38 |   Cons ((fun x -> (x,x)), Cons (bool_not, Nil))
>                         △△△          ▲▲▲▲▲▲▲▲ bool -> bool      
>                          |                            ----
>                     bool * bool <----------------------^