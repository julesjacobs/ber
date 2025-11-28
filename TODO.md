- [x] Fix mutual `let rec` so polymorphism is preserved (no weak vars leak).
- [x] Update fixtures after the fix.
- [x] Run `./ber.sh` and review diffs.








Future:
- [x] Make paths in type errors relative.
- [x] Add tests for inner let generalization where the generalized type will still have some outer type variables that shouldn't be generalized, but also some inner variables that should be generalized. This will test the level based polymorphism.
