## Context

The package currently carries **two complete RNG-seeded runner paths**:

1. The original **monolith** path — `ssd_run_scenario()` (five S3 methods) chaining `ssd_sim_data()` (five S3 methods) → `ssd_fit_dists_sims()` → `ssd_hc_sims()` in memory, with the private `run_scenario()` chainer in `R/internal.R`. It seeds every per-task draw through the **L'Ecuyer-CMRG sub-stream lattice**: `get_lecuyer_cmrg_stream_state(s)(seed, stream, start_sim)` produces a length-7 `.Random.seed` state advanced with `parallel::nextRNGStream()` / `nextRNGSubStream()`, installed by `with_lecuyer_cmrg_state()` inside the `*_state` ops (`slice_sample_state`, `fit_dists_state`, `hc_state`) and their `*_seed` wrappers (`do_call_seed`, `fit_dists_seed`, `hc_seed`). The RNG backend lives in `R/lecuyer-cmrg-seed.R`.

2. The **targets** path — `ssd_run_scenario_baseline()` / `ssd_run_scenario_shards()` and the per-step targets runners, seeding each task through the **dqrng + primer** contract (`local_dqrng_state(seed, task_primer(identity))` under `local_dqrng_backend()`, `R/dqrng-backend.R` + `R/dqrng-state.R` + `R/task-lists.R`). Its public front door — `ssd_define_scenario()` fed by `ssd_scenario_data()` / `ssd_gen()` — already accepts every input source the monolith did, including the `fitdists` / `tmbfit` / function / function-name generators (`scenario-input-types`, archived).

The original plan retired the legacy surface in two steps: `migrate-public-api` would *port* the three step functions onto dqrng (keeping the L'Ecuyer helpers as a one-release shim), then `cleanup-lecuyer` would delete the shim. With the declarative + sharded surface now complete and covering the full input range, that migration is moot — there is no value in porting code that is about to be deleted, and nothing to migrate *to* that is not already shipped. So this change **folds `migrate-public-api` in** and collapses the two steps into one: retire the legacy public API by deleting the monolith path, and delete the L'Ecuyer backend with it. The intentional one-release overlap of two RNG backends (anticipated since `dqrng-init` / `local-dqrng-state`) ends here.

## Goals / Non-Goals

**Goals:**
- Retire the legacy public API: remove the exported `ssd_run_scenario()` / `ssd_sim_data()` generics + methods, `ssd_fit_dists_sims()`, and `ssd_hc_sims()`, leaving `ssd_define_scenario()` + `ssd_run_scenario_baseline()` / `ssd_run_scenario_shards()` as the sole documented entry points.
- Delete the monolith implementation in full: `R/run-scenario.R`, `R/simulate-data.R`, `R/fit-dists-sims.R`, `R/hc-sims.R`, and the `run_scenario()` chainer in `R/internal.R`.
- Delete the L'Ecuyer-CMRG RNG backend in full: `R/lecuyer-cmrg-seed.R` and the six `*_state`/`*_seed` shims in `R/internal.R`.
- Drop the corresponding `NAMESPACE` exports / `S3method()` registrations, `man/` pages, dead tests, and legacy reference scripts; remove residual L'Ecuyer prose from `GLOSSARY.md` / vignettes / `TARGETS-DESIGN.md` and point user-facing docs at the replacement surface.
- Leave the package with exactly one RNG backend (dqrng) and one runner family, `R CMD check`-clean.

**Non-Goals:**
- *Migrating* the legacy functions onto dqrng — the superseded `migrate-public-api` approach; they are deleted, not ported.
- The comprehensive README / vignette overhaul — that stays the separate `readme` change (now gated on this one rather than on `migrate-public-api`). This change only ensures docs do not reference removed functions and point at the canonical surface.
- Any change to the dqrng path's behaviour, the task tables, the step runners, or the sharded pipeline — they are untouched.
- Providing a deprecation shim — there is no shim; the functions are removed outright (acceptable pre-1.0).
- The `mixed-code-lockin` shard-pinning recipe itself (a separate prerequisite); this change only relies on it so that deleting source does not force a wholesale shard rebuild.

## Decisions

### D1 — Retire by deletion, folding in `migrate-public-api`; no migration, no shim
Because the declarative + sharded surface already covers every monolith capability (data sources via `ssd_scenario_data()` / `ssd_gen()`; the three steps via the step runners), retiring the legacy API means **deleting** it, not porting it. The functions are removed, not soft-deprecated with `lifecycle::deprecate_warn()`: pre-1.0 (`0.0.0.90xx`) there is no stability promise, and a shim (or a port) would re-introduce the very monolith/L'Ecuyer code this change exists to delete. `migrate-public-api` is therefore folded in and removed from the backlog; its spec deltas (which re-scoped `simulate-data` / `fit-distributions` / `hazard-concentrations` / `parallel-safe-seeding` onto dqrng) are superseded by this change's whole-capability removals.
- *Alternative (rejected):* keep `migrate-public-api` as a porting step and let `cleanup-lecuyer` delete the shim afterward. Rejected — porting code slated for deletion is wasted effort and prolongs the two-backend overlap for no user benefit; the migration target is already shipped.

### D2 — Order of removal: callers before callees, then prune `internal.R`
Delete in dependency order so the tree never references a missing symbol mid-change: (a) `R/run-scenario.R` (top-level callers); (b) `R/simulate-data.R`, `R/fit-dists-sims.R`, `R/hc-sims.R`; (c) the now-orphaned `run_scenario()` and the six `*_state`/`*_seed` shims in `R/internal.R`; (d) `R/lecuyer-cmrg-seed.R`. After (c), confirm whether `R/internal.R` retains any live symbol — if it is emptied, delete the file rather than leave an empty stub. A repo-wide search for each removed symbol (including `parallel::nextRNGStream`/`nextRNGSubStream`, `with_lecuyer_cmrg_*`, `*_state`, `*_seed`, `do_call_seed`) gates the change: zero references in `R/` outside deleted files.

### D3 — Spec deltas: four whole-capability removals + one modification
`run-scenario`, `simulate-data`, `fit-distributions`, and `hazard-concentrations` describe **only** the monolith functions, so every requirement in each is removed (the new path's sample/fit/hc behaviour is specified by the `task-lists` / `shard-runner` / `task-shards` / `scenario-definition` capabilities, which are untouched). `parallel-safe-seeding` *spans both* RNG paths, so it is modified: the six L'Ecuyer requirements (scalar-seed scoping, state-vector installation, their validation, sub-stream generation, stream isolation, caller-state independence) are removed, leaving only the dqrng `(seed, primer)` requirements. The capability's *Purpose* prose ("spans two RNG paths") is reconciled to the single dqrng path when the delta is synced into the main spec on archive. Folding in `migrate-public-api` removes that change's competing deltas on these same specs, so there is no longer any cross-change spec conflict to sequence.

### D4 — Tests: delete monolith-only suites, do not migrate
`test-run-scenario.R`, `test-simulate-data.R`, `test-fit-dists-sims.R`, `test-hc-sims.R`, and `test-lecuyer-cmrg-seed.R` exercise only the removed functions and are deleted. The dqrng path's reproducibility, order-independence, and backend-integrity guarantees are already covered by the existing dqrng / baseline / shard / task-primer suites (landed across `local-dqrng-state`, `primer-primitives`, `task-rng-postcheck`, etc.), so no behaviour is left unverified. After deletion, scan the remaining suites for any incidental dependence on a removed helper (e.g. a test that called `with_lecuyer_cmrg_seed()` for setup) and rewrite it onto `local_dqrng_backend()` if found.

### D5 — Scripts and docs: drop monolith-only, reconcile shared
The `scripts/` reference and exploration files that drive only the monolith path (`example.R`, `example2.R`, `example-expanded.R`, `example-expanded-grids.R`, `example-expanded-grids-independent.R`, `experiment-substream-restart.R`, `reprex-trace.R` / `reprex-trace_reprex.md`) are removed — their byte-equivalence role ends when `ssd_run_scenario()` is gone. `GLOSSARY.md` keeps the *primer* / dqrng terminology and drops the L'Ecuyer `_state`/`state =` "misnomer" notes (which only existed to contrast the legacy naming). Vignettes are checked for references to the removed functions and re-pointed at the canonical surface (deep overhaul deferred to `readme`). `man/` is regenerated by `devtools::document()`; the `.Rd` pages for the removed functions disappear automatically.

### D6 — Backlog hygiene: remove `migrate-public-api`, re-point the roadmap
Folding in is reflected in the backlog: the `migrate-public-api` change directory is removed (its purpose now lives here), `ROADMAP.md` drops its line and re-points `readme`'s blocker and this change's dependency, and the `TARGETS-DESIGN.md` §12 dependency prose that named `migrate-public-api` as a `cleanup-lecuyer` prerequisite is corrected. Archived changes that mention `migrate-public-api` are historical record and left untouched.

## Risks / Trade-offs

- **A live caller of a "dead" symbol is missed, breaking the build** → The D2 repo-wide symbol search (across `R/`, `tests/`, `vignettes/`, `scripts/` kept after D5) is a hard gate; `R CMD check` must pass clean before push.
- **A monolith capability turns out not to be fully covered by the declarative surface** → Verified: `ssd_gen()` (`scenario-input-types`) covers the `fitdists`/`tmbfit`/function/character generators, `ssd_scenario_data()` covers data frames, and the step runners cover sample/fit/hc; if a gap surfaces during apply, stop and raise it rather than deleting a still-needed capability.
- **Deleting source invalidates cached `targets` shards on existing runs** → Mitigated by the `mixed-code-lockin` prerequisite (the `tar_cue(depend = FALSE)` shard-pinning recipe): the removed code is not part of any shard's command closure, so pinned shards stay valid; unpinned stores would rebuild, which is acceptable for a source-only cleanup.
- **External user scripts still call `ssd_run_scenario()` / `ssd_sim_data()`** → Accepted pre-1.0 BREAKING; the `NEWS.md` entry and each spec delta's `Migration` note point at the replacement runners.
- **`R/internal.R` left as an empty stub** → D2 deletes the file if pruning empties it, avoiding a dangling collation entry.
- **Removing `migrate-public-api` orphans incidental references** → `scenario-option-vocabulary` names it only in a non-normative "concurrent changes that may conflict" list; left as-is (another in-flight change owns those files). The normative roadmap / design references are corrected (D6).

## Migration Plan

1. Confirm the prerequisite landed: `mixed-code-lockin` (shard pinning), and that the replacement surface (`ssd_define_scenario()` / `ssd_scenario_data()` / `ssd_gen()` / `ssd_run_scenario_baseline()` / `ssd_run_scenario_shards()`) is shipped (it is — archived).
2. Delete in D2 order; regenerate `NAMESPACE` and `man/` with `devtools::document()`.
3. Delete the D4 test suites; rewrite any incidental L'Ecuyer test-setup onto dqrng.
4. Remove the D5 scripts; strip L'Ecuyer prose from `GLOSSARY.md` / vignettes / `TARGETS-DESIGN.md` and re-point docs at the replacement surface.
5. Repo-wide symbol search returns clean; `air format`; `devtools::test()` and `R CMD check` pass.
6. `NEWS.md` BREAKING entry; `openspec validate cleanup-lecuyer --strict` passes.

Rollback: the change is a pure deletion on a feature branch; reverting the commit restores the monolith + L'Ecuyer path verbatim.

## Open Questions

None outstanding — folding in `migrate-public-api` resolves the prior question of whether that step would un-export or delete the monolith functions (this change deletes them).
