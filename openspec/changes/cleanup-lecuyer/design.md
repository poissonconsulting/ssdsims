## Context

The package currently carries **two complete RNG-seeded runner paths**:

1. The original **monolith** path — `ssd_run_scenario()` (five S3 methods) chaining `ssd_sim_data()` (five S3 methods) → `ssd_fit_dists_sims()` → `ssd_hc_sims()` in memory, with the private `run_scenario()` chainer in `R/internal.R`. It seeds every per-task draw through the **L'Ecuyer-CMRG sub-stream lattice**: `get_lecuyer_cmrg_stream_state(s)(seed, stream, start_sim)` produces a length-7 `.Random.seed` state advanced with `parallel::nextRNGStream()` / `nextRNGSubStream()`, installed by `with_lecuyer_cmrg_state()` inside the `*_state` ops (`slice_sample_state`, `fit_dists_state`, `hc_state`) and their `*_seed` wrappers (`do_call_seed`, `fit_dists_seed`, `hc_seed`). The RNG backend lives in `R/lecuyer-cmrg-seed.R`.

2. The **targets** path — `ssd_run_scenario_baseline()` / `ssd_run_scenario_shards()` and the per-step targets runners, seeding each task through the **dqrng + primer** contract (`local_dqrng_state(seed, task_primer(identity))` under `local_dqrng_backend()`, `R/dqrng-backend.R` + `R/dqrng-state.R` + `R/task-lists.R`).

`migrate-public-api` (the prerequisite) retires the legacy *public API*: the declarative + sharded surface becomes the canonical, documented way to run a scenario. Once that lands, the monolith path is unreachable from the supported API, and the L'Ecuyer backend exists only to serve it. This change deletes the monolith path and the L'Ecuyer backend together — the terminal `cleanup-lecuyer` roadmap step, re-scoped from "remove the helpers" to "drop the whole dead path."

The deletion was anticipated since `dqrng-init` / `local-dqrng-state` (both archived note "the L'Ecuyer path stays until `cleanup-lecuyer`"). The intentional one-release overlap of two RNG backends ends here.

## Goals / Non-Goals

**Goals:**
- Delete the monolith runner path in full: `R/run-scenario.R`, `R/simulate-data.R`, `R/fit-dists-sims.R`, `R/hc-sims.R`, and the `run_scenario()` chainer in `R/internal.R`.
- Delete the L'Ecuyer-CMRG RNG backend in full: `R/lecuyer-cmrg-seed.R` and the six `*_state`/`*_seed` shims in `R/internal.R`.
- Drop the corresponding `NAMESPACE` exports / `S3method()` registrations, `man/` pages, dead tests, and legacy reference scripts; remove residual L'Ecuyer prose from `GLOSSARY.md` / vignettes / `TARGETS-DESIGN.md`.
- Leave the package with exactly one RNG backend (dqrng) and one runner family, `R CMD check`-clean.

**Non-Goals:**
- Retiring the public API itself — that is `migrate-public-api` (the prerequisite). This change assumes the canonical surface is already the declarative + sharded runners.
- Any change to the dqrng path's behaviour, the task tables, the step runners, or the sharded pipeline — they are untouched.
- Preserving the L'Ecuyer numeric outputs or providing a deprecation shim — there is no shim; the functions are removed outright (acceptable pre-1.0).
- The `mixed-code-lockin` shard-pinning recipe itself (a separate prerequisite); this change only relies on it so that deleting source does not force a wholesale shard rebuild.

## Decisions

### D1 — Remove outright, no deprecation shim
The functions are deleted, not soft-deprecated with `lifecycle::deprecate_warn()`. Pre-1.0 (`0.0.0.90xx`) there is no stability promise, the replacement (`ssd_run_scenario_baseline()` / `ssd_run_scenario_shards()`) is established by `migrate-public-api`, and a shim would re-introduce the very L'Ecuyer code this change exists to delete. The removals are flagged **BREAKING** in `NEWS.md` and in each capability's spec delta (`Reason` / `Migration`).
- *Alternative (rejected):* keep thin deprecated wrappers for one release. Rejected — they would have to call into the deleted monolith/L'Ecuyer code, defeating the cleanup; the migration target is already documented by the prerequisite.

### D2 — Order of removal: callers before callees, then prune `internal.R`
Delete in dependency order so the tree never references a missing symbol mid-change: (a) `R/run-scenario.R` (top-level callers); (b) `R/simulate-data.R`, `R/fit-dists-sims.R`, `R/hc-sims.R`; (c) the now-orphaned `run_scenario()` and the six `*_state`/`*_seed` shims in `R/internal.R`; (d) `R/lecuyer-cmrg-seed.R`. After (c), confirm whether `R/internal.R` retains any live symbol — if it is emptied, delete the file rather than leave an empty stub. A repo-wide search for each removed symbol (including `parallel::nextRNGStream`/`nextRNGSubStream`, `with_lecuyer_cmrg_*`, `*_state`, `*_seed`, `do_call_seed`) gates the change: zero references in `R/` outside deleted files.

### D3 — Spec deltas: four whole-capability removals + one modification
`run-scenario`, `simulate-data`, `fit-distributions`, and `hazard-concentrations` describe **only** the monolith functions, so every requirement in each is removed (the new path's sample/fit/hc behaviour is specified by the `task-lists` / `shard-runner` / `task-shards` / `scenario-definition` capabilities, which are untouched). `parallel-safe-seeding` *spans both* RNG paths, so it is modified: the six L'Ecuyer requirements (scalar-seed scoping, state-vector installation, their validation, sub-stream generation, stream isolation, caller-state independence) are removed, leaving only the dqrng `(seed, primer)` requirements. The capability's *Purpose* prose ("spans two RNG paths") is reconciled to the single dqrng path when the delta is synced into the main spec on archive.
- *Note:* `migrate-public-api`'s in-flight deltas re-scope these same specs onto dqrng. Because that change is the prerequisite and is itself moving to a "retire" framing, these removals supersede rather than conflict; if `migrate-public-api` lands a re-scoped `simulate-data`/`fit-distributions`/`hazard-concentrations` first, this change still removes them (a removal delta applies to whatever the main spec holds at archive time).

### D4 — Tests: delete monolith-only suites, do not migrate
`test-run-scenario.R`, `test-simulate-data.R`, `test-fit-dists-sims.R`, `test-hc-sims.R`, and `test-lecuyer-cmrg-seed.R` exercise only the removed functions and are deleted. The dqrng path's reproducibility, order-independence, and backend-integrity guarantees are already covered by the existing dqrng / baseline / shard / task-primer suites (landed across `local-dqrng-state`, `primer-primitives`, `task-rng-postcheck`, etc.), so no behaviour is left unverified. After deletion, scan the remaining suites for any incidental dependence on a removed helper (e.g. a test that called `with_lecuyer_cmrg_seed()` for setup) and rewrite it onto `local_dqrng_backend()` if found.

### D5 — Scripts and docs: drop monolith-only, reconcile shared
The `scripts/` reference and exploration files that drive only the monolith path (`example.R`, `example2.R`, `example-expanded.R`, `example-expanded-grids.R`, `example-expanded-grids-independent.R`, `experiment-substream-restart.R`, `reprex-trace.R` / `reprex-trace_reprex.md`) are removed — their byte-equivalence role ends when `ssd_run_scenario()` is gone. `GLOSSARY.md` keeps the *primer* / dqrng terminology and drops the L'Ecuyer `_state`/`state =` "misnomer" notes (which only existed to contrast the legacy naming). `man/` is regenerated by `devtools::document()`; the `.Rd` pages for the removed functions disappear automatically.

## Risks / Trade-offs

- **Premature removal before `migrate-public-api` lands** → Hard prerequisite: this change must not be applied until `migrate-public-api` has made the declarative + sharded surface canonical. Sequencing is enforced by the roadmap (`migrate-public-api` in `## Now`, this in `## Later`).
- **A live caller of a "dead" symbol is missed, breaking the build** → The D2 repo-wide symbol search (across `R/`, `tests/`, `vignettes/`, `scripts/` kept after D5) is a hard gate; `R CMD check` must pass clean before push.
- **Deleting source invalidates cached `targets` shards on existing runs** → Mitigated by the `mixed-code-lockin` prerequisite (the `tar_cue(depend = FALSE)` shard-pinning recipe): the removed code is not part of any shard's command closure, so pinned shards stay valid; unpinned stores would rebuild, which is acceptable for a source-only cleanup.
- **External user scripts still call `ssd_run_scenario()` / `ssd_sim_data()`** → Accepted pre-1.0 BREAKING; the `NEWS.md` entry and each spec delta's `Migration` note point at the replacement runners.
- **`R/internal.R` left as an empty stub** → D2 deletes the file if pruning empties it, avoiding a dangling collation entry.

## Migration Plan

1. Confirm prerequisites landed: `migrate-public-api` (legacy public API retired) and `mixed-code-lockin` (shard pinning).
2. Delete in D2 order; regenerate `NAMESPACE` and `man/` with `devtools::document()`.
3. Delete the D4 test suites; rewrite any incidental L'Ecuyer test-setup onto dqrng.
4. Remove the D5 scripts; strip L'Ecuyer prose from `GLOSSARY.md` / vignettes / `TARGETS-DESIGN.md`.
5. Repo-wide symbol search returns clean; `air format`; `devtools::test()` and `R CMD check` pass.
6. `NEWS.md` BREAKING entry; `openspec validate cleanup-lecuyer --strict` passes.

Rollback: the change is a pure deletion on a feature branch; reverting the commit restores the monolith + L'Ecuyer path verbatim.

## Open Questions

- Does `migrate-public-api` *un-export* the monolith functions (leaving the bodies for this change to delete) or *delete* them outright? Either way this change ends with them gone; the only effect is whether step (2) deletes function bodies or merely already-orphaned files. Resolve by reading the landed `migrate-public-api` before applying.
