## 1. Name-based `min_pmix` (D1)

- [ ] 1.1 Change `ssd_fit_dists_sims(min_pmix=)` from a list of functions to a character vector of function **names** (default `"ssd_min_pmix"`); validate it is a character vector and that each name resolves via `resolve_min_pmix()` (`ssdtools` namespace → global env), aborting informatively otherwise.
- [ ] 1.2 Confirm all three steps reuse the shipped `sample_data_task_primer()` / `fit_data_task_primer()` / `hc_data_task_primer()` directly (signatures now match — `fit_data_task_primer()` takes the `min_pmix` name); add a thin adapter only if argument names differ.

## 2. Migrate `ssd_sim_data.data.frame()` (`R/simulate-data.R`)

- [ ] 2.1 Open a (reentrant) `local_dqrng_backend()` scope at entry; resolve `seed` per D4 (use a supplied `seed` verbatim; when `NULL`, draw one scalar integer seed from the ambient RNG once, inside the restored scope).
- [ ] 2.2 Replace the L'Ecuyer `get_lecuyer_cmrg_stream_states()` + per-`nrow` `slice_sample_state()` with: one shared `n_max = max(nrow)` draw per `(stream, sim, replace)` via `sample_data_task_primer(data, n_max, replace, seed, primer)`, with `primer = task_primer(list(stream, sim, replace))`; then `head(., nrow)`-truncate per `nrow`.
- [ ] 2.3 Preserve the output tibble shape (`sim`, `stream`, `nrow`, `replace`, `data`) and all existing argument validation; keep the public signature unchanged.
- [ ] 2.4 Leave the generator methods (`function`/`character`/`fitdists`/`tmbfit`) on the L'Ecuyer shim, untouched.

## 3. Migrate `ssd_fit_dists_sims()` (`R/fit-dists-sims.R`)

- [ ] 3.1 Open a reentrant `local_dqrng_backend()` scope; resolve `seed` per D4.
- [ ] 3.2 Replace the `fit_dists_seed()` call in the `pmap` body with `fit_data_task_primer()`, deriving `primer = task_primer()` over the fit task identity `(stream, sim, nrow, rescale, computable, at_boundary_ok, min_pmix name, range_shape1, range_shape2)`.
- [ ] 3.3 Pass the `min_pmix` **name** through to `fit_data_task_primer()` (resolved by `resolve_min_pmix()`); update the Roxygen `@param min_pmix` from "a list of functions" to "a character vector of function names". Preserve empty-input handling and the rest of the signature.

## 4. Migrate `ssd_hc_sims()` (`R/hc-sims.R`)

- [ ] 4.1 Open a reentrant `local_dqrng_backend()` scope; resolve `seed` per D4.
- [ ] 4.2 Replace the `hc_seed()` call in the `pmap` body with `hc_data_task_primer()`, deriving `primer = task_primer()` over the hc task identity `(stream, sim, nboot, ci_method, parametric)` — the scalar `ci` flag and the `est_method` setting are applied uniformly and excluded from the primer (D6).
- [ ] 4.3 Preserve the `min_pboot`-is-reserved guard, empty-input handling, `save_to`, the scalar `ci` flag handling (`chk_flag`, applied uniformly, absent from the primer), and the public signature.

## 5. Deprecate the L'Ecuyer shim (`R/internal.R`)

- [ ] 5.1 Mark `slice_sample_state()`, `fit_dists_state()`, `fit_dists_seed()`, `hc_state()`, `hc_seed()`, `do_call_seed()` (and the `get_lecuyer_cmrg_stream_state(s)()` usage) as a deprecated one-release shim in comments/Roxygen, noting removal in `cleanup-lecuyer`. Keep them defined and working for the generator methods and scripts/tests.

## 6. Reference scripts (byte-equivalence)

- [ ] 6.1 Rewrite `scripts/example-expanded.R` so its `data_list`/`fit_list`/`hc_list` are built from `sample_data_task_primer()` / `fit_data_task_primer()` (or the 1.1 helper) / `hc_data_task_primer()`, seeding each entry with `local_dqrng_backend()` + the per-task primer.
- [ ] 6.2 Re-establish the byte-equivalence comparison against the migrated `ssd_run_scenario(seed = …)` (`data`/`hc` via `identical()`, `fits` via `ssdtools::estimates()`).
- [ ] 6.3 Reconcile `scripts/example-expanded-grids.R` and `scripts/example-expanded-grids-independent.R` with the per-task-own-primer dqrng model (update to the new primitives or annotate as historical).

## 7. Tests

- [ ] 7.1 `test-simulate-data.R`: same `(seed, stream)` ⇒ identical; different `stream` ⇒ different; `head(., nrow)` is a prefix of the `n_max` draw; standalone vs. in-grid task identical (order-independence); global `.Random.seed`/`RNGkind()` unchanged across a call.
- [ ] 7.2 `test-fit-dists-sims.R`: deterministic fits for a fixed `seed`; order-independent fits; `min_pmix` accepted as a name and resolved via `resolve_min_pmix()` (incl. a custom function found in the global env); an unresolvable name aborts informatively; the primer keys on the name; empty-input path preserved.
- [ ] 7.3 `test-hc-sims.R`: deterministic bootstrap for a fixed `seed` with `ci = TRUE`; order-independence; `min_pboot` guard; scalar `ci` flag (a vector `ci` aborts; the primer is identical across `ci` values for a fixed identity since `ci` is excluded).
- [ ] 7.4 Add a byte-equivalence test mirroring `scripts/example-expanded.R` (migrated public path == loop-free dqrng expansion).
- [ ] 7.5 Refresh any snapshots tied to the previous L'Ecuyer numeric output.

## 8. Docs and finalize

- [ ] 8.1 Update Roxygen for the three functions to document dqrng + per-task primer seeding (cross-reference `task_primer()` / `local_dqrng_state()`), note `stream` as a primer component and the `seed = NULL` behavior; run `devtools::document()`.
- [ ] 8.2 Update `GLOSSARY.md` cross-references and add a `NEWS.md` entry flagging the BREAKING (numeric) change.
- [ ] 8.3 Run `air format`, then `devtools::test()` / `R CMD check`; fix fallout.
- [ ] 8.4 `openspec validate migrate-public-api --strict` passes.
