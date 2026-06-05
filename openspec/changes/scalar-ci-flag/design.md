## Context

The hc step of a scenario exposes a `ci` knob that flows from `ssd_define_scenario()` (stored at `scenario$hc$ci`) through the hc task table into `ssdtools::ssd_hc(..., ci = ci)`. Two layers treat `ci` as a multi-valued **axis** today:

- **Constructor** (`R/scenario.R`): `chk_length(ci, upper = 2L)` admits `c(FALSE, TRUE)`; a dedicated guard rejects bootstrap-only knobs *only* when `ci` is the single value `FALSE`, telling the user to "set `ci = c(FALSE, TRUE)` to enable bootstrap."
- **Task expansion** (`R/task-lists.R`): `task_axes("hc")` includes `"ci"`, and `hc_grid_tbl()` builds the grid in two branches bound together — the `ci = FALSE` branch with bootstrap-only knobs forced to `NA` (the §1.2 *collapse*, so a no-bootstrap row does not fan out across `nboot`/`ci_method`/`parametric`), and the `ci = TRUE` branch fanning out fully.

The premise behind the axis treatment — that `ci = FALSE` and `ci = TRUE` produce *different, both-wanted* results — is false for the estimate. Verified against `ssdtools` 2.6.0.9002 (`ssd_fit_dists(ssddata::ccme_boron, dists = ssd_dists_bcanz())`):

- `est` is **byte-identical** for `ci = FALSE` vs `ci = TRUE` (`1.25677835382383`), and identical across seeds `1`, `99999`, `7` — the point estimate is analytic and RNG-independent.
- This holds for **all seven** `ssd_ci_methods()`; none recompute `est` from bootstrap draws.
- The only non-CI column that differs is `nboot` (`0` when `ci = FALSE`, the bootstrap count otherwise) — cosmetic metadata, not a result. `se`/`lcl`/`ucl` are `NA` under `ci = FALSE` and populated under `ci = TRUE`.

So a `ci = TRUE` hc row is a strict superset of the `ci = FALSE` row for the same fit-task. The repo already encodes this exact reasoning for `samples` (the *"a single `TRUE` is a superset of `FALSE`"* requirement), keeping it a scalar, non-axis, non-primer knob. This change brings `ci` into line.

## Goals / Non-Goals

**Goals:**

- Make `ci` a scalar flag (`chk_flag`, default `FALSE`) on `ssd_define_scenario()` and validate it as a flag on `ssd_hc_sims()`.
- Remove `"ci"` from `task_axes("hc")` so it is neither a path nor an inner axis and does not enter the per-task primer; apply the scalar `ci` uniformly to every hc task.
- Retire the §1.2 *ci = FALSE collapse*: `hc_grid_tbl()` becomes a single grid keyed by the scalar `ci`.
- Keep the bootstrap-knob guard (reject `nboot`/`ci_method`/`parametric` when `ci = FALSE`), with `ci = TRUE` as the enablement path.

**Non-Goals:**

- Changing the point-estimate computation, `est_method` (still a genuine axis — it affects the estimate), or any other hc knob.
- Removing `ci = FALSE` as a *mode*. `ci = FALSE` stays the cheap, bootstrap-free, RNG-free point-estimate path — it is simply a scenario-wide scalar choice, not combinable with `TRUE` in one scenario.
- Touching `migrate-public-api`'s artifacts (see the cross-reference decision).

## Decisions

### Decision: `ci` is a scalar flag, modelled on `samples`

`ci` becomes `chk::chk_flag(ci)` (a single non-`NA` `TRUE`/`FALSE`), default `FALSE`, stored at `scenario$hc$ci`. It is **not** a member of `task_axes("hc")` and therefore never enters `path_key()`, the inner-axis complement, or `task_primer()`. It is applied to every hc task uniformly. `print.ssdsims_scenario()` continues to render it among the hc knobs.

*Rationale:* the estimate is invariant to `ci`, so `ci` carries no task-distinguishing information; a constant in the primer hash only obscures intent. Excluding it makes the hc task identity depend solely on the knobs that actually change a task's output (`est_method`, and under `ci = TRUE` the bootstrap knobs).

*Alternative considered — keep `ci` an axis but document the redundancy.* Rejected: it preserves the collapse machinery, the length-2 validation, and a user-facing footgun (`c(FALSE, TRUE)` silently doubles hc cost for redundant rows) to no benefit. The maintainer selected the full demotion.

### Decision: retire the §1.2 collapse; `hc_grid_tbl()` is single-branch

With a scalar `ci`, `hc_grid_tbl()` no longer needs `any(ci == FALSE)` / `any(ci == TRUE)` branches or `bind_rows`:

- `ci = FALSE` → `expand_grid(ci = FALSE, nboot = NA_integer_, est_method = hc$est_method, ci_method = NA_character_, parametric = NA)` — one row per `est_method`, bootstrap-only knobs canonically `NA` so they cannot enter task identity for a no-bootstrap estimate.
- `ci = TRUE` → `expand_grid(ci = TRUE, nboot = as.integer(hc$nboot), est_method, ci_method, parametric)` — the full fan-out, unchanged.

The "collapse" as a concept (deciding the `ci = FALSE` *portion* of a mixed grid) disappears; what remains is the simpler, local statement "`ci = FALSE` ⟹ bootstrap-only knobs are `NA`." The `NA`-canonicalisation that keeps `task_primer()` well-defined is retained for the `ci = FALSE` row.

### Decision: keep the bootstrap-knob guard, swap the escape hatch

The constructor still aborts when `ci = FALSE` *and* any of `nboot`/`ci_method`/`parametric` is explicitly supplied — those knobs are meaningless without a bootstrap. The guard simplifies from `length(ci) == 1L && isFALSE(ci)` to `isFALSE(ci)`, and the message changes from "Set `ci = c(FALSE, TRUE)` to enable bootstrap, or omit the knob(s)" to "Set `ci = TRUE` to enable bootstrap, or omit the knob(s)."

### Decision: `ci` stays a *carried column* on the hc task table (like `n_max`), not an axis

`ci` remains a column on the hc task table — `hc_grid_tbl()` keeps emitting it (now single-valued) — but is excluded from `task_axes("hc")`. This is the established **carried-column** pattern: `n_max` is likewise a column on every `sample` task (the shared draw size) yet is not a `sample` axis, so it is neither hashed into the primer nor used as a partition level. Both hc step runners already read `ci` from that column — the in-memory baseline (`ssd_run_scenario_baseline()`, via `pmap` over `hc_args[c("ci", …)]` in `R/task-lists.R`) and the shared `ssd_run_hc_step()` (`ci = t$ci`, used by both the single-core `ssd_run_scenario_sharded()` and the `targets` pipeline) — so **no runner code changes**: dropping `ci` from `task_axes("hc")` removes it from the primer/partition split while the column it reads stays put. The shard write/read already round-trips non-axis carried columns (it does so for `n_max`), so `ci` needs no special handling there either.

*Alternative considered — thread `ci` from the scenario slice* (like `proportion`/`samples`, which are *not* task columns). Rejected as more churn for no gain: it would edit both runners and `hc_grid_tbl()`, whereas the carried-column route matches the existing `n_max` precedent and leaves the runners untouched. (`proportion`/`samples` are scenario-threaded for historical reasons; `ci` follows the closer `n_max` analogue.)

### Decision: `ci` leaves the per-task primer — bootstrap CIs shift, estimates do not

Dropping `"ci"` from `task_axes("hc")` changes the hash fed to `task_primer()` for hc tasks (a constant term is removed), which shifts the per-task dqrng stream and therefore the bootstrap draws. Consequently `lcl`/`ucl`/`se` change value for a given seed; `est` does not (it is RNG-independent). This is a one-time re-baseline, acceptable for an unreleased package with no downstream dependants (`TARGETS-DESIGN.md` §12 — "breaking-change steps are fine"). Snapshot fixtures re-record.

### Decision: orthogonal to `migrate-public-api`; second-to-land drops `ci` from the primer enumeration

`migrate-public-api`'s *"Reproducible bootstrapping"* (MODIFIED) requirement enumerates the hc primer identity as "its `sim`/`stream` together with its hc-grid row: `ci`, `nboot`, `est_method`, `ci_method`, `parametric`." Since `ci` becomes a scenario-wide constant excluded from `task_axes("hc")`, it no longer belongs in that per-task identity. The two changes do not otherwise touch the same code; whichever lands second drops `"ci"` from the enumeration so the two specs stay consistent. No `migrate-public-api` artifact is edited here.

## Risks / Trade-offs

- **Lost flexibility for "cheap estimate now, CI later" in one scenario** → the only thing the `ci = FALSE` shard offered alongside `ci = TRUE` was *earlier* availability of a point estimate that the `ci = TRUE` shard also contains; it never added information. Users who want a fast point-estimate sweep run a `ci = FALSE` scenario; users who want CIs run `ci = TRUE`. Either is a one-line scalar choice.
- **CI re-baseline** (above) → mitigated by it being pre-release and by `est` (the headline result) staying byte-identical; snapshots are regenerated in the same change.
- **Spec drift with `migrate-public-api`** → the cross-reference decision pins the reconciliation (drop `ci` from the primer enumeration in whichever lands second).

## Open Questions

- **None blocking.** The carried-column-vs-scenario-slice question is resolved above (carried column, like `n_max`); both are spec-conformant since `ci` is excluded from `task_axes("hc")`.
