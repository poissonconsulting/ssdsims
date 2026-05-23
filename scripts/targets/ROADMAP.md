# Roadmap — ssdsims targets tooling

Working hypothesis: `ssdsims` evolves from "a few simulation helpers + a
hand-rolled example" into a package that **mints reproducible targets
projects from a higher-level R DSL**, so that researchers can express a
large simulation exercise as a few lines of R and get a runnable,
interruption-tolerant, locally- *or* SLURM-parallel project.

The architectural anchor is the `targets` package. Everything we add is
in service of letting researchers describe **what** they want simulated
and letting the tooling decide **how** the work is chopped, cached, and
distributed.

## Workflow and artefacts

### Local

```
┌──────────────────────────── User's machine ────────────────────────────┐
│                                                                        │
│   R session (controller)                                               │
│     │                                                                  │
│     │  targets::tar_make()                                             │
│     │   ├── reads _targets/meta/*  (hashes, errors, wall time)         │
│     │   ├── decides which branches are stale                           │
│     │   └── hands stale branches to crew_controller_local(workers=N)   │
│     │                                                                  │
│     │            ┌────────┬────────┬────────┐                          │
│     │            ▼        ▼        ▼        ▼                          │
│     │           w1       w2       w3  ...   wN     ← R subprocesses    │
│     │            │        │        │        │                          │
│     │            └─ each runs sim → fit → hc ┘                         │
│     │                       │                                          │
│     │                       ▼                                          │
│     │   data/<config>/branch-*.parquet     ← local filesystem          │
│     │                                                                  │
│     ▼                                                                  │
│   collect.R / analysis.qmd  →  duckplyr reads parquet back lazily      │
│                                                                        │
│   Artefacts on disk:                                                   │
│     _targets/meta/    target metadata, error log, wall time            │
│     _targets/objects/ small return values (paths, summaries)           │
│     data/<config>/    hive-partitioned parquet (one file per branch)   │
└────────────────────────────────────────────────────────────────────────┘
```

### SLURM HPC

```
┌─ User laptop ─┐         ┌──────────────── Login node ─────────────────┐
│               │  ssh /  │                                             │
│ write DSL,    │──remote │  R session (controller, long-running)       │
│ run           │  R─────▶│   │                                         │
│ tar_make()    │         │   │ targets::tar_make()                     │
│ remotely (or  │         │   │  ├── reads _targets/meta/* (shared FS)  │
│ copy project  │         │   │  └── crew_controller_slurm(             │
│ up)           │         │   │        workers = N,                     │
└───────────────┘         │   │        template = slurm.tmpl,           │
                          │   │        resources = list(cpus, mem, …))  │
                          │   │           │                             │
                          │   │           │ sbatch one job per worker   │
                          │   │           ▼                             │
                          │   │   ┌──── Compute nodes ────┐             │
                          │   │   │                       │             │
                          │   │   │  worker R sessions    │             │
                          │   │   │   ├ pull task from    │             │
                          │   │   │   │   controller      │             │
                          │   │   │   ├ run sim→fit→hc    │             │
                          │   │   │   └ write parquet     │             │
                          │   │   │                       │             │
                          │   │   └───────┼───────────────┘             │
                          │   │           │                             │
                          │   │           ▼                             │
                          │   │   ┌────────────────────────┐            │
                          │   │   │ Shared FS  OR  S3      │ ← workers  │
                          │   │   │ data/<config>/*.parquet│   write,   │
                          │   │   └────────────────────────┘   login    │
                          │   │           ▲                    reads    │
                          │   │           │                             │
                          │   │   collect.R / analysis.qmd              │
                          │   │   reads parquet via duckplyr            │
                          │   │   (DuckDB httpfs for S3)                │
                          │   │                                         │
                          │   │  _targets/meta/  lives on shared FS so  │
                          │   │  the controller and any rerun sees the │
                          │   │  same hashes and error log              │
                          │   └─────────────────────────────────────────┘
                          └─────────────────────────────────────────────┘
                                              │
                                              ▼
                                       analyst on a different
                                       machine reads the S3 parquet
                                       directly — no scp / no shared FS
```

The login-vs-worker split matters because: (a) the controller R session
must stay alive for the duration of the run (often `tmux`/`screen`),
(b) only the worker nodes do CPU-heavy work, and (c) the **storage
root** has to be reachable from both. Shared FS is the path of least
resistance on most clusters; S3 lets analysts pull results without
touching the cluster at all (see [Storage](#storage)).

## Where we are (M0, today)

* Simulation primitives live in `R/`: `ssd_sim_data()`,
  `ssd_fit_dists_sims()`, `ssd_hc_sims()`, `ssd_run_scenario()`.
* `scripts/example.R` shows a typical exercise as a flat script.
* `scripts/targets/` (this PR) hand-rolls a targets project around that
  example: dynamic branching by `nrow`, hive-partitioned Parquet output
  written through duckplyr, parallel via `crew::crew_controller_local()`,
  collected lazily with duckplyr.
* `scripts/targets/experiments/` (also this PR) explores split granularity
  vs. wall time and per-branch size, with reproducible `.qmd` reports.

## Split granularity — recommendations, not magic

The experiments characterise a handful of obvious candidates:

| split label | branch key                                  | typical branch count    | when it shines                                |
|-------------|---------------------------------------------|-------------------------|------------------------------------------------|
| `coarse`    | `nrow`                                      | ~5–10                   | fastest wall time, lowest scheduler overhead   |
| `medium`    | `nrow × ci_method`                          | tens                    | recommended default; ~30–120 s per branch      |
| `fine`      | `nrow × ci_method × nboot`                  | hundreds                | good for HPC at scale                          |
| `atomic`    | full `(nrow, ci_method, nboot, proportion)` | full grid (thousands)   | maximum resume granularity; fault-tolerant     |

The DSL surfaces these as named choices, plus a custom escape hatch:

```r
ssdsims_project("boron-sims") |>
  recommend_branching()       # prints the table above with size estimates
                              # and an "(recommended)" marker

ssdsims_project("boron-sims") |>
  set_branching("medium")     # named option

ssdsims_project("boron-sims") |>
  set_branching(by = c("nrow", "ci_method", "nboot"))  # custom
```

We give a recommendation based on grid size and compute target (local
vs. SLURM), but always let the user pick — splitting affects
fault-tolerance and analysis ergonomics, not just speed, so it's not a
choice we should make silently.

## Storage

Two backends, same DSL:

```r
project |> set_storage("data/")                          # local / shared FS
project |> set_storage("s3://my-bucket/boron-sims/")     # S3
```

Both write hive-partitioned Parquet. Reads use duckplyr
(`read_parquet_duckdb()`) which goes through DuckDB's `httpfs` extension
for S3 — same code path for analysis whether the data lives on disk or
in a bucket.

**Why S3.** On HPC, results often need to escape the cluster — a
collaborator without a login wants to render the analysis notebook on
their laptop. Writing to S3 from the workers removes the scp / shared-FS
step entirely; the analyst's `analysis.qmd` reads
`s3://bucket/<config>/*.parquet` directly, locally, with no SLURM
involved.

Caveats we'll need to handle:

* Worker AWS credentials (instance role on cloud HPC; `~/.aws` for
  on-prem with S3 gateway).
* Atomic writes: each branch writes to a unique file path keyed by the
  branch index, so partial writes from a failed branch are orphans, not
  corruption.
* Parquet listing latency on S3 — we'll cache the manifest in
  `_targets/objects/<config>_files.rds` for fast collection.

## Errors and partial re-runs

A simulation project's lifecycle is dominated by debugging cycles — a
user changes one helper, one scenario errors, they fix and re-run. The
package should make this fast and unambiguous, and **never** ask the
user to recompute work they already have.

Two classes of failure, handled differently:

### Infrastructure failures

Scheduler timeout, OOM, network blip, S3 throttling. Re-running
`tar_make()` retries just the failed branches because the successful
ones are still cached in `_targets/meta`. Workers are stateless; nothing
to clean up.

The package adds a small retry wrapper around each branch
(`tar_target(..., error = "null", retrieval = "main")` plus a configurable
`max_retries`) so that transient errors don't even surface as failures
on the first pass.

### User-code errors

Typo in a helper, NA in input data, scenario formula doesn't converge
for one combination. These need debugging, not retry. The package adds:

* **Clear error surfacing.** `ssdsims_errors(project)` reads
  `tar_meta(fields = c("error", "warnings"))`, joins it to the branch
  parameter grid, and prints something like:

  ```
  3 of 240 branches errored:

    scenario1 / nrow=20 / ci_method=weighted_sample / nboot=1000
      ! "system is computationally singular: …"

    scenario1 / nrow=50 / ci_method=weighted_sample / nboot=1000
      ! "system is computationally singular: …"
    …
  ```

  So the failure mode (and the inputs that triggered it) is visible
  without spelunking through `_targets/meta`.

* **Re-run after code change.** `targets` already invalidates downstream
  targets when their code-dependency hash changes. The DSL leans into
  this:

  - User edits `R/functions.R` → only branches whose code-deps changed
    re-run. All others stay cached, including their parquet output.
  - User edits one scenario's body → only that scenario's branches
    invalidate. Other scenarios stay green.
  - User changes input data → all branches downstream of that input
    invalidate; everything else stays green.

* **Surgical invalidation.** When the user *wants* to re-run a specific
  cut without changing code (e.g. "redo all `nboot=1000` branches with
  a different seed"), the DSL exposes:

  ```r
  ssdsims_invalidate(project, where = nboot == 1000)
  ```

  which calls `targets::tar_invalidate()` on the matching branches and
  deletes their parquet files. Other branches and their outputs are
  untouched.

* **Debug a single branch.** `ssdsims_debug(project,
  branch_id = "scenario1_b042")` runs that one branch in the foreground
  with `browser()` on error — no crew, no parquet write, just the
  function call with its captured inputs. This is the workflow a user
  reaches for when `ssdsims_errors()` shows something unfamiliar.

Combined: a debug cycle becomes "see the error in
`ssdsims_errors()`, reproduce with `ssdsims_debug()`, fix the code,
re-run `tar_make()`" — and only the previously-failing branches re-run.

## Milestones

### M1 — Reusable targets factory (local) `[next]`

Goal: turn the hand-rolled `scripts/targets/` workflow into a
package-exported function that any researcher can call to get the same
shape of project for their own scenario.

* `ssdsims_targets_project(dir, scenarios, ...)` — exported function.
  Given a directory and a description of one-or-more scenarios, writes
  out a self-contained targets project (the equivalent of
  `scripts/targets/`).
* The scenario description is a plain R list, e.g.
  ```r
  scenario(
    data       = ssddata::ccme_boron,
    nrow       = c(5, 6, 10, 20, 50),
    nsim       = 100,
    proportion = c(0.01, 0.05, 0.1),
    ci_method  = ssdtools::ssd_ci_methods(),
    nboot      = c(10, 100, 1000),
    samples    = TRUE
  )
  ```
* The minted project keeps the structure we have today:
  `_targets.R`, `R/functions.R`, `run.R`, `collect.R`, `README.md`,
  `.gitignore`, plus an `experiments/` runner if requested.
* Output layout: hive-partitioned Parquet on the local filesystem
  (`set_storage()` defaults to `"data/"`).
* Local execution only; `crew::crew_controller_local()` is the default
  controller.
* `ssdsims_errors()` / `ssdsims_debug()` / `ssdsims_invalidate()` land
  here — these are independent of compute backend.

**Done when:** a researcher can replicate `scripts/targets/` end-to-end
by calling `ssdsims_targets_project()` and `targets::tar_make()`, with
no hand-editing.

### M2 — SLURM HPC backend + S3 storage

Goal: the same `ssdsims_targets_project()` call produces a project that
runs on a SLURM cluster, and either backend can write to S3.

* `compute = c("local", "slurm")`. Under `"slurm"` the minted
  `_targets.R` swaps `crew::crew_controller_local()` for
  `crew.cluster::crew_controller_slurm()`, with sensible defaults:
  - `tasks_max` sized from the input grid,
  - per-worker resources templated (cpus, memory, walltime, partition),
  - host- and queue-aware tuning hooks.
* `set_storage()` accepts an `s3://` URI. Workers write through DuckDB's
  `httpfs` extension; `collect.R` reads the same way.
* A small `slurm_template.tmpl` ships with the package; researchers can
  override per-site.
* The error/debug/invalidate UX from M1 works identically on SLURM —
  branch-level granularity, no special cases.
* Auth / accounts are out of scope; the project assumes the user can
  `sbatch` already (and, for S3, has working credentials).

**Done when:** the same scenario description from M1 runs on a SLURM
cluster against the same Parquet output, with no project-source
changes besides flipping `compute = "slurm"` and `set_storage()`.

### M3 — Higher-level DSL

Goal: hide the "build a list of scenarios" step behind a DSL designed
for simulation exercises.

* Surface is **R functions** (per the chosen DSL direction), composable
  with the existing tidyverse-style API:
  ```r
  ssdsims_project("boron-sims") |>
    add_scenario("ci_methods",
      data      = ssddata::ccme_boron,
      nrow      = c(6, 20, 50),
      ci_method = ssdtools::ssd_ci_methods(),
      nboot     = c(100, 1000)) |>
    add_scenario("est_methods",
      data       = ssddata::ccme_boron,
      est_method = c("arithmetic", "geometric", "multi"),
      ci         = FALSE) |>
    recommend_branching() |>          # prints the candidate table
    set_branching("medium") |>        # user picks; not picked silently
    set_compute("slurm", workers = 32) |>
    set_storage("s3://my-bucket/boron-sims/") |>
    write_project()
  ```
* DSL emits the same targets project shape as M1/M2; everything below
  the DSL is unchanged.
* Validation: the DSL checks parameter combinations against
  `ssdtools::ssd_ci_methods()` / `ssd_est_methods()` etc. and reports
  configuration errors **at DSL time**, not at `tar_make()` time.
* `recommend_branching()` estimates branch count and approximate
  wall time per split candidate (from the experiments matrix and the
  current compute config), so the recommendation is grounded.

**Done when:** a researcher can express a multi-scenario simulation
exercise in <30 lines of R and run it on local or SLURM with one
command.

### M4 — Ergonomics and operations

Smaller items, mostly polish, mostly post-M3:

* **Resume / extend.** A new scenario added to the DSL should reuse the
  Parquet outputs that are still valid. `targets` already handles this
  for unchanged targets; surface it cleanly.
* **Live monitoring.** A `monitor_project()` helper that tails crew /
  `_targets/meta/progress` and reports wall time, branches done, ETA.
* **Notebook output.** Each project ships a default `.qmd` that reads
  the Parquet output and renders summary tables/plots — analogous to
  `analysis/{timing,output}.qmd` from this PR.
* **Schema docs.** Each minted project lists the columns it writes,
  including list-column shapes for `samples` / `dists`.

## Related work

This is well-trodden ground; the design borrows liberally from existing
ecosystems and tries to be explicit about where it fits.

### Closest precedent — the R Targetopia

[**R Targetopia**](https://wlandau.github.io/targetopia/) is Will
Landau's term for a family of R packages that wrap `targets` for a
specific domain, "abstracting away most of the tricky planning and
engineering typically required to write pipelines." That's exactly the
niche this roadmap targets. Concrete blueprint packages in the
ecosystem:

* [**stantargets**](https://docs.ropensci.org/stantargets/) — domain
  package for Bayesian Stan workflows. The user calls
  `tar_stan_mcmc()` / `tar_stan_mcmc_rep_summary()` / etc., each of
  which returns a list of `tar_target()` objects (a "target factory")
  that they drop into `_targets.R`. Direct API model for what
  `add_scenario()` should look like in M3.
* [**jagstargets**](https://docs.ropensci.org/jagstargets/) — same
  pattern for JAGS.
* [**tarchetypes**](https://docs.ropensci.org/tarchetypes/) — the
  generic target-factory toolkit underneath the Targetopia. Provides
  `tar_map()` for static branching over a grid, `tar_plan()` for a
  drake-style DSL, and the factory-authoring helpers we'd build on.

**Where `ssdsims` extends the model:** Targetopia packages typically
produce *target factories* (lists of targets); the user still authors
their own `_targets.R`. M1 of this roadmap goes one step further and
mints a *full project directory* (analogous to
[`targets::use_targets()`](https://docs.ropensci.org/targets/reference/use_targets.html)
or `usethis::create_project()`), so a researcher running a one-off
simulation exercise doesn't have to learn `_targets.R` at all. The
target factories underneath that project are pure stantargets-style.

### Simulation-framework cousins

Pre-`targets` simulation tooling that influenced the M3 DSL:

* [**SimDesign**](https://cran.r-project.org/package=SimDesign) —
  Monte Carlo simulation framework with a tibble-shaped design grid
  and a `generate / analyse / summarise` triplet of user functions.
  Native HPC support including SLURM array jobs. Closest in spirit to
  what M3 looks like end-to-end; differs in that it doesn't lean on
  `targets` for caching/invalidation.
* [**simChef**](https://github.com/Yu-Group/simChef) — PCS framework
  (Data-Generating Process / Method / Evaluator / Visualizer), runs a
  Cartesian product across DGP × Method. Uses
  [`future`](https://future.futureverse.org/) /
  [`future.batchtools`](https://future.futureverse.org/) for HPC
  rather than `targets` / `crew.cluster`.
* [**simsalapar**](https://cran.r-project.org/package=simsalapar) —
  older but design-grid-and-graphics-focused; useful reference for
  result-table shape.

### HPC backends (M2)

* [**crew.cluster**](https://wlandau.github.io/crew.cluster/) — the
  targets-native path. `crew_controller_slurm()` is what M2 will
  emit. Persistent SLURM workers; per-worker resource templates.
* [**clustermq**](https://github.com/mschubert/clustermq) /
  [**slurmR**](https://uscbiostats.github.io/slurmR/) /
  [**future.batchtools**](https://future.futureverse.org/) —
  alternative SLURM bridges; we won't ship them by default but the
  generated `_targets.R` is small enough that a power user can swap
  controllers if their cluster needs something specific.
* The [targets HPC
  chapter](https://books.ropensci.org/targets/hpc.html) is the
  canonical writeup; we'll cite it from the minted project's README.

### Project-scaffolding precedent

* [`targets::use_targets()`](https://docs.ropensci.org/targets/reference/use_targets.html) —
  the official scaffold. Writes `_targets.R` with TODO comments. Good
  baseline; M1 is "use_targets, but with all the SSD-shaped TODOs
  already filled in."
* [`usethis`](https://usethis.r-lib.org/) project / package
  templates — same conceptual move (function-call generates a
  directory tree) for a different problem domain.

## Packaging strategy

**Direction:** keep everything in `ssdsims` and prefer fast shipping
over premature separation. Build on `tarchetypes` from day one.

The shortest path to a useful product is: one package, one install,
one release cycle. Splitting "primitives" from "workflow factories"
into separate packages buys conceptual cleanliness but costs release
velocity, dependency coordination, and discoverability. The
[Targetopia](https://wlandau.github.io/targetopia/) precedent (e.g.
`stantargets` separate from `cmdstanr`) is principled but optional —
many R packages successfully ship both primitives and workflow tools
together. We only extract when there's a concrete second consumer.

Building on `tarchetypes` from day one means M1's
`ssdsims_targets_project()` produces target factories that desugar to
`tarchetypes::tar_map()` / `tarchetypes::tar_group_by()` /
`tarchetypes::tar_combine()` rather than hand-rolled `tar_target()`
lists. Concretely, the experiments harness in this PR is being
migrated to that pattern now; the production minted projects will
follow the same shape.

### The boundary problem

R doesn't have a real internal-module concept — files are mostly a
convention. Practical options for marking the SSD-vocabulary layer:

* **File prefixes.** `R/targets-*.R` for the workflow tooling,
  `R/sim-*.R` (or no prefix) for the primitives. The boundary lives
  in the directory listing.
* **Roxygen `@keywords internal`** on factory helpers that aren't
  meant to be SSD-vocabulary-aware, so they aren't documented as
  domain functions.
* **Naming.** `ssdsims_project_*`, `ssdsims_branching_*`,
  `ssdsims_errors_*` for the workflow side; `ssd_*` for the
  primitives side (matching `ssdtools`'s convention). Anyone reading
  the NAMESPACE can tell what's what.

None of these are enforced. They drift unless we test for drift.

### Static-analysis tests for boundary maintenance

The package should ship a couple of automated checks that fail in
`R CMD check` when the boundary is crossed:

* **Primitives layer must not know about workflow.** A test that
  greps `R/sim-*.R` for forbidden tokens (`tar_target`, `tarchetypes`,
  `crew`, `duckplyr`, `arrow`). If any appear, the test fails with a
  pointer to which file to clean up.
* **Workflow layer must not bypass primitives.** A test that asserts
  every workflow factory body calls into `ssd_*` functions rather
  than reimplementing simulation logic. Cheap version: parse the
  factory files with `parse()` / `getParseData()`, walk the call
  tree, and assert that any call to `ssdtools::*` or `ssddata::*`
  goes via the primitive helpers, not directly.
* **Exported-name conventions.** A test that walks `NAMESPACE`, bins
  exports by prefix, and asserts each bin maps to one layer. New
  unprefixed exports trigger a failure that asks the contributor to
  pick a side. (lintr can be configured for this if we don't want to
  hand-roll.)
* **Dependency direction.** Using
  [`pkgnet`](https://github.com/uptake/pkgnet) or a small custom
  walker, assert the workflow files import from the primitive files
  but not the reverse. This catches accidental cycles before they
  become real coupling.

These run in CI, take seconds, and turn "we should be careful about
the boundary" into "the boundary is checked on every PR." When the
checks fail repeatedly because the boundary is genuinely wrong, that's
the signal to extract — not vibes.

### Extraction trigger

We extract the workflow layer into a separate package (`ssdtargets`?)
when:

1. A second domain package wants to depend on the workflow factories
   without inheriting SSD vocabulary, **or**
2. The static-analysis tests above keep needing exceptions (i.e. the
   "boundary" is moving), **or**
3. The dep weight of `tarchetypes` / `crew` / `crew.cluster` /
   `duckplyr` is genuinely hurting downstream consumers who just want
   the primitives.

Until one of those bites, the cost of a split exceeds the benefit.

## Out of scope (for now)

* Generic targets project generation outside the SSD domain. The DSL
  encodes SSD vocabulary (`ci_method`, `nboot`, `est_method`); a generic
  "any function over any grid" abstraction is a different package.
* Bayesian / Stan workflows. The current `ssd_hc_sims` path is
  frequentist; if Bayesian fits land in `ssdtools`, M2 may need to
  revisit worker walltimes.
* GUI / Shiny surface. R DSL is the canonical surface.

## Open questions

* **S3 credentials on HPC.** Most academic clusters don't have instance
  roles; the user has to ship credentials. We can document the
  `AWS_PROFILE` / `~/.aws/credentials` pattern but it's a real
  onboarding step — worth a "first SLURM run" cookbook in M2.
* **Random-number reproducibility across controllers.** ssdsims already
  uses L'Ecuyer-CMRG seeds keyed by `(start_sim, stream)`. Need to
  confirm SLURM workers see the same seed sequence as local crew
  workers. Likely fine, but worth a small property test in M2.
* **Heuristic batch sizing on HPC.** Local sweet-spot from the
  experiments is ~30–120 s per branch. On SLURM with multi-minute
  scheduler latency, the floor is likely higher; M2 should re-run the
  split-granularity experiment on a real cluster before locking
  `recommend_branching()` outputs for the SLURM compute target.
