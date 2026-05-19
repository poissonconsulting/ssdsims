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

The split-granularity finding from the experiments is folded into the
roadmap: M2/M3 ship heuristics that pick sensible batch sizes by
default, rather than asking the researcher.

## M1 — Reusable targets factory (local) `[next]`

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
* Output layout: hive-partitioned Parquet, written through duckplyr.
* Local execution only; `crew::crew_controller_local()` is the default
  controller.

**Done when:** a researcher can replicate `scripts/targets/` end-to-end
by calling `ssdsims_targets_project()` and `targets::tar_make()`, with
no hand-editing.

## M2 — SLURM HPC backend

Goal: the same `ssdsims_targets_project()` call produces a project that
runs on a SLURM cluster.

* Add a `compute = c("local", "slurm")` argument. Under `"slurm"` the
  minted `_targets.R` swaps `crew::crew_controller_local()` for
  `crew.cluster::crew_controller_slurm()`, with sensible defaults:
  - `tasks_max` sized from the input grid,
  - per-worker resources templated (cpus, memory, walltime, partition),
  - host- and queue-aware tuning hooks.
* File staging: minted project writes Parquet to a configurable
  `storage_root` (shared FS by default, S3 if requested) so workers and
  the controller process see the same outputs.
* A small `slurm_template.tmpl` ships with the package; researchers can
  override per-site.
* Auth / accounts are out of scope; the project assumes the user can
  `sbatch` already and the package wires up the controller, not the
  login.

**Done when:** the same scenario description from M1 runs on a SLURM
cluster against the same Parquet output, with no project-source
changes besides flipping `compute = "slurm"`.

## M3 — Higher-level DSL

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
    set_compute("local", workers = parallel::detectCores()) |>
    set_storage("data/") |>
    write_project()
  ```
* DSL emits the same targets project shape as M1/M2; everything below
  the DSL is unchanged.
* Validation: the DSL checks parameter combinations against
  `ssdtools::ssd_ci_methods()` / `ssd_est_methods()` etc. and reports
  configuration errors **at DSL time**, not at `tar_make()` time.
* Heuristic batch sizing: the DSL picks a default split based on the
  experiment results (≈30–120 s per branch locally, scaled up for
  HPC). Researchers can override with `set_branching(by = c("nrow",
  "ci_method"))`.

**Done when:** a researcher can express a multi-scenario simulation
exercise in <30 lines of R and run it on local or SLURM with one
command.

## M4 — Ergonomics and operations

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

## Out of scope (for now)

* Generic targets project generation outside the SSD domain. The DSL
  encodes SSD vocabulary (`ci_method`, `nboot`, `est_method`); a generic
  "any function over any grid" abstraction is a different package.
* Bayesian / Stan workflows. The current `ssd_hc_sims` path is
  frequentist; if Bayesian fits land in `ssdtools`, M2 may need to
  revisit worker walltimes.
* GUI / Shiny surface. R DSL is the canonical surface.

## Open questions

* **Storage backends.** Parquet on a shared filesystem is the default,
  but how seriously do we want to support object stores (S3, GCS)? M2
  may answer this; tooling-wise it's a controller config issue rather
  than a code change.
* **Random-number reproducibility across controllers.** ssdsims already
  uses L'Ecuyer-CMRG seeds keyed by `(start_sim, stream)`. Need to
  confirm SLURM workers see the same seed sequence as local crew
  workers. Likely fine, but worth a small property test in M2.
* **Heuristic batch sizing on HPC.** Local sweet-spot from the
  experiments is ~30–120 s per branch. On SLURM with multi-minute
  scheduler latency, the floor is likely higher; M2 should re-run the
  split-granularity experiment on a real cluster before locking
  defaults.
