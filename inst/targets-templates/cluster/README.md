# ssdsims on a SLURM cluster — zero to a running job

This is the editable **cluster** targets template (a sibling of `small/` and
`large/`). It runs an ssdsims scenario across a SLURM cluster by dispatching the
scenario's shards as `crew.cluster` jobs. The pipeline **shape** and the per-task
**content + RNG** are exactly the local `large/` template's — only the `crew`
controller changes (`TARGETS-DESIGN.md` §4). So the per-task `sample`/`fit`/`hc`
results are byte-identical to the local run; the cluster is just a faster
backend.

Three ingredients assemble into `_targets.R`; **only B is cluster-specific**:

| Ingredient | What | Where |
|---|---|---|
| **A — shape** | the per-shard target graph | `ssd_scenario_targets()` (reused verbatim) |
| **B — backend** | the SLURM `crew` controller (queue, modules, scratch, workers, walltime) | the **one editable block** in `_targets.R` |
| **C — content** | the scenario (seed, datasets, grids) | `scenario.R` |

The files: `_targets.R` (pipeline + controller + probe wiring), `functions.R`
(the probe body + the shard-gating helper, sourced by `_targets.R`),
`scenario.R` (the study), `run.R` (the targets driver), `run-serial.R`
(single-core oracle).

Get the template:

```r
dir <- system.file("targets-templates", "cluster", package = "ssdsims")
file.copy(list.files(dir, full.names = TRUE, all.files = TRUE, no.. = TRUE), ".")
```

You do **not** need to know `crew` or `targets` to follow this guide — only your
cluster's own (non-R) usage instructions. Four steps.

---

## Step 1 — Map your site's SLURM instructions to the controller

Your real starting point is your site's documentation: how to log in, submit an
`sbatch` job, which partition/queue and account/allocation to use, the `module`
system that provides R, the scratch filesystem, and the walltime/core limits.
None of that mentions R, `crew`, or `targets`. This table bridges it to the
**one editable controller block** in `_targets.R`
(`crew.cluster::crew_controller_slurm()` /
`crew.cluster::crew_options_slurm()` — see `?crew.cluster::crew_controller_slurm`):

| Your site says… | …goes here |
|---|---|
| log in to the **login/submit node** | run `run.R` from there (where `sbatch` is on `PATH`) |
| submit with **`sbatch`** | nothing to set — `crew.cluster` calls `sbatch` for you |
| **partition / queue** name | `crew_options_slurm(partition = "...")` |
| **account / allocation** code | a `"#SBATCH --account=..."` line in `script_lines` |
| **`module load R/4.x`** (and any deps) | a `"module load R/4.x"` line in `script_lines` |
| **scratch filesystem** path | an `"export TMPDIR=/scratch/$USER/..."` line in `script_lines` |
| **walltime** limit (per job) | `crew_options_slurm(time_minutes = ...)` |
| **cores** per task | `crew_options_slurm(cpus_per_task = ...)` |
| **memory** per cpu | `crew_options_slurm(memory_gigabytes_per_cpu = ...)` |
| how many jobs you may run at once | `crew_controller_slurm(workers = ...)` |

Anything not covered by a named argument (a constraint, a QOS, a `--gres`) goes
in `script_lines` as a literal `#SBATCH` line — `script_lines` is injected
verbatim into the generated `sbatch` script. Also set `expected_r_version` (top
of the editable block) to the major.minor your `module load` provides, so the
probe (step 2) can confirm it.

### Backend B prerequisite — the worker install path

A SLURM worker is a fresh R process on a compute node. It must be able to
`library(ssdsims)` **without compiling** the dependency tree from source (compute
nodes are often offline or slow to build). The crew labs pinned this down with a
**ManyLinux binary install path**: install ssdsims and its dependencies as
pre-built Linux binaries (e.g. from the Posit Public Package Manager binary
repository for your distro) into the library your `module load R/...` exposes, so
the worker resolves everything at load time. Put the `module load` that exposes
that library in `script_lines`. The probe in step 2 is the **login-node
prerequisite checker**: it fails loudly if a worker cannot load ssdsims, so you
fix the install/module path before launching the study.

---

## Step 2 — Confirm the mapping with the probe

`_targets.R` defines a **probe** target that `tar_make()` builds *first*, as an
explicit upstream dependency of every scenario shard. Building it dispatches
**one** SLURM job through your controller and, inside that job, checks the things
that actually break a real run:

1. **R resolves** at `expected_r_version` — else fix the `module load` line;
2. **`library(ssdsims)` loads** — else fix the worker install/module path
   (backend B above);
3. **`tempdir()` is writable** — else fix the scratch `export TMPDIR=...` line.

A green probe returns a small witness (the worker's R version + node id) and
means your controller block matches the site. A red probe **blocks the scenario
shards** and names exactly which check failed, with the fix:

- *no job dispatched* → controller / queue / account wiring;
- *R or ssdsims missing on the worker* → `module load` / install path;
- *scratch not writable* → the `TMPDIR` / scratch path.

So you debug the cluster wiring, never an obscure scenario error.

---

## Step 3 — Run the built-in `small` scenario (minimal first job)

For your first job, make the scenario tiny and cheap so you are testing the
**wiring**, not waiting on a big study. Replace `scenario.R` with the built-in
`small` scenario:

```r
file.copy(
  system.file("targets-templates", "small", "scenario.R", package = "ssdsims"),
  "scenario.R",
  overwrite = TRUE
)
```

Then run the pipeline:

```r
source("run.R") # or, from a shell on the login node:  Rscript run.R
```

`run.R` builds the probe first, then dispatches the `small` scenario's shards
across SLURM jobs (independent shards run concurrently), writes one Parquet per
shard under `results/layout=<hash>/<step>/...`, unions them into
`results/layout=<hash>/summary.parquet`, and prints the probe witness and a peek
at the estimates. That is your first running cluster job, end to end.

> **No cluster handy?** `run.R` guards: if `crew.cluster` is not installed or
> `sbatch` is not on `PATH`, it aborts with a clear message naming the missing
> prerequisite. To run the **same study off a cluster** (no scheduler), use the
> `large/` template — it builds the identical pipeline (same factory + scenario)
> under a `crew::crew_controller_local()` controller, and `run-serial.R` asserts
> the results are byte-identical.

---

## Step 4 — Swap in your own scenario

Once the minimal job succeeds, edit `scenario.R` to your own study (start from
the wider sweep this template ships, or `large/scenario.R`). **Leave `_targets.R`
and the controller block unchanged** — the scenario and the
`ssd_scenario_targets()` call are scheduler-independent. Run `run.R` again.

---

## Shard ↔ SLURM-job packing

Shards are the **unit of parallelism**: independent shard targets run
concurrently across your SLURM jobs. How many shard targets ride in one job is a
`crew` configuration knob (`workers` caps concurrent jobs; `tasks_max` on the
controller packs several shards into one worker's lifetime — "many-to-one";
`tasks_max = 1L` gives one shard per job — "one-to-one"). It is documented in
`_targets.R`, not hard-coded as 1:1 (`TARGETS-DESIGN.md` §11 Q6).

## See also

- `TARGETS-DESIGN.md` §4 (from local to a cluster), §11 (open questions), §12
  (`cluster-pipeline`).
- The ["Running a Sharded Pipeline"](https://poissonconsulting.github.io/ssdsims/articles/sharded-pipeline.html)
  vignette — the `small`/`large`/`cluster` template trio.
- `?ssd_scenario_targets`, `?crew.cluster::crew_controller_slurm`.
