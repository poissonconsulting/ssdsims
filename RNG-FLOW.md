# RNG flow in `ssdsims` — from `scripts/example.R`

This note traces what happens when `scripts/example.R` runs the three-step
pipeline

```r
sims <- ssd_sim_data(ssddata::ccme_boron)
fits <- ssd_fit_dists_sims(sims)
hcs  <- ssd_hc_sims(fits)
```

and answers four questions:

1. Where does stochasticity enter, and how is it handled?
2. Is the L'Ecuyer-CMRG state that `ssd_sim_data()`, `fit_dists_seed()` and
   `hc_seed()` each derive for a given `sim` actually the *same* state?
3. What is a *stream*, and is it used for in-package parallelism?
4. What does the PoC in **PR #59** add, and what is still missing for the
   goal of "parallel + reproducible + extensible"?

References below are to current `main` (commit at time of writing) unless
prefixed with `[PoC]`, which points at PR #59.

---

## 1. Pipeline overview

The example does three things, each driven from one tibble row per `(sim,
stream)` combination (cross-joined with the parameter grid):

```
                   ssddata::ccme_boron
                          │
                          ▼
              ┌───────────────────────────┐
              │   ssd_sim_data(data, …)   │   ┐
              │   R/simulate-data.R       │   │  Stage 1 — Simulate
              │   one row per (sim, …)    │   │  resampled data sets
              └─────────────┬─────────────┘   ┘
                            │  nested tibble with `sim`, `stream`,
                            │  `nrow`, `replace`, `data` columns
                            ▼
              ┌───────────────────────────┐
              │  ssd_fit_dists_sims(x,…)  │   ┐
              │   R/fit-dists-sims.R      │   │  Stage 2 — Fit
              │   delegates per-row to    │   │  distributions
              │   fit_dists_seed()        │   ┘
              │   R/internal.R            │
              └─────────────┬─────────────┘
                            │  + `fits` list-column (`fitdists` objects)
                            ▼
              ┌───────────────────────────┐
              │     ssd_hc_sims(x, …)     │   ┐
              │     R/hc-sims.R           │   │  Stage 3 — Bootstrap
              │     delegates per-row to  │   │  hazard concentration
              │     hc_seed()             │   ┘
              │     R/internal.R          │
              └─────────────┬─────────────┘
                            │  + `hc` list-column (tibbles of hc estimates)
                            ▼
                          result
```

Each stage is "row-major": one `purrr::pmap()` over the tibble rows, with the
`sim` and `stream` columns carried through. The cross-joining of additional
parameter vectors (e.g. `nboot = c(1, 5, 10, …)` in `example.R`) happens
*inside* each stage with `tidyr::expand_grid()` / `dplyr::cross_join()`, so
the row count of the tibble grows but the `(sim, stream)` pair on each row
stays as the RNG anchor.

---

## 2. Where stochasticity enters, and how it is anchored

Three places consume random numbers:

| Stage          | What is sampled                                            | Where in code                              |
| -------------- | ---------------------------------------------------------- | ------------------------------------------ |
| Data simulate  | `dplyr::slice_sample()` (or `ssdtools::ssd_r*()` rvg)      | `R/internal.R::slice_sample_state()` / `do_call_seed()` |
| Fit            | optimizer start values inside `ssdtools::ssd_fit_dists()`  | `R/internal.R::fit_dists_seed()`           |
| Bootstrap HC   | parametric/non-parametric bootstrap inside `ssd_hc()`      | `R/internal.R::hc_seed()`                  |

Each of the three call sites independently asks for the **same**
L'Ecuyer-CMRG substream state for its `(sim, stream)` row, then runs its
operation inside `with_lecuyer_cmrg_state(state, { … })`. The `with_…`
helper is `withr::defer`-based: it saves the global RNG state on entry,
installs `state`, runs the body, and restores the global state on exit.
So nothing leaks between rows or between stages.

### How the state is derived

`R/lecuyer-cmrg-seed.R::get_lecuyer_cmrg_stream_states(seed, nsim,
stream, start_sim)` is the engine. With the convention `s = stream`, `k =
start_sim`, `m = nsim`, it produces:

```
        set.seed(seed)              ← scalar seed → MT global state
                │
                ▼
   get_lecuyer_cmrg_state()         ← RNGkind("L'Ecuyer-CMRG"); set.seed(rinteger(1))
                │
                ▼
        L'Ecuyer master state
                │
   nextRNGStream  × s−1             ← advance s−1 streams
                │
                ▼
   nextRNGSubStream × k−1           ← advance k−1 substreams
                │
                ▼
          states[[1]]               ← state used for (sim = k,   stream = s)
                │
   nextRNGSubStream × 1
                │
                ▼
          states[[2]]               ← state for (sim = k+1, stream = s)
                │  ⋮
                ▼
          states[[m]]               ← state for (sim = k+m−1, stream = s)
```

`get_lecuyer_cmrg_stream_state(seed, stream, start_sim)` is just
`get_lecuyer_cmrg_stream_states(seed, nsim = 1, …)[[1]]`. The per-row
helpers (`fit_dists_seed()`, `hc_seed()`) use this single-row form.

---

## 3. The `sim` column identity claim — confirmed *with a caveat*

**Claim (from the user's question):** the `sim` column on every row of the
nested tibble identifies a unique L'Ecuyer-CMRG substream, and for a fixed
`(seed, stream, sim)` the *same* state is used by `ssd_sim_data()`,
`fit_dists_seed()` and `hc_seed()`.

**Verdict — design intent: confirmed.** All three call sites pass the
same `(seed, stream, sim)` triple through
`get_lecuyer_cmrg_stream_state{,s}()`, and that helper is a pure function
of those three inputs.

Empirical check (Rscript against current `main`, with `seed = 42`):

```
fit_seed(sim=1)         == hc_seed(sim=1)             TRUE
stream=1 != stream=2 (same sim)                       TRUE
sim k+1 == nextRNGSubStream(sim k)                    TRUE
stream s+1,sim=1 == nextRNGStream(stream s,sim=1)     TRUE
```

So the substream lattice is exactly the canonical L'Ecuyer-CMRG one:

```
            stream=1            stream=2            stream=3
sim=1   ●──nextSubStream──▶●   ●                    ●
        │                      ▲                    ▲
        nextStream             │                    │
        │                      nextStream           nextStream
        ▼                      │                    │
        ●  ───────────────────●  ──────────────────●
sim=2   ●  ───────────────────●  ──────────────────●
sim=3   ●                      ●                    ●
```

**Caveat — implementation bug uncovered while verifying.** The check
`states_data[[1]] == fit_seed(sim=1)` initially came back **FALSE** in a
fresh R session. Root cause:

- `get_lecuyer_cmrg_state()` (`R/lecuyer-cmrg-seed.R:156`) calls
  `RNGkind("L'Ecuyer-CMRG", …)`, which mutates the global RNG kind.
- `get_lecuyer_cmrg_stream_states()` wraps that in
  `on.exit(set_state(ostate))`, **but** if `.Random.seed` did not exist
  before the call (`get_state()` returns `NULL`), `set_state(NULL)` is a
  documented no-op — the RNG kind is *not* restored.
- The next call therefore enters with kind already L'Ecuyer-CMRG, and
  `set.seed(seed)` produces a different starting state than it would
  under Mersenne-Twister. Same inputs, different output.

```
   first call                     second call
   ──────────                     ───────────
   kind = MT  ─┐                  kind = LEcuyer  ─┐    (residue from
   set.seed(S) ├─ deterministic   set.seed(S)      ├─   first call!)
   RNGkind LE ─┘    state A      (RNGkind LE noop) │    state B ≠ A
                                 set.seed(rint())  ┘
```

In normal usage the *practical* impact is small — once anything in the
session has touched the RNG, `.Random.seed` exists and `on.exit` restores
properly — but it is real and reproducible from a clean `R --vanilla`.
**The PoC fixes this** by pinning `RNGkind("Mersenne-Twister", …)`
*before* `set.seed(seed)` inside `get_lecuyer_cmrg_stream_states()`
(`R/lecuyer-cmrg-seed.R:178` on `claude/adoring-ramanujan-rXimG`), with
an explanatory comment.

### Implication of "same state, three stages"

Once the bug is set aside, the same L'Ecuyer-CMRG state seeds the data
draw *and* the fit *and* the HC bootstrap for a given `(sim, stream)`. The
three operations are different (`slice_sample` vs. optimizer start values
vs. parametric bootstrap), so the random sequences they actually *consume*
diverge after the first draw — but they share their first few uniform
deviates. That is:

```
   .state(sim, stream)  ┐
                        ├── slice_sample(data, n=nrow)  ← stage 1
                        │   ░░░░░░░░░░░░░░░░░░░░░░░░░░
                        │
   .state(sim, stream)  ┤
                        ├── ssd_fit_dists(...) starts  ← stage 2
                        │   ▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒
                        │
   .state(sim, stream)  ┤
                        ├── bootstrap inside ssd_hc()  ← stage 3
                        ┘   ▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓
   (each "with_state" then restores the prior global state)
```

The three streams are **identical for their first N₁ uniforms**, then
diverge as soon as the operations differ. In practice this only matters
if someone wanted to argue that data, fit and HC for the same `sim` are
*statistically independent* — they are not, strictly speaking; they are
correlated through their shared initial state. The PoC removes this
correlation (see §5).

---

## 4. Streams — concept vs. usage in `ssdsims`

L'Ecuyer-CMRG defines two levels of advancement:

- `parallel::nextRNGStream(s)` — jumps ~2^127 draws ahead; the canonical
  way to give *different parallel workers* statistically independent
  sequences.
- `parallel::nextRNGSubStream(s)` — jumps a smaller (~2^76) amount;
  intended to separate replicates within the same worker.

`ssdsims` maps these to:

| L'Ecuyer term | `ssdsims` user-facing name | What it means here                          |
| ------------- | -------------------------- | ------------------------------------------- |
| stream        | `stream` (default `1L`)    | An independent "batch axis"                 |
| sub-stream    | `sim` (1, 2, …, `nsim`)    | Replicate index within a batch              |

**Is the stream concept used for parallelism inside `ssdsims`?** No. There
is no `parallel`/`furrr`/`mirai` call in the package. Every call to
`ssd_sim_data()` / `ssd_fit_dists_sims()` / `ssd_hc_sims()` uses a single
scalar `stream` (default `1L`, overridable via the `ssdsims.stream`
option or the explicit argument). The package iterates with
`purrr::pmap()`, sequentially.

**What is `stream` actually for, then?** It is a *user-facing
extensibility hook*. By calling

```r
sims1 <- ssd_sim_data(data, seed = 42, stream = 1L, nsim = 100)   # batch A
sims2 <- ssd_sim_data(data, seed = 42, stream = 2L, nsim = 100)   # batch B
```

the user gets two batches whose substreams do not overlap (they are
separated by `nextRNGStream`, i.e. ~2^127 draws), but both batches are
reproducible from the *same* master seed. In other words, `stream` is the
existing "I want to add more sims without disturbing the ones I already
have" axis — exactly the lever the user's goal #3 (extensibility) wants
to push on. It just hasn't been wired into a workflow yet.

---

## 5. PoC review (PR #59 — `claude/adoring-ramanujan-rXimG`)

The PoC introduces a parallel set of "v2" entry points
(`ssd_sim_data2()`, `ssd_run_scenario2()`) that hold the scenario as an
S3 object and a fully-expanded task grid, plus a `targets` interface and
four example pipelines under `inst/targets-examples/`.

### What it gets right (against the three goals)

#### Parallelization ✓ (well, "wired up")

```
   ssd_sim_data2(...) ── builds ──▶  ssdsims_scenario (S3)
                                            │
                                            ▼
                       ssd_scenario_tasks(scenario)
                                            │  full cross-join
                                            ▼
                            tasks tibble (one row = one task)
                            ├─ sim, stream, nrow, …
                            ├─ .state_data, .state_fit, .state_hc
                            └─ .gen_kind, .gen_x / .gen_fn / .gen_args
                                            │
                          tar_group_by(task_groups, tasks, <axis>)
                                            │
                       pattern = map(task_groups), workers = N
                                            ▼
                       crew_controller_local()   ←   per-job parquet
```

`R/scenario-job.R::ssd_run_job(tasks, scenario)` runs an arbitrary subset
of rows, so a `tar_target(... pattern = map(task_groups))` can fan out N
ways and each worker writes one Parquet file. Four granularities are
shipped: per-task, per-sim, per-parameter-slice, whole-scenario.

#### Reproducibility ✓ (with the bug fix)

The substream states are computed **up front** in `build_data_grid()`
(`R/run-scenario2.R:99-114`) and stored on each task row as
`.state_data`, `.state_fit`, `.state_hc` integer-vector list-columns.
Each per-task call site (`generate_task_data`, `fit_task_dists`,
`hc_task`) then does `local_lecuyer_cmrg_state(task$.state_*)` instead of
re-deriving from the master seed. Two consequences:

1. The starting RNG state of every task is **explicit** and survives
   serialization (it's a length-7 `integer` vector).
2. The data/fit/hc states are now **independent substreams of one
   another** (the PoC chains `nextRNGSubStream` from `.state_data` →
   `.state_fit` → `.state_hc`), so the "shared first uniforms" issue
   from §3 disappears — data, fit and HC for the same `sim` are now
   statistically independent.

Plus the bug fix in §3: `R/lecuyer-cmrg-seed.R:178` pins
`RNGkind("Mersenne-Twister", …)` before `set.seed(seed)`, with a comment
that exactly matches the symptom uncovered above.

#### Extensibility — *partial*

The `stream` axis is preserved (still on every task row), so in principle
"start a new stream" works. But the PoC does **not** demonstrate the
"DAG-of-DAGs" pattern the goal description sketched:

- All four example pipelines hardcode `seed = 42` and `stream` defaults
  to `1L`. None of them shows extending a previous run with `stream = 2`.
- There is no helper to "point at a previous run" — concretely, no
  function that takes an existing scenario (or a Parquet output
  directory) and produces a derived scenario whose `(stream, start_sim)`
  picks up where the previous one left off.
- The `stable_config` indirection in the `_targets.R` files demonstrates
  awareness of cache hash stability, but the README admits the
  `nsim`-grow cache hit is only ~half (`inst/targets-examples/README.md`
  lines 53-66) — `targets` is invalidating branches whose content the
  PoC believes is byte-identical.

### What needs to be added to reach the stated goal

```
   GOAL:    one declarative scenario  ──▶  many jobs running in parallel
            ───────────────────────       ───────────────────────────────
            ssd_sim_data2(...) ✓          targets + crew ✓ (per-task etc.)
            S3 scenario ✓                 per-task L'Ecuyer state ✓
                                  + …
   GOAL:    bit-for-bit reproducible across reruns and machines
            ───────────────────────────────────────────────────
            master seed → substream lattice ✓
            RNGkind pinned ✓
            per-task state stored on the row ✓
            ⚠ but: targets cache-hit on nsim-grow is empirically only ~50%
            ⚠ and: scenario hash is not separated from "current nsim"

   GOAL:    extensible — "scenarios can be defined and then extended"
            ───────────────────────────────────────────────────────
            ✗ no fork helper (no `ssd_extend_scenario(prev, …)`)
            ✗ no "load previous run from Parquet and continue" path
            ✗ no DAG-of-DAGs primitive (no nested tar_make_subset /
              tar_objects loaded from a sibling project)
```

Concrete additions, ordered from cheapest to most architectural:

1. **Diagnose the `nsim`-grow cache miss.** The PoC's README calls it
   *"a `targets` invalidation quirk we haven't fully diagnosed"*. With
   `tar_outdated()` and `tar_meta(fields = c("name", "data", "depend"))`
   it should be possible to identify which dependency hash is shifting.
   Likely culprits: the `scenario` target's hash includes `nsim`, so
   every branch depends transitively on it; the `stable_config` buffer
   strips most fields but does not strip everything that flows in. If
   that's the cause, `tasks` (or each row of it) needs to be the unit
   that downstream branches depend on, not `scenario`.

2. **Stable, content-addressed task IDs.** Right now `task_id` is
   `seq_len(nrow(t))` (`inst/targets-examples/per-task/_targets.R:65`),
   which is positional. If `nrow` or `dist_sim` is reordered, every
   task_id shifts. A hash of `(seed, stream, sim, nrow, replace,
   dist_sim, …)` — or simply a tuple of those fields — would survive
   reordering and would also be the natural primary key for an extend
   operation.

3. **An explicit "extend" entry point.** Sketch:

   ```r
   ssd_extend_scenario(prev_scenario_or_dir,
                       nsim = NULL,          # adds substreams
                       stream = NULL,        # forks to a new batch axis
                       ...)
   ```

   - `stream = old_max + 1` is the "fork a new independent batch" path.
   - `start_sim = old_max + 1, nsim = N` is the "extend the existing
     batch with more sims" path.
   - Returns a new `ssdsims_scenario` whose task grid contains *only*
     the new tasks. Old Parquet files remain valid (their task_ids
     don't change), and a thin merge target reads the union.

4. **DAG-of-DAGs.** If the goal is literally one `targets` project per
   scenario, with downstream meta-projects pulling from upstream ones,
   the building block is `targets::tar_read(name, store = "../prev/_targets")`
   or the `tarchetypes::tar_render_rep`-style cross-project link. The
   PoC doesn't have this yet; it would land as a fifth example
   pipeline (`inst/targets-examples/extend-previous/`) that points at
   the per-sim project's `results/` directory and adds `stream = 2`.

5. **Persist the starting RNG state with the scenario, not just the
   seed.** The user's question framed this exactly: *"does this mean we
   want to define and save the starting RNG state up front for each
   task?"*. The PoC already does this on the task row
   (`.state_{data,fit,hc}`), but it does **not** persist the *scenario*
   master state (it persists only `seed`). Two implications:

   - If the bug fix in `get_lecuyer_cmrg_stream_states()` ever regresses
     or its derivation changes (e.g. someone "improves" the seed→state
     transformation), every old Parquet becomes meaningless even though
     `seed` is unchanged.
   - For extend, downstream pipelines need the master L'Ecuyer state
     *exactly as it was on the original run*, not just the integer
     seed.

   Concrete fix: store the post-`set.seed(seed)`, post-`RNGkind` master
   state on `scenario$sim` (one length-7 integer vector), and have
   `build_data_grid()` advance from *that* with `nextRNGStream` /
   `nextRNGSubStream` rather than re-running `set.seed(seed)`. The
   master state then becomes the only thing extensions depend on, not
   the implementation of the seed→state transformation.

### Smaller observations on PR #59 (non-blocking)

- `ssd_run_scenario2()` is a sequential `purrr::map()` over the task
  grid (`R/run-scenario2.R:41-45`). For pure single-process use it's
  fine, but if it's meant to be the in-R parallel entry point it should
  also accept a `furrr::future_map` / `mirai_map` strategy. (The
  `targets` examples bypass this and call `ssd_run_job()` directly.)
- The `.gen_x`, `.gen_fn`, `.gen_args` columns store the generator
  (potentially a heavy fitdists object) on **every** task row via
  `replicate(nrow(grid), gen$x, simplify = FALSE)`. This is fine in RAM
  (R copy-on-write) but bloats Parquet output by N copies of the same
  blob. The qs2-encoding path in `ssd_write_job_parquet()` should
  dedupe or, more practically, the generator should be looked up by
  reference from the scenario at run time rather than carried on the
  row.
- `ssd_run_job(tasks, scenario)` requires the caller to pass both
  pieces. With a `stable_config` indirection in every `_targets.R`
  that's already the pattern; it might be worth promoting
  `stable_config` to a helper (`ssd_stable_scenario(scenario)`) and
  documenting which scenario fields a job actually needs.

---

## TL;DR for each question

> *Where does stochasticity come to play? How is it handled?*

Three places — data resample, fit start values, HC bootstrap — each
opened inside `with_lecuyer_cmrg_state(state, { … })` so that the global
RNG is unaffected. The `state` is a precomputed L'Ecuyer-CMRG substream,
keyed by `(seed, stream, sim)`.

> *Is the `sim` column unique per slice, and does the same `sim` yield
> the same state across `ssd_sim_data()`, `fit_dists_seed()` and
> `hc_seed()`?*

Yes by design and yes in practice — verified empirically — but the
*current* code has a latent bug where the RNGkind side-effect from
`get_lecuyer_cmrg_state()` can leak across calls when `.Random.seed`
didn't exist on entry, breaking reproducibility. The PoC fixes this by
pinning `RNGkind("Mersenne-Twister")` before `set.seed(seed)`. The PoC
also changes the design so that data, fit and HC use *independent*
substreams of the same master rather than all starting from the same
state.

> *Are streams used inside the package, or purely user-defined?*

Purely user-defined. No internal call advances `stream`; iteration is
single-stream, single-process `purrr::pmap()`. `stream` is the natural
hook for the extensibility goal, but no current code exercises it.

> *What does the PoC add, and what is still missing?*

Adds: scenario S3 + task-grid materialization + per-task L'Ecuyer state
columns + targets/crew pipelines at four granularities + Parquet/duckplyr
I/O with qs2-encoded blob columns. Fixes the RNGkind-leak bug, decouples
data/fit/hc randomness.

Missing for the full goal: an `ssd_extend_scenario()` primitive, stable
content-addressed task IDs (currently positional), persistence of the
*master L'Ecuyer state* on the scenario (currently only `seed`), a
worked "extend a previous run by starting a new stream" example, and a
diagnosis of why `targets` invalidates ~50% of branches on `nsim`-grow
even though every per-task state is content-stable.
