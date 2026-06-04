## Context

`ssd-define-scenario` landed a declarative `ssdsims_scenario` and an `ssd_data()` collector that are **data-frame-only**. `ssd_run_scenario()`, by contrast, dispatches over five `ssd_sim_data()` S3 methods — `data.frame`, `fitdists`, `tmbfit`, `function`, and `character` (a function-name string) — where the last four are *data generators*: they produce data rather than carrying it. This change widens `ssd_data()` / `ssd_define_scenario()` to accept those four generator types.

The original plan recorded each generator as an *inert descriptor* and deferred materialisation to a targets-only `registry` step (`TARGETS-DESIGN.md` §1.1). Exploration on this branch **reversed that**: datasets — generated ones included — are tiny, so a generator is **materialised once, eagerly, in the constructor**, and the realised tibble is carried inline on the scenario exactly like a data-frame input. The scenario object is the transport; the name-only `registry` regeneration path is a later decoupling (`dataset-provenance`, §12), taken only if dataset size ever forces it. `ssd_run_scenario()` is untouched.

## Goals / Non-Goals

**Goals:**

- Accept the same input set as `ssd_run_scenario()` in `ssd_data()` / `ssd_define_scenario()`: data frame, `fitdists`, `tmbfit`, function, function-name string — singly or in a list.
- **Materialise** each generator once, at construction, to a validated tibble stored inline in `scenario$data`, indistinguishable downstream from a data-frame input.
- Seed generation reproducibly and **independently of the scenario** via a dedicated `ssd_data(..., .seed = NULL)` argument, with the dataset **name as the dqrng stream**.
- Enforce a dqrng-only, reproducible generator contract via a post-hoc RNG-state check.
- Reuse the existing name-derivation machinery (`expr_to_name()`, `list_expr_names()`, argument names) unchanged.

**Non-Goals:**

- Persisting datasets to Parquet, or a name-only registry that *regenerates* them — deferred to the `registry` / `dataset-provenance` steps (§1.1, §12). Here the realised tibble rides on the scenario.
- Per-task simulation RNG (§2). The scenario's `seed` governs `sample`/`fit`/`hc`; the generation `.seed` is separate and plays **no** part in the task primers.
- Recording generator provenance (the function, its `.seed`, args) as scenario metadata — a separate, deferred roadmap item (`dataset-provenance`). For now the **generator code is the provenance**; execution-environment capture is out of scope entirely.
- Changing the `fit`/`hc` grids, `partition_by`, the `ci = FALSE` rule, or the legacy `ssd_run_scenario()` / `ssd_sim_data()` dispatch.

## Decisions

### Decision: materialise generators eagerly into the collection (no descriptor, no extra step)

`ssd_data()` returns a named list of **tibbles**. A generator input is run through its `ssd_sim_data()` method **at construction** and the resulting tibble is stored under its derived name, so the collection stays homogeneous and every downstream consumer — #80's `ssd_run_scenario_baseline()` reading `scenario$data[[dataset]]`, the task tables, the future `registry` — sees a data frame. There is **no** persistent generator descriptor and **no** separate `generate` step.

*Why eager-and-inline rather than an inert descriptor materialised later?* Datasets are tiny — even generated ones — so transporting the realised bytes on the scenario is cheap and makes a generated dataset reproducible *by being kept*, not by regeneration. This removes the descriptor type, the heterogeneous-collection handling, and the baseline runner's abort guard the earlier draft needed. *Alternative considered:* name-only + a `registry` target that regenerates on demand — deferred (`dataset-provenance`, §12); revisit only if dataset size makes inline transport too heavy.

### Decision: `ssd_data(..., .seed = NULL)` seeds generation; the name is the dqrng stream

Generation is **decoupled from the scenario seed**. `ssd_data()` gains a dot-prefixed `.seed` (default `NULL`); each generator is materialised under a scoped

```r
local_dqrng_state(.seed, task_primer(list(dataset = "<name>")))
```

so the **dataset name is the dqrng `stream`** and `.seed` is the base seed. A single `.seed` therefore fans out reproducibly across every named generator in the call, each on its own independent stream: `hash({dataset})` differs per name, and differs from the `sample` primer `hash({dataset, sim, replace})`, so generation and the per-sim draws never share a stream.

The `.seed` dot-prefix is deliberate. `ssd_data(...)` takes **dataset names as argument names**, so a bare `seed =` would be a dataset named `"seed"`; `.seed` is a formal (never absorbed into `...`), and R will not partial-match `seed=` onto it (the leading dot blocks the prefix match).

*Why reuse `task_primer()` for the stream but not `scenario$seed` for the base?* The seed *source* is what must be decoupled (a generated dataset is a fixture, not part of the experiment's randomness); the primer *hash* is just a convenient, collision-resistant stream allocator, so reusing it keeps stream separation consistent without recoupling to the scenario seed.

- **Q1 (resolved):** supplying `.seed` when the call has **no** generator inputs aborts (user-facing frame) — it is almost always a mistake.
- **Q2 (resolved):** one `.seed` per `ssd_data()` call; a per-generator `.seed` override is deferred until a real need appears (names-as-streams already separate the draws).

### Decision: dqrng-only generators, enforced by a post-hoc RNG-state check

A generator must be a **pure function** of its `.seed`-via-dqrng, with no side effects. The constructor snapshots both RNG states — base R `.Random.seed` and the dqrng state — around the scoped run and applies:

| generator touches | `.seed = NULL` | `.seed = <int>` |
| --- | --- | --- |
| nothing (pure) | ok | ok (`.seed` unused) |
| dqrng | abort: *"consumes RNG — supply `.seed`"* | ok, reproducible (stream = name) |
| base R `.Random.seed` | abort: *"use dqrng, not base R"* | abort: *"use dqrng, not base R"* |

Base-R RNG is always rejected — we only seed dqrng, so we could not make a base-R draw reproducible. dqrng usage is allowed but forces a `.seed`. Because the run is scoped, the constructor leaves global RNG state unchanged: the existing unchanged-`.Random.seed` invariant still holds, now by *restoration* rather than by *not drawing*. (Residual blind spot: a generator reaching for a C-level RNG with no R-visible state escapes detection — acceptable under the pure/no-side-effects contract.)

### Decision: route on input type in one place, reuse name derivation

`ssd_data()` / `scenario_dataset_names()` branch on input type at a single junction; one `classify_input(value, expr, name, call)` helper validates and **materialises** each element to a tibble. Name derivation is unchanged: argument name → `expr_to_name()` (symbol / `::` call) → explicit `name=` / list name; a function-name string is its own name and the resolution target, resolved with `get0()` / `match.fun()` to a **bare name** only (no `eval(parse())`). Dispatch on the most specific class first (`tmbfit` before `fitdists`), matching the S3 methods.

### Decision: structural validation in the user-facing frame

Per the error-origin convention, validation aborts with `chk::abort_chk(..., call = call)` where `call` is the exported frame. A function generator is checked for the single-argument (`n`) shape; a function-name string must resolve to a function; `fitdists` / `tmbfit` get an `inherits()` check. The post-hoc RNG check (above) runs immediately after a generator executes, in the same frame.

## Risks / Trade-offs

- **The constructor now runs user code and draws RNG** → it can be slow or throw for an expensive or buggy generator, where construction used to be inert. Mitigation: datasets are tiny by assumption; the scoped seed restores global RNG; failures abort in the user-facing frame naming the offending dataset.
- **Two-seed reproducibility story** → `scenario$seed` reproduces the experiment, each `.seed` reproduces a fixture; "one scalar seed reproduces everything" no longer holds. Mitigation: accepted deliberately — the realised bytes are transported, so day-to-day reproduction is by storage, and `.seed` only regenerates a fixture from scratch.
- **New `dqrng` / `task-primer` dependency at construction** → `scenario-input-types` now depends on `local-dqrng-state` and `task-primer`, edges the original `ssd-define-scenario` deliberately avoided. Mitigation: scoped use only, no global perturbation; reflected in the §12 DAG (`primer → scenario-input-types`).
- **Inline transport bloats a serialised scenario if datasets grow** → contrary to the tiny-dataset assumption. Mitigation: the name-only / `registry` regeneration path (`dataset-provenance`, §12) is the documented escape hatch.

## Migration Plan

Additive and backward-compatible: existing data-frame calls behave identically (a tibble element is unchanged). No deprecations. The `registry` / `dataset-provenance` steps later add Parquet persistence and name-only regeneration; until then a generated dataset rides on the scenario as a realised tibble, which is a coherent working state.

## Open Questions

Resolved during exploration:

- *Eager vs. deferred materialisation* → eager and inline (datasets are tiny; transport the bytes).
- *Generation seed source* → a dedicated `ssd_data(.seed)`, decoupled from `scenario$seed`, with the dataset name as the dqrng stream.
- *Provenance recording* → deferred to `dataset-provenance` (§12); the generator code is the provenance for now, and execution-environment tracking is out of scope.

No blocking questions remain. Revisit inline transport vs. name-only regeneration only if a real generator produces a dataset large enough to make scenario serialisation heavy.
