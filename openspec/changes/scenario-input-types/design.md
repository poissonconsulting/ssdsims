## Context

`ssd-define-scenario` landed a declarative `ssdsims_scenario` and an `ssd_data()` collector that are **data-frame-only**. `ssd_run_scenario()`, by contrast, dispatches over five `ssd_sim_data()` S3 methods — `data.frame`, `fitdists`, `tmbfit`, `function`, and `character` (a function-name string) — where the last four are *data generators*. This change widens the declarative path to accept those four generator types.

The plan was reworked during exploration (`exploration/reprexes-and-api.md`, `exploration/ssd-gen-reprex.md`). The earlier draft put a `.seed` argument and eager generator materialisation **inside `ssd_data()` / the constructor**. Two empirical findings retired that shape:

1. **`ssd_sim_data()` cannot back a seedable generation contract.** Its generator methods seed with L'Ecuyer-CMRG off the *sim index* and **ignore their `seed=` argument** (verified: `seed = 42` ≡ `seed = 999`). Routed under the dqrng backend, the L'Ecuyer `RNGkind` toggle even moves the dqrng state as a *side effect* (not from `.seed`), so "different `.seed` → different data" fails and the dqrng-only check misfires. Only a **direct draw under dqrng** satisfies the contract (validated in the reprex).
2. **A materialised generator is a fixture the scenario *resamples*, indistinguishable from a data frame.** The `sample` step draws `slice_sample(data, n = max(nrow), replace)` from the stored tibble, and this change stores **no** generator descriptor (provenance deferred to `dataset-provenance`). So accepting a generator *function* buys no downstream capability over a data frame; the value it adds is *reproducibly producing the fixture* — a **generation** concern, separable from scenario definition.

So the design splits into **two helpers**: `ssd_scenario_data()` (assembly/validation) and `ssd_gen()` (generation). `ssd_run_scenario()` / `ssd_sim_data()` are untouched.

## Goals / Non-Goals

**Goals:**

- Accept the same generator set as `ssd_run_scenario()` — `fitdists`, `tmbfit`, function, function-name string — through a dedicated `ssd_gen()`, materialised once to validated `Conc` tibbles.
- Seed generation reproducibly and **independently of the scenario**, with the dataset **name** as the dqrng stream and a **required** `.seed`, so reproducibility is hard to get wrong.
- Keep `ssd_define_scenario()` RNG-free and inert (generation happens before it).
- Rename `ssd_data()` → `ssd_scenario_data()` to escape the `ssdtools::ssd_data(x)` clash, and route all scenario dataset input through it.
- Reuse `task-rng-postcheck` for the dqrng gate (`dqrng_usable()`) and the per-draw witness (`chk_dqrng_backend_intact()`).

**Non-Goals:**

- Persisting datasets to Parquet, or a name-only registry that *regenerates* them — deferred to `registry` / `dataset-provenance` (§1.1). Here the realised tibble rides on the scenario.
- "Fresh data per sim" (the legacy `ssd_run_scenario(gen, …)` semantics). A materialised generator is a single fixture that the scenario resamples; reproducing per-sim regeneration is a separate, deferred concern.
- Per-task simulation RNG (§2). The scenario's `seed` governs `sample`/`fit`/`hc`; the generation `.seed` is separate and plays **no** part in the task primers.
- Recording generator provenance (function, `.seed`, args) as scenario metadata — deferred (`dataset-provenance`). For now the **generator code is the provenance** and the realised bytes are transported.
- Changing the `fit`/`hc` grids, `partition_by`, the `ci = FALSE` rule, or the legacy dispatch.

## Decisions

### Decision: two helpers — `ssd_scenario_data()` (assemble) and `ssd_gen()` (generate)

`ssd_scenario_data()` is today's `ssd_data()`, renamed: it assembles one or more **data frames** (and the results of `ssd_gen()`) into a validated, named `ssdsims_data` collection of `Conc` tibbles. `ssd_gen(..., .n, .seed)` accepts **only** generator-style inputs and returns a classed `ssdsims_gen` collection of materialised `Conc` tibbles. Intended composition:

```r
ssd_scenario_data(boron = ccme_boron, ssd_gen(synth = ssd_rlnorm, .n = 30, .seed = 42))
ssd_scenario_data(boron = ccme_boron, !!!ssd_gen(synth = ssd_rlnorm, .n = 30, .seed = 42))
```

*Why two helpers rather than generators inside `ssd_data()`/the constructor?* It removes every "surprise" of the eager-in-constructor draft: no RNG or user code in the constructor; the materialise-once-then-resample semantics are stated by a visibly separate generation step rather than borrowing `ssd_run_scenario`'s single-call shape with different meaning; and a **required** `.seed`/`.n` on `ssd_gen()` makes irreproducible/unsized generation impossible. *Why a classed `ssdsims_gen` return?* So an unnamed `ssd_gen(...)` argument can be detected (`inherits()`) and flattened into the collection, while `!!!ssd_gen(...)` also works as a plain named-list splice — both call forms, the latter with no magic.

### Decision: rename `ssd_data()` → `ssd_scenario_data()`

`ssdtools::ssd_data(x)` is an unrelated single-argument export that masks `ssdsims::ssd_data()` whenever `ssdtools` is attached (it broke the first reprex). `ssd_scenario_data()` is unambiguous, free of `ssdtools`/`ssddata` exports (checked), and pairs with `ssd_define_scenario()`/`scenario_dataset()`/`$data`. The collection's S3 class stays `ssdsims_data`. *Alternative considered:* `ssd_datasets()` — rejected for visual proximity to a hypothetical `ssddata::ssd_data_sets()` and the overloaded "data".

### Decision: `ssd_gen(..., .n, .seed)` — required, dot-prefixed `.n` and `.seed`; the name is the dqrng stream

`ssd_gen()` takes generators as named `...` arguments, so a bare `seed=`/`n=` would be a generator named `"seed"`/`"n"`; the dot-prefix makes them formals (never absorbed into `...`) that R will not partial-match a `seed=`/`n=` onto. Both are **required**:

- `.seed` (base seed) — required so generation is always reproducible (closes the "forgot to seed" and "inert `seed=`" footguns the reprex exposed).
- `.n` (rows generated) — required because there is no universal population size, and a silently-too-small population makes `replace = FALSE` resampling degenerate. It maps to the generator's own first argument `n`. Distinct from the scenario's `nrow` (the resample/truncation sizes), so **not** named `.nrow`.

Each generator is materialised under a scoped

```r
local_dqrng_state(.seed, task_primer(list(dataset = "<name>")))
```

so the dataset **name is the dqrng `stream`** and `.seed` is the base seed. One `.seed` therefore fans out across every named generator on its own independent stream (`hash({dataset})` differs per name). One `.n` per call (the common case); differing sizes use multiple `ssd_gen()` calls spliced together. The scoped run restores the dqrng state and leaves global `.Random.seed` unchanged on return.

### Decision: dqrng-backed generation, reusing `task-rng-postcheck`

`ssd_gen()` draws under an active `local_dqrng_backend()` (pcg64). It depends on `task-rng-postcheck`:

- **`dqrng_usable()`** gates every dqrng touch — `ssd_gen()` aborts with actionable `library(dqrng)` guidance when dqrng is not already loaded, never loading a user-RNG provider implicitly (consistent with `task-rng-postcheck` moving `dqrng` to `Suggests`).
- **`chk_dqrng_backend_intact()`** is the post-hoc witness after each generator draw: it verifies the draw came from dqrng. With `.seed` required, the outcomes collapse to two — *drew from dqrng* (reproducible) or *pure / no draw* (reproducible) → ok; *escaped dqrng* (e.g. switched `RNGkind` and drew base R) → abort. This replaces the bespoke three-way base-R/dqrng state check of the earlier draft with `task-rng-postcheck`'s shared, tested helper.

*Residual blind spot (accepted):* a generator reaching a C-level RNG with no R-visible state escapes detection — acceptable under the pure/no-side-effects contract.

### Decision: route on input type in one place, reuse name derivation, validate in the user frame

`ssd_gen()` branches on input type at a single junction (most-specific class first — `tmbfit` before `fitdists`, matching the S3 methods) and resolves each input to a single-argument draw `fn(n)`:

- **function** → `fn`.
- **function-name string** → resolve a **bare name** via `get0()`/`match.fun()` in the caller environment then `ssdtools` (no `eval(parse())`); the string is also the dataset name; abort if it does not resolve to a function.
- **tmbfit** → `ssdtools::estimates()` + the matching `ssd_r<dist>` via `do.call`.
- **fitdists** → pick the top-weighted dist (`glance(wt = TRUE)` → `which.max`), then the `tmbfit` path.
- **data.frame** → rejected (it belongs in `ssd_scenario_data()`).

Name derivation reuses the existing machinery unchanged: argument name → `expr_to_name()` (symbol / `::` call) → (for a string) the string itself; abort when no name is derivable. A generator function is checked structurally (is a function) before execution. Validation aborts with `chk::abort_chk(..., call = call)` in the exported frame (error-call-origin rule).

### Decision: `ssd_define_scenario()` accepts only an `ssd_scenario_data()` collection

The `data` argument must be an `ssdsims_data` object; the bare-data-frame, bare-list, and `name=` forms are dropped. Naming/validation live in `ssd_scenario_data()`/`ssd_gen()`, so `scenario_datasets()` collapses to "assert `ssdsims_data`, return it" — no `data_expr` capture, no list routing. Because generation is done by `ssd_gen()` *before* construction, the constructor draws no RNG: the landed "No side effects on RNG state" requirement is **preserved**.

## Risks / Trade-offs

- **Breaking rename + dropped convenience forms** → `ssd_data()` becomes `ssd_scenario_data()` and `ssd_define_scenario(ccme_boron, …)` becomes `ssd_define_scenario(ssd_scenario_data(ccme_boron), …)`; `name=` is gone. Mitigation: pre-1.0 package (`0.0.0.9xxx`); examples/tests/vignette/README updated in this change; the single-collection input is simpler to reason about.
- **Dependency on `task-rng-postcheck`** → `ssd_gen()` builds on its `dqrng_usable()`/`chk_dqrng_backend_intact()` and the dqrng-as-`Suggests` move. `task-rng-postcheck` is booked under Done in `ROADMAP.md` but its code is **not yet in the tree** (helpers absent, dqrng still in `Imports`), so §3 of this change must land after it. Recorded as the `task-rng-postcheck → scenario-input-types` dependency.
- **`ssd_gen()` runs user code and draws RNG** → it can be slow or throw for a buggy/expensive generator. Mitigation: datasets are tiny by assumption; the scoped backend restores global RNG; failures abort in the user frame naming the offending dataset; this stays out of the constructor.
- **Materialise-once ≠ legacy per-sim** → a generator becomes one fixture the scenario resamples, not fresh data per sim. Mitigation: deliberate and documented; the helper split makes the fixture step explicit; per-sim regeneration is a separate deferred concern.
- **Two-seed reproducibility story** → `scenario$seed` reproduces the experiment, each `ssd_gen(.seed)` reproduces a fixture. Mitigation: accepted; the realised bytes are transported, so day-to-day reproduction is by storage and `.seed` only regenerates a fixture from scratch.
- **Inline transport bloats a serialised scenario if datasets grow** → contrary to the tiny-dataset assumption. Mitigation: the name-only / `registry` regeneration path (`dataset-provenance`) is the documented escape hatch.

## Migration Plan

- Rename `ssd_data()` → `ssd_scenario_data()` across `R/`, `man/`, `NAMESPACE`, `_pkgdown.yml`, tests, vignette, README.
- Add `ssd_gen()` and the `ssdsims_gen` class; teach `ssd_scenario_data()` to flatten an `ssdsims_gen` argument and to accept `!!!` splices.
- Change `ssd_define_scenario()` to require an `ssd_scenario_data()` collection and drop `name=`; simplify `scenario_datasets()`.
- Existing data-frame scenarios migrate by wrapping input in `ssd_scenario_data(...)`. Generator scenarios are new. The `registry`/`dataset-provenance` steps later add persistence and name-only regeneration.

## Open Questions

Resolved during exploration:

- *Eager-in-constructor vs. a generation helper* → a dedicated `ssd_gen()`, separate from `ssd_scenario_data()` and from the constructor.
- *Generation seed source* → a required `ssd_gen(.seed)`, decoupled from `scenario$seed`, with the dataset name as the dqrng stream; row count is a required `.n`.
- *dqrng-only enforcement* → reuse `task-rng-postcheck`'s `dqrng_usable()` + `chk_dqrng_backend_intact()`.
- *Provenance recording* → deferred to `dataset-provenance`; the generator code is the provenance for now, and execution-environment tracking is out of scope.
- *Naming* → `ssd_scenario_data()` (collection) + `ssd_gen()` (generators).

No blocking questions remain. Revisit inline transport vs. name-only regeneration only if a real generator produces a dataset large enough to make scenario serialisation heavy (`dataset-provenance`).
