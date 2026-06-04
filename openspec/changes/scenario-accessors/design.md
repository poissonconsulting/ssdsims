## Context

`TARGETS-DESIGN.md` §1.1 framed datasets and `min_pmix` as an *implicit registry* resolved per project — datasets persisted to `results/datasets/<name>.parquet`, `min_pmix` pinned to a function value. Two things have since made the registry redundant. First, `scenario-input-types` materialises every dataset **inline on the scenario** at construction (validated via `ssd_data()`, `Conc` enforced), and the scenario is a referenced global in `_targets.R` transported to workers — so a shard reads a dataset straight off the scenario. Second, the same trick works for `min_pmix`: store the resolved function on the scenario keyed by name, hash the **name only**, and the function rides along for execution. The result is that both are *materialised-on-scenario, accessed-by-name*, so the registry collapses into two thin accessors and the change is renamed `registry` → `scenario-accessors`.

## Goals / Non-Goals

**Goals:**

- Materialise `min_pmix` functions on the scenario (keyed by name) at construction, keeping name-only hashing/identity.
- Provide `scenario_dataset(scenario, name)` and `scenario_min_pmix(scenario, name)` accessors that isolate a materialised value by name, erroring on an unknown name.
- Rewire `resolve_min_pmix()` to the accessor; remove the runtime `ssdtools`/global-env lookup.
- Remove the registry concept (registration functions, Parquet persistence, `_index.json`) from the design and roadmap.

**Non-Goals:**

- Persisting datasets/functions to disk — deferred to `dataset-provenance` (only for datasets too large to ride inline).
- The shard Parquet I/O, summary query layer, and `duckplyr` dependency — those belong to `task-tables`.
- Changing the task hash: it still keys on the `min_pmix` *name* (`task_axes("fit")`), never the function value.

## Decisions

### Decision: accessors, not a registry — for `min_pmix` as well as datasets

A registry only earns its keep when a name must be resolved to a value that is *not already to hand*. Here it always is: datasets are materialised inline (`scenario-input-types`), and `min_pmix` can be materialised the same way. So both reduce to `scenario_<thing>(scenario, name)`. *Why this is safe for `min_pmix` specifically:* the §1.1 worry was that **hashing** a function value is not byte-stable (byte-compilation, `srcref`, captured envs). We never hash the value — `task_primer()` hashes the *name*. Storing the function for *execution* has none of that risk, exactly as storing dataset bytes doesn't. So the name→value indirection that the registry provided is unnecessary; the value travels on the scenario.

### Decision: materialise `min_pmix` at construction (resolve name-strings then, not at run time)

`ssd_define_scenario()` already validates `min_pmix` (functions must be single-argument) and derives names. Extend it to also **keep the resolved functions**, keyed by name: a supplied function is stored under its derived name; a name-string (e.g. `"ssd_min_pmix"`, or a user function name) is resolved to a function **at construction** (from `ssdtools` / the caller's environment) and stored, failing fast with an informative error if it cannot be resolved. This mirrors dataset materialisation (realise once, at construction) and means a cluster worker never has to resolve a name against an environment it doesn't share. *Alternative considered:* keep resolving at run time via a registry/`ssdtools` lookup (status quo) — rejected because it re-introduces the worker-environment fragility the materialise-on-scenario approach removes.

### Decision: the scenario stores names *and* functions; hashing/path use names only

The scenario keeps the `min_pmix` **names** where it does today (`$fit$min_pmix`, feeding `task_axes("fit")`, the primer, and the Hive path) and adds the **functions** in a parallel name-keyed store (e.g. `$min_pmix_fns`). The task table's `min_pmix` column stays the name. `scenario_min_pmix(scenario, name)` returns the function. This keeps the identity surface (names) exactly as `task-lists`/`partition-by` already rely on, while making the value reachable. *Trade-off:* the scenario is no longer "names + numeric knobs only" — it now carries closures, as it already carries dataset tibbles. For tiny single-argument `min_pmix` functions this is negligible, and it is the same trade already accepted for inline datasets.

### Decision: `resolve_min_pmix()` becomes the accessor

`resolve_min_pmix()` in `R/task-lists.R` currently searches `ssdtools` then the global env at call time. Replace its body with `scenario_min_pmix(scenario, name)` (the runner already has the scenario). The function keeps its name and call site; only its source of truth changes (the materialised store instead of a live lookup). The baseline runner is observably unchanged for built-in `min_pmix` because the constructor resolves `"ssd_min_pmix"` to the same `ssdtools` function it would have found at run time.

### Decision: drop the registry from the design and rename the node

`registry` ceases to exist as a concept: no `ssd_register_dataset()`/`ssd_register_min_pmix()`, no Parquet dataset cache, no `_index.json`, no pinned-function target. TARGETS-DESIGN.md §1.1's "Implicit registries" section is rewritten to "materialised on the scenario, accessed by name," and the §12 DAG node `reg[registry]` becomes `acc[scenario-accessors]` (edges unchanged: `inputs → acc → tt`). There is no glossary entry for "registry" to retire. *Why rename rather than keep the slug:* the slug names the concept, and the concept is gone; keeping "registry" would mislead.

## Risks / Trade-offs

- **Closures on the scenario** → the scenario serialises a few tiny functions. Mitigation: `min_pmix` functions are single-argument and small; they are never hashed (names are), so byte-instability is irrelevant; this matches inline datasets.
- **Construction-time resolution of name-strings** → a custom `min_pmix` referenced by a bare name must be resolvable in the constructor's environment. Mitigation: resolve eagerly and fail fast with a clear error (better than a deferred failure on a worker); supplying the function directly side-steps it entirely.
- **Touches the scenario constructor** (`scenario-definition`) → a behavioural change beyond a pure accessor. Mitigation: the stored names — the identity surface other capabilities depend on — are unchanged; only an additional function store is added.
- **Renaming an in-flight change** → churn in the change folder and the roadmap. Mitigation: a clean `git mv` plus the §1.1/§12 edits; cheap to do now while it is still a proposal.

## Open Questions

- **Where to store the functions on the scenario** (`$min_pmix_fns` vs. embedding in `$fit`) — an implementation detail settled in apply; the accessor hides it.
- **Should `scenario_dataset()` return the bare tibble or the `ssd_data()` element** — default to the tibble (what a shard body fits); revisit if a caller needs the richer collection.
