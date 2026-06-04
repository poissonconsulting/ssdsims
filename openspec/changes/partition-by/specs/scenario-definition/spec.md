## ADDED Requirements

### Requirement: Configurable, validated partition_by knob with a complementary bundle entry point
`ssd_define_scenario()` SHALL accept an optional `partition_by` argument and a complementary optional `bundle` argument. Each, when supplied, is a named list keyed by task step (`sample`/`fit`/`hc`; `task-list-loop-baseline`, #80, with the `data` truncation folded into `fit`) and MAY be partial (cover a subset of steps). A `partition_by` entry SHALL be a character vector of **path-axis** names; a `bundle` entry SHALL be a character vector of **inner-axis** names; in both cases the vector SHALL be unique, free of missing values, and a subset of that step's vocabulary (`task_axes(step)`). For each step **at most one** of `partition_by`/`bundle` SHALL name it; a step named in **both** SHALL abort. A step named in **neither** SHALL use its documented default. The constructor SHALL normalize the two into a single stored `partition_by` — a complete `sample`/`fit`/`hc` **path** list: a `bundle` entry becomes its path complement `setdiff(task_axes(step), bundle[[step]])`, and defaults fill unnamed steps. The constructor SHALL abort, in the context of the user-facing function, with an informative error naming the offending step and axis when any condition is violated.

#### Scenario: Defaults populated when absent
- **WHEN** `ssd_define_scenario()` is called without `partition_by`
- **THEN** the object's `partition_by` SHALL be the documented defaults: `sample = c("dataset", "sim", "replace")`, `fit = c("dataset", "sim", "nrow", "rescale")`, `hc = c("dataset", "sim")`

#### Scenario: Valid override accepted
- **WHEN** `ssd_define_scenario(..., partition_by = list(sample = c("dataset", "sim", "replace"), fit = c("dataset", "sim", "nrow"), hc = c("dataset", "sim", "rescale")))` is called
- **THEN** the object SHALL store that `partition_by` verbatim with no error

#### Scenario: Partial spec defaults the unnamed steps
- **WHEN** `partition_by` (or `bundle`) is supplied covering only some of the `sample`/`fit`/`hc` steps
- **THEN** the steps named in neither argument SHALL use their documented defaults, and the stored `partition_by` SHALL be the complete three-step path list

#### Scenario: bundle accepted as the inner-axis complement
- **WHEN** `ssd_define_scenario(..., bundle = list(fit = c("computable", "at_boundary_ok", "min_pmix", "range_shape1", "range_shape2")))` is called
- **THEN** the stored `fit` `partition_by` SHALL be `setdiff(task_axes("fit"), bundle$fit)` = `c("dataset", "sim", "replace", "nrow", "rescale")`

#### Scenario: partition_by and bundle mixed across steps
- **WHEN** `partition_by` names the `sample` step and `bundle` names the `fit` step (with `hc` omitted)
- **THEN** the constructor SHALL accept it: `sample` uses the given path axes, `fit` uses the `bundle` complement, and `hc` uses its default

#### Scenario: A step named in both partition_by and bundle is rejected
- **WHEN** the same step (e.g. `fit`) appears in both the `partition_by` and the `bundle` argument
- **THEN** the constructor SHALL abort with an informative error naming that step

#### Scenario: Unknown axis rejected
- **WHEN** a step's `partition_by` entry names an axis outside that step's vocabulary (e.g. `nboot` under `sample`, or a typo)
- **THEN** the constructor SHALL abort with an informative error naming the offending step and axis

#### Scenario: nrow rejected only for the sample step
- **WHEN** the `sample` step's `partition_by` entry includes `"nrow"`
- **THEN** the constructor SHALL abort with an informative error stating that the `sample` step is the shared draw and carries no `nrow` axis (every `nrow` truncates the same draw inside the `fit` step, §5)

#### Scenario: nrow accepted as a path axis for fit/hc
- **WHEN** the `fit` (or `hc`) step's `partition_by` entry includes `"nrow"`
- **THEN** the constructor SHALL accept it, because the `fit` step truncates its parent sample (`head`, RNG-free) so `nrow` is a genuine `fit` cross-join axis (#80, data folded into fit)

#### Scenario: Duplicate or missing axis names rejected
- **WHEN** a step's `partition_by` entry contains duplicate names or `NA`
- **THEN** the constructor SHALL abort with an informative error

### Requirement: Path-axis vs inner-axis split
The package SHALL define, for each step, the inner (Parquet-column) axes as that step's axis vocabulary (`task_axes()`, #80) minus its `partition_by` path axes, and SHALL expose this split to downstream task-table and shard construction. The path axes determine the shard count for a step (`Π |path axis|`) and the **Hive shard path** (`path_key()` over the chosen path axes — distinct from the `<step>_id` task-identity key, which #80 keys over *all* axes and which `partition_by` does not change); the inner axes are carried as columns within each shard.

#### Scenario: Inner axes are the complement of path axes
- **WHEN** the fit step's vocabulary is queried for a scenario whose `fit` path axes are `c("dataset", "sim", "nrow", "rescale")`
- **THEN** the inner axes SHALL be the remaining fit axes (`replace`, `computable`, `at_boundary_ok`, `min_pmix`, `range_shape1`, `range_shape2`)

#### Scenario: All-axes-in-path yields no inner axes
- **WHEN** a step's `partition_by` lists every axis in that step's vocabulary
- **THEN** the inner-axis set for that step SHALL be empty (one task per shard — the shard path then equals the `<step>_id` task identity, the only case where they coincide)

#### Scenario: Per-step vocabularies match #80's task_axes()
- **WHEN** the axis vocabulary is queried per step
- **THEN** it SHALL equal `task_axes(step)`: `sample` = `dataset`, `sim`, `replace`; `fit` adds `nrow`, `rescale`, `computable`, `at_boundary_ok`, `min_pmix`, `range_shape1`, `range_shape2`; `hc` adds `ci`, `nboot`, `est_method`, `ci_method`, `parametric`

#### Scenario: Steps partition independently — no cross-step constraint
- **WHEN** a step's path axes are a valid subset of its own vocabulary but differ arbitrarily from its parent step's path axes (e.g. finer or coarser on a shared axis)
- **THEN** the constructor SHALL accept them without any parent-consistency check, since a child shard may span several parent shards (an m:n relationship resolved at the read layer, not by restricting `partition_by`); the `<parent>_id` foreign key remains well-defined regardless

### Requirement: partition_by and bundle rendered by print method
`print.ssdsims_scenario()` SHALL render, per step, **both** the `partition_by` (across-shards) path axes and the `bundle` (within-shard) inner axes, regardless of which was supplied, alongside the other declarative fields.

#### Scenario: Print shows both path and bundle axes
- **WHEN** an `ssdsims_scenario` is printed
- **THEN** the output SHALL show, for the `sample`, `fit`, and `hc` steps, both the path axes (`partition_by`) and the inner axes (`bundle`)
