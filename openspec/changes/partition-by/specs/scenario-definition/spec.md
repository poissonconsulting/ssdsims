## ADDED Requirements

### Requirement: Configurable, validated partition_by knob
`ssd_define_scenario()` SHALL accept an optional `partition_by` argument that, when supplied, is a named list with `sample`, `data`, `fit`, and `hc` entries — one per task step (`task-list-loop-baseline`, #80). Each entry SHALL be a character vector of **path-axis** names that is unique, free of missing values, and a subset of that step's axis vocabulary (the step's `task_axes()` from #80). The constructor SHALL abort, in the context of the user-facing function, with an informative error naming the offending step and axis when these conditions are not met. When `partition_by` is `NULL`, the documented per-step defaults SHALL be used.

#### Scenario: Defaults populated when absent
- **WHEN** `ssd_define_scenario()` is called without `partition_by`
- **THEN** the object's `partition_by` SHALL be the documented defaults: `sample = c("dataset", "sim", "replace")`, `data = c("dataset", "sim", "replace", "nrow")`, `fit = c("dataset", "sim", "rescale")`, `hc = c("dataset", "sim")`

#### Scenario: Valid override accepted
- **WHEN** `ssd_define_scenario(..., partition_by = list(sample = c("dataset", "sim", "replace"), data = c("dataset", "sim", "replace", "nrow"), fit = c("dataset", "sim"), hc = c("dataset", "sim", "rescale")))` is called
- **THEN** the object SHALL store that `partition_by` verbatim with no error

#### Scenario: Missing required step entry rejected
- **WHEN** `partition_by` is supplied as a list lacking one of the `sample`, `data`, `fit`, or `hc` entries
- **THEN** the constructor SHALL abort with an informative error naming the missing step

#### Scenario: Unknown axis rejected
- **WHEN** a step's `partition_by` entry names an axis outside that step's vocabulary (e.g. `nboot` under `sample`, or a typo)
- **THEN** the constructor SHALL abort with an informative error naming the offending step and axis

#### Scenario: nrow rejected only for the sample step
- **WHEN** the `sample` step's `partition_by` entry includes `"nrow"`
- **THEN** the constructor SHALL abort with an informative error stating that the `sample` step is the shared draw and carries no `nrow` axis (every `nrow` truncates the same draw at the `data` step, §5)

#### Scenario: nrow accepted as a path axis for data/fit/hc
- **WHEN** the `data` (or `fit`/`hc`) step's `partition_by` entry includes `"nrow"`
- **THEN** the constructor SHALL accept it, because `nrow` is a genuine cross-join axis of the RNG-free truncation step (#80)

#### Scenario: Duplicate or missing axis names rejected
- **WHEN** a step's `partition_by` entry contains duplicate names or `NA`
- **THEN** the constructor SHALL abort with an informative error

### Requirement: Path-axis vs inner-axis split
The package SHALL define, for each step, the inner (Parquet-column) axes as that step's axis vocabulary (`task_axes()`, #80) minus its `partition_by` path axes, and SHALL expose this split to downstream task-table and shard construction. The path axes determine the shard count for a step (`Π |path axis|`) and the `<step>_id` Hive path key (#80's `path_key()` over the chosen path axes); the inner axes are carried as columns within each shard.

#### Scenario: Inner axes are the complement of path axes
- **WHEN** the fit step's vocabulary is queried for a scenario whose `fit` path axes are `c("dataset", "sim", "rescale")`
- **THEN** the inner axes SHALL be the remaining fit axes (`nrow`, `computable`, `at_boundary_ok`, `min_pmix`, `range_shape1`, `range_shape2`)

#### Scenario: All-axes-in-path yields no inner axes
- **WHEN** a step's `partition_by` lists every axis in that step's vocabulary
- **THEN** the inner-axis set for that step SHALL be empty (one task per shard — #80's default `<step>_id` granularity)

#### Scenario: Per-step vocabularies match #80's task_axes()
- **WHEN** the axis vocabulary is queried per step
- **THEN** it SHALL equal `task_axes(step)`: `sample` = `dataset`, `sim`, `replace`; `data` adds `nrow`; `fit` adds `rescale`, `computable`, `at_boundary_ok`, `min_pmix`, `range_shape1`, `range_shape2`; `hc` adds `ci`, `nboot`, `est_method`, `ci_method`, `parametric`

#### Scenario: Child path is consistent with its parent
- **WHEN** a step's path axes are validated against its parent step's path axes (the `<parent>_id` foreign key, #80)
- **THEN** the child's path axes restricted to the parent's vocabulary SHALL be a subset of the parent's path axes, so the foreign-key join to the parent shard is well-defined

### Requirement: partition_by rendered by print method
`print.ssdsims_scenario()` SHALL render the per-step `partition_by` path axes alongside the other declarative fields.

#### Scenario: Print shows partition axes
- **WHEN** an `ssdsims_scenario` is printed
- **THEN** the output SHALL show the `sample`, `data`, `fit`, and `hc` path axes
