## ADDED Requirements

### Requirement: Configurable, validated partition_by knob
`ssd_define_scenario()` SHALL accept an optional `partition_by` argument that, when supplied, is a named list with `data`, `fit`, and `hc` entries. Each entry SHALL be a character vector of **path-axis** names that is unique, free of missing values, and a subset of that step's known axis vocabulary. The constructor SHALL abort, in the context of the user-facing function, with an informative error naming the offending step and axis when these conditions are not met. When `partition_by` is `NULL`, the documented per-step defaults (§5) SHALL be used.

#### Scenario: Defaults populated when absent
- **WHEN** `ssd_define_scenario()` is called without `partition_by`
- **THEN** the object's `partition_by` SHALL be the documented defaults: `data = c("dataset", "sim", "replace")`, `fit = c("dataset", "sim", "rescale")`, `hc = c("dataset", "sim")`

#### Scenario: Valid override accepted
- **WHEN** `ssd_define_scenario(..., partition_by = list(data = c("dataset", "sim", "replace"), fit = c("dataset", "sim"), hc = c("dataset", "sim", "rescale")))` is called
- **THEN** the object SHALL store that `partition_by` verbatim with no error

#### Scenario: Missing required step entry rejected
- **WHEN** `partition_by` is supplied as a list lacking one of the `data`, `fit`, or `hc` entries
- **THEN** the constructor SHALL abort with an informative error naming the missing step

#### Scenario: Unknown axis rejected
- **WHEN** a step's `partition_by` entry names an axis outside that step's vocabulary (e.g. `nboot` under `data`, or a typo)
- **THEN** the constructor SHALL abort with an informative error naming the offending step and axis

#### Scenario: nrow rejected as a path axis
- **WHEN** any step's `partition_by` entry includes `"nrow"`
- **THEN** the constructor SHALL abort with an informative error stating that `nrow` is a sub-truncation column, never a partition axis

#### Scenario: Duplicate or missing axis names rejected
- **WHEN** a step's `partition_by` entry contains duplicate names or `NA`
- **THEN** the constructor SHALL abort with an informative error

### Requirement: Path-axis vs inner-axis split
The package SHALL define, for each step, the inner (Parquet-column) axes as that step's axis vocabulary minus its `partition_by` path axes, and SHALL expose this split to downstream task-table and shard construction. The path axes determine the shard count for a step (`Π |path axis|`); the inner axes are carried as columns within each shard.

#### Scenario: Inner axes are the complement of path axes
- **WHEN** the fit step's vocabulary is queried for a scenario whose `fit` path axes are `c("dataset", "sim", "rescale")`
- **THEN** the inner axes SHALL be the remaining fit-grid axes (`computable`, `at_boundary_ok`, `min_pmix`, `range_shape1`, `range_shape2`)

#### Scenario: All-axes-in-path yields no inner axes
- **WHEN** a step's `partition_by` lists every axis in that step's vocabulary
- **THEN** the inner-axis set for that step SHALL be empty (one task per shard)

#### Scenario: Per-step vocabularies are step-specific
- **WHEN** the axis vocabulary is queried per step
- **THEN** `data` SHALL admit `dataset`, `sim`, `replace`; `fit` SHALL additionally admit `rescale`, `computable`, `at_boundary_ok`, `min_pmix`, `range_shape1`, `range_shape2`; and `hc` SHALL additionally admit `nboot`, `est_method`, `ci_method`, `parametric`

### Requirement: partition_by rendered by print method
`print.ssdsims_scenario()` SHALL render the per-step `partition_by` path axes alongside the other declarative fields.

#### Scenario: Print shows partition axes
- **WHEN** an `ssdsims_scenario` is printed
- **THEN** the output SHALL show the `data`, `fit`, and `hc` path axes
