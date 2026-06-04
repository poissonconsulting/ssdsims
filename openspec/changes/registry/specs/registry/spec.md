## ADDED Requirements

### Requirement: Register a dataset to the registry
The package SHALL provide `ssd_register_dataset(name, data)` that persists a dataset's tibble to `results/datasets/<name>.parquet` and records an entry `name -> { rows, conc_col, sha256, source }` in `results/datasets/_index.json`. Registration SHALL verify the SSD invariant that the data contains a numeric `Conc` column (`TARGETS-DESIGN.md` §1.1) and SHALL abort with an informative error naming the offending dataset otherwise; other columns SHALL pass through unchanged. The `sha256` SHALL be computed over the written Parquet file's bytes.

#### Scenario: A valid dataset is persisted and indexed
- **WHEN** `ssd_register_dataset("boron", df)` is called with a data frame carrying a numeric `Conc` column
- **THEN** `results/datasets/boron.parquet` SHALL be written, and `_index.json` SHALL gain a `"boron"` entry whose `rows` equals `nrow(df)`, whose `conc_col` records the `Conc` column, and whose `sha256` matches the written Parquet file's bytes

#### Scenario: A dataset without a Conc column is rejected
- **WHEN** `ssd_register_dataset("bad", df)` is called with `df` lacking a numeric `Conc` column
- **THEN** the call SHALL abort with an informative error identifying the dataset, and SHALL NOT write a Parquet file or an index entry

#### Scenario: Round-trip preserves the dataset
- **WHEN** a registered dataset is read back from its Parquet file
- **THEN** the returned tibble SHALL equal the registered tibble (including the `Conc` column and any pass-through columns)

### Requirement: Re-registration is refused unless byte-identical
Registering a payload under a name that already exists in the registry SHALL succeed as a no-op when the new payload is byte-identical to the stored one, and SHALL abort with an informative error otherwise (resolving `TARGETS-DESIGN.md` §11.2 in favour of refuse-unless-identical). This applies to both dataset and `min_pmix` registrations.

#### Scenario: Identical re-registration is a no-op
- **WHEN** a dataset is registered twice under the same name with byte-identical content
- **THEN** the second call SHALL succeed without error and SHALL leave the Parquet file and index entry unchanged

#### Scenario: Conflicting re-registration aborts
- **WHEN** a name is registered a second time with content that differs from the stored bytes
- **THEN** the call SHALL abort with an informative error naming the conflicting name, and SHALL NOT overwrite the stored payload

### Requirement: Register a min_pmix function under a name
The package SHALL provide `ssd_register_min_pmix(name, fn)` that pins a single-argument function under a name for per-project resolution. The function SHALL be validated as taking a single argument (the number of rows). Only the name is a task-hash key; the function value is the resolved payload (`TARGETS-DESIGN.md` §1.1).

#### Scenario: A valid min_pmix function is registered
- **WHEN** `ssd_register_min_pmix("strict", function(n) 0.05)` is called
- **THEN** the registry SHALL resolve the name `"strict"` to that function

#### Scenario: A non-conforming min_pmix function is rejected
- **WHEN** `ssd_register_min_pmix("bad", function(a, b) a)` is called with a function that does not take exactly one argument
- **THEN** the call SHALL abort with an informative error

### Requirement: Resolve a scenario's referenced names through the registry
The package SHALL resolve each name referenced by a scenario through the registry, once per project: each referenced dataset name to its persisted Parquet file, and each referenced `min_pmix` name to a pinned function value. Dataset resolution SHALL persist the tibble the scenario already carries inline (`scenario$data[[name]]`) rather than regenerating it (`TARGETS-DESIGN.md` §1.1; name-only regeneration is the deferred `dataset-provenance` step). `min_pmix` resolution SHALL consult the registry first and fall back to the existing `ssdtools` / global-environment lookup when no registry entry exists, so a local run with no registry is unaffected.

#### Scenario: Referenced datasets are persisted on resolution
- **WHEN** the registry resolver is run against a scenario referencing datasets `c("boron", "cadmium")`
- **THEN** `results/datasets/boron.parquet` and `results/datasets/cadmium.parquet` SHALL exist, persisted from the scenario's inline tibbles, with matching `_index.json` entries

#### Scenario: A min_pmix name resolves to its registered function
- **WHEN** a `min_pmix` name is registered and then resolved
- **THEN** resolution SHALL return the registered function value

#### Scenario: Resolution falls back when no registry entry exists
- **WHEN** a `min_pmix` name is resolved with no matching registry entry but a matching `ssdtools` function
- **THEN** resolution SHALL return the `ssdtools` function (the existing baseline-runner behaviour), without error

### Requirement: Function-body edits do not move task hashes
Because `min_pmix` is referenced by name and the per-task primer hashes the name (not the function value), re-registering a name with a different function body SHALL NOT change the `(seed, primer)` of any task that references it (`TARGETS-DESIGN.md` §1.1).

#### Scenario: A registered min_pmix body does not affect the fit hash
- **WHEN** a fit task's primer is computed for a scenario referencing `min_pmix = "default"` with the registry empty, a function is then registered under `"default"`, and the primer is recomputed
- **THEN** the two primers SHALL be byte-identical, because the primer hashes the name string and never the resolved function value
