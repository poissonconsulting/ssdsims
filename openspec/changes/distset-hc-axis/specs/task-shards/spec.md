## ADDED Requirements

### Requirement: distset is a valid hc partition axis, bundled by default
`"distset"` SHALL be an accepted `hc` axis for both `partition_by` (path) and
`bundle` (inner), since it is a member of `task_axes("hc")`. The documented
default hc path SHALL remain `c("dataset", "sim")`, leaving `distset` as an
**inner** (bundled) hc axis by default: one hc shard then holds every declared
pool for its `(dataset, sim)` cell, so the shard runner decodes the parent union
fit once and serves all pools from it. A user MAY promote `distset` to a path
axis via `partition_by$hc` to obtain one shard per `(…, distset)` cell.

#### Scenario: distset accepted as an hc path axis
- **WHEN** `ssd_define_scenario(..., dists = list(BCANZ = ..., Iwasaki = ...), partition_by = list(hc = c("dataset", "sim", "distset")))` is called
- **THEN** the constructor SHALL accept it and the hc shards SHALL be keyed by `dataset`, `sim`, and `distset` (one shard per pool per `(dataset, sim)`)

#### Scenario: distset is bundled by default
- **WHEN** a distset scenario is constructed without an explicit `hc` `partition_by`
- **THEN** the hc path SHALL be `c("dataset", "sim")` and `distset` SHALL be an inner axis, so one hc shard carries all declared pools for each `(dataset, sim)` cell

### Requirement: The hc shard runner subsets the union fit per distset, decoding each parent fit once
The hc shard runner (`ssd_run_hc_step()`) SHALL read each distinct parent `fit`
shard the shard's tasks reference, decode each union fit **once** per `fit_id`,
and for each hc task subset that decoded fit to the task's `distset` members
before estimating the hazard concentration. Multiple `distset` tasks sharing a
`fit_id` SHALL reuse the one decoded fit (no repeated deserialisation), and each
output row SHALL be tagged with its `hc_id`, parent `fit_id`, and `distset`.

#### Scenario: One decode per fit serves every pool in the shard
- **WHEN** an hc shard bundles several `distset` tasks that share a parent `fit_id`
- **THEN** the runner SHALL decode that union fit once and subset it per `distset`, writing one Parquet for the shard with rows tagged by `hc_id`, `fit_id`, and `distset`

#### Scenario: Adding a distribution set mints only new hc shards
- **WHEN** a scenario gains an additional distribution set (with `distset` on the hc path) and the pipeline is re-run
- **THEN** only the new hc shards SHALL be built; every `sample` and `fit` shard, and every pre-existing hc shard, SHALL be served from cache (the fit layer carries no `distset` axis)
