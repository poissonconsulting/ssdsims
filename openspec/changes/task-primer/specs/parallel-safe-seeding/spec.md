## ADDED Requirements

### Requirement: Task primer derivation from a task-parameter hash
The package SHALL expose `task_primer(params)` returning a length-2 integer vector — the per-task **primer** — derived from `rlang::hash(params)`. The primer SHALL pack 64 bits of the hash as `c(hi32, lo32)` suitable for dqrng's `stream` argument, so that `dqrng::dqset.seed(seed, stream = task_primer(params))` fully specifies a task's RNG starting point together with the scenario `seed`.

#### Scenario: Primer is a length-2 integer vector
- **WHEN** `task_primer(list(dataset = "boron", sim = 1L, replace = FALSE))` is called
- **THEN** the result SHALL be an integer vector of length 2

#### Scenario: Deterministic and reproducible
- **WHEN** `task_primer(p)` is called twice with identical `p`
- **THEN** the two results SHALL be `identical()`

#### Scenario: Sensitive to task parameters
- **WHEN** `task_primer(p1)` and `task_primer(p2)` are called with `p1` and `p2` differing in any parameter (e.g. `rescale = FALSE` vs `rescale = TRUE`)
- **THEN** the two primers SHALL differ

#### Scenario: Primer seeds dqrng reproducibly
- **WHEN** `dqrng::dqset.seed(seed, stream = task_primer(p))` is set twice with the same `seed` and `p`, drawing the same number of values each time
- **THEN** the two draw sequences SHALL be identical; a different `p` (or `seed`) SHALL yield a different sequence

### Requirement: 64-bit primer encoding with NA/INT_MIN mapping
`task_primer()` SHALL encode each 32-bit half of the hash slice as a signed int32, mapping the reserved bit pattern `0x80000000` (INT_MIN, which R cannot represent as a non-NA integer) to `NA_integer_`. dqrng accepts `NA_integer_` in `stream` and treats it as INT_MIN, so the encoding SHALL recover the full 64 bits of stream entropy.

#### Scenario: INT_MIN bit pattern encoded as NA
- **WHEN** a hash slice produces the 32-bit value `0x80000000`
- **THEN** the corresponding primer element SHALL be `NA_integer_`

#### Scenario: Other values encoded as signed int32
- **WHEN** a hash slice produces a 32-bit value other than `0x80000000`
- **THEN** the corresponding primer element SHALL be that value as a (possibly negative) non-`NA` int32

#### Scenario: Collision-resistant over many tasks
- **WHEN** `task_primer()` is applied to the distinct task-parameter sets in `scripts/experiment-dqrng-hash.R`
- **THEN** distinct task parameters SHALL yield distinct primers (no empirical collisions over the validated example set)

### Requirement: Canonical name-keyed hash input
`task_primer()` SHALL hash a canonical, name-keyed representation of the task's parameters. Function-valued parameters (e.g. `min_pmix`) SHALL be referenced **by name**, not by function value, so that a recompile or JIT does not change a task's primer. `nrow` SHALL NOT contribute to a data task's hash, because every `nrow` value is sub-truncation of the same `n_max`-row sample (§5).

#### Scenario: min_pmix referenced by name
- **WHEN** two fit-task parameter sets differ only in the function *value* bound to a `min_pmix` name (e.g. a recompiled copy) but share the same name
- **THEN** their primers SHALL be identical

#### Scenario: nrow excluded from the data-task hash
- **WHEN** two data-task parameter sets share `(dataset, sim, replace)` but differ in `nrow`
- **THEN** their primers SHALL be identical
