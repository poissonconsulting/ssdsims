## ADDED Requirements

### Requirement: User-facing validation errors report the calling function as origin
Every exported user-facing function SHALL raise its validation errors in the
context of that function. This covers the `ssd_*()` and `scenario_*()`
functions and the `with_*` / `local_*` RNG helpers, so the error condition's `call`
(and therefore the `Error in \`...\`:` header) names the function the user
called. A validation error MUST NOT report an internal frame — a private helper,
a `purrr::map()` / `purrr::walk()` / `lapply()` body, or `chk::chk_all()` — as
its origin. To achieve this, an exported function SHALL capture its own frame
(e.g. `call <- environment()`) and thread it into the validators it invokes,
both as the `call =` argument of `chk::abort_chk()` and as the `call =` argument
of any private validator it delegates to; element-wise validation that would
otherwise surface a wrapper frame SHALL use a plain loop rather than
`chk::chk_all()` / `purrr::walk()`. This contract changes only the reported
origin of an error; it SHALL NOT change which inputs a function accepts or
rejects.

#### Scenario: A list-argument failure names the public function, not chk_all
- **WHEN** `ssd_fit_dists_sims()` is called with an invalid `min_pmix` (e.g. a list element that is not a function) that was previously validated via `chk::chk_all()`
- **THEN** the raised condition's `call` / `Error in \`...\`:` header SHALL name `ssd_fit_dists_sims()` and SHALL NOT name `chk_all`, `purrr`, or any internal helper

#### Scenario: A delegated validation failure names the public function, not the helper
- **WHEN** an exported function (e.g. `ssd_scenario_fit_shards()`) is called with an invalid argument whose validation is delegated to a private helper
- **THEN** the raised condition's `call` / header SHALL name the exported function the user called, not the private helper that performed the check

#### Scenario: The contract holds across the exported surface
- **WHEN** any exported `ssd_*()` / `scenario_*()` function is called with an invalid argument
- **THEN** the raised condition's `call` / header SHALL name that exported function and SHALL NOT name an internal frame (a private helper, a `purrr::map()` / `purrr::walk()` / `lapply()` body, or `chk::chk_all()`)

#### Scenario: Fixing the origin does not change what is accepted or rejected
- **WHEN** the same inputs are passed to an audited function before and after the origin is threaded
- **THEN** the set of inputs that pass and fail validation SHALL be unchanged; only the origin reported in the error condition SHALL differ
