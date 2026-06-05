## ADDED Requirements

### Requirement: User-facing validation errors report the calling function as origin
This contract SHALL apply only to **anticipated, legitimate user errors**
(validation of user input), not to **unexpected errors** (internal or
programming failures the package does not anticipate).

For an *anticipated user error*, every exported user-facing function SHALL raise
the validation error in the context of that function. This covers the `ssd_*()`
and `scenario_*()` functions and the `with_*` / `local_*` RNG helpers, so the
error condition's `call` (and therefore the `Error in \`...\`:` header) names the
function the user called. Such an error MUST NOT report an internal frame — a
private helper, a `purrr::map()` / `purrr::walk()` / `lapply()` body, or
`chk::chk_all()` — as its origin. To achieve this, an exported function SHALL
capture its own frame (e.g. `call <- environment()`) and thread it into the
validators it invokes, both as the `call =` argument of `chk::abort_chk()` and as
the `call =` argument of any private validator it delegates to. Element-wise
validation that would otherwise surface a wrapper frame SHALL keep its functional
(`purrr::map()` / `walk()`) wrapped in `rlang::try_fetch()`, re-raising the caught
condition with the public frame (`chk::abort_chk(..., call = call)` or
`rlang::abort(..., parent = cnd, call = call)`), rather than emitting the raw
`chk::chk_all()` frame. This contract changes only the reported origin of an
error; it SHALL NOT change which inputs a function accepts or rejects.

For an *unexpected error*, the contract imposes no origin requirement: ordinary
functionals or plain looping primitives MAY be used without `rlang::try_fetch()`,
and an unexpected error surfacing an internal frame is acceptable (it signals a
bug to fix, not a user-facing validation message).

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

#### Scenario: Anticipated element-wise validation keeps a functional via try_fetch
- **WHEN** an exported function validates a user-supplied collection element-wise (e.g. `ssd_fit_dists_sims()` checking each `min_pmix` / `range_shape*` element)
- **THEN** the check SHALL keep its `purrr::map()` / `walk()` functional wrapped in `rlang::try_fetch()` and re-raise with the public `call`, naming the public function as origin (not a hand-rolled `for` loop, and not the raw `chk::chk_all()` frame)

#### Scenario: Unexpected errors are not constrained to the public origin
- **WHEN** iteration that is not user-input validation hits a condition the package does not anticipate (an internal/programming failure)
- **THEN** the contract SHALL NOT require wrapping it in `rlang::try_fetch()` or reporting the public function; a plain looping primitive or ordinary functional is acceptable and the surfaced internal frame is treated as a bug to fix
