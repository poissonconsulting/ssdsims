## ADDED Requirements

### Requirement: ssd_pmix() assembles a validated min_pmix function collection
The package SHALL expose `ssd_pmix(...)` as the single entry point that assembles
one or more `min_pmix` functions into a validated, named collection — an
`ssdsims_pmix` object keyed by name. Each entry SHALL be a single-argument
function (inputs a row count, returns a proportion in `[0, 0.5]`) — **functions
only; a name-string SHALL NOT be accepted**, so the constructor performs no
string→function resolution. Names SHALL be taken from the `...` argument names
where supplied, otherwise derived per argument by symbol capture for a bare
`symbol` or `pkg::name` (scoped to this constructor, mirroring
`ssd_scenario_data()`), and SHALL be unique. `ssd_define_scenario(min_pmix = ...)`
SHALL accept **only** an `ssd_pmix()` collection.

#### Scenario: Functions assembled and named
- **WHEN** `ssd_pmix(ssd_min_pmix = ssdtools::ssd_min_pmix, strict = my_pmix)` is called
- **THEN** it SHALL return an `ssdsims_pmix` collection carrying both single-argument functions keyed by `"ssd_min_pmix"` and `"strict"`

#### Scenario: Name derived per argument for a bare reference
- **WHEN** `ssd_pmix(ssdtools::ssd_min_pmix)` is called (unnamed)
- **THEN** it SHALL derive the name `"ssd_min_pmix"` by per-argument symbol capture and validate that the supplied value is a single-argument function

#### Scenario: Non-function or wrong-arity entry rejected
- **WHEN** `ssd_pmix()` is given an entry that is not a function, or is a multi-argument function, or a name-string
- **THEN** it SHALL abort with an informative error naming the offending entry (no string→function resolution is attempted)

### Requirement: Legacy min_pmix input forms fail loudly
`ssd_define_scenario()` SHALL reject the pre-constructor input forms for
`min_pmix` with an actionable error raised in the user-facing function's context
(not a silent fallback or an internal `rlang`/`purrr` frame). A `min_pmix`
supplied as a bare function, a (named or unnamed) plain list, or a character
vector of names SHALL abort with a message naming `ssd_pmix()`. No input form
SHALL rely on capturing or parsing the unevaluated argument expression of
`min_pmix`, and no name-string SHALL be resolved to a function.

#### Scenario: min_pmix function/list forms abort
- **WHEN** `ssd_define_scenario(..., min_pmix = ssdtools::ssd_min_pmix)` or `min_pmix = list(ssdtools::ssd_min_pmix)` is called
- **THEN** the constructor SHALL abort with an informative error instructing the caller to use `ssd_pmix()`

#### Scenario: A character vector of names aborts
- **WHEN** `ssd_define_scenario(..., min_pmix = "ssd_min_pmix")` (or `c("default", "strict")`) is called
- **THEN** the constructor SHALL abort with an informative error instructing the caller to use `ssd_pmix()`, resolving no name-string to a function

#### Scenario: Indirectly supplied value aborts cleanly
- **WHEN** `fns <- list(ssdtools::ssd_min_pmix); ssd_define_scenario(..., min_pmix = fns)` is called (the form the expression-inference path mishandled)
- **THEN** the constructor SHALL abort with the same actionable error naming `ssd_pmix()`, not an obscure internal frame

#### Scenario: Naming never reads the argument expression
- **WHEN** `min_pmix` is supplied through an `ssd_pmix()` collection
- **THEN** naming SHALL be determined entirely by the collection's names, and `ssd_define_scenario()` SHALL NOT capture the unevaluated `min_pmix` argument expression

## MODIFIED Requirements

### Requirement: min_pmix referenced by name
`ssd_define_scenario()` SHALL store `min_pmix` in the `fit` grid as one or more names (a character vector) — used for hashing and the task path — and SHALL additionally materialise the single-argument **functions**, keyed by name, on the scenario for execution. The input SHALL be an `ssd_pmix()` collection (which already carries validated functions keyed by name); no other form is accepted. The default SHALL be `ssd_pmix(ssd_min_pmix = ssdtools::ssd_min_pmix)` — an `ssdsims_pmix` value built at construction, not a string and not a captured unnamed-`list()` expression. Naming SHALL be value-based: `ssd_define_scenario()` SHALL NOT capture or parse the unevaluated `min_pmix` argument expression, and SHALL NOT resolve any name-string to a function. The names taken from the collection — and therefore task hashes — SHALL be unchanged versus supplying the same functions under those names the old way, and the function values SHALL NOT be hashed. Resolution of a name back to a function is the scenario accessor `scenario_min_pmix()`, not a separate registry.

#### Scenario: Default min_pmix is the ssd_pmix() collection
- **WHEN** `ssd_define_scenario()` is called without `min_pmix` (the default `ssd_pmix(ssd_min_pmix = ssdtools::ssd_min_pmix)`)
- **THEN** the object's `fit$min_pmix` SHALL be `"ssd_min_pmix"` (the collection's name), and the collection's function SHALL be materialised on the scenario keyed by that name

#### Scenario: ssd_pmix() collection accepted and stored by name
- **WHEN** `ssd_define_scenario(..., min_pmix = ssd_pmix(ssd_min_pmix = ssdtools::ssd_min_pmix, strict = my_pmix))` is called
- **THEN** the object SHALL store the collection's names (`c("ssd_min_pmix", "strict")`) as `fit$min_pmix` and materialise the collection's functions keyed by those names, without reading the argument expression
