## ADDED Requirements

### Requirement: Public as_ssd_data() coerces already-named input forms
The package SHALL expose a public `as_ssd_data()` that coerces the **already-named** dataset input forms into a validated `ssdsims_data` collection, reusing the same numeric-`Conc` per-dataset validation as `ssd_data()`. It SHALL accept (a) an `ssd_data()` collection (returned unchanged), (b) a named list of data frames (names taken from the list), and (c) a single data frame with an explicit `name=`. It SHALL NOT derive names by symbol capture, and therefore SHALL reject a bare unnamed data frame or an unnamed list with an informative error pointing the caller at `ssd_define_scenario()` (which captures the argument expression) or at supplying names via a named list or `name=`. The internal `scenario_datasets()` SHALL delegate its already-named forms to `as_ssd_data()`, deriving symbol-capture names (bare data frame / unnamed list) in the `ssd_define_scenario()` frame before delegating, so that the dataset contract has a single validation path and the existing `ssd_define_scenario()` input behaviour is unchanged.

#### Scenario: Collection passes through unchanged
- **WHEN** `as_ssd_data()` is given an existing `ssd_data()` collection
- **THEN** it SHALL return that `ssdsims_data` collection unchanged, and SHALL abort with an informative error if a redundant `name=` is also supplied

#### Scenario: Named list of data frames coerces
- **WHEN** `as_ssd_data(list(boron = ccme_boron, cadmium = ccme_cadmium))` is given a named list of data frames each with a numeric `Conc` column
- **THEN** it SHALL return an `ssdsims_data` collection using the list names, with each element validated and coerced to a tibble preserving `Conc` and any additional columns

#### Scenario: Single data frame with explicit name coerces
- **WHEN** `as_ssd_data(ccme_boron, name = "boron")` is given a single data frame and an explicit `name=`
- **THEN** it SHALL return a one-element `ssdsims_data` collection named `"boron"` with the data frame validated against the `Conc` contract

#### Scenario: Bare data frame or unnamed list is rejected with a clear message
- **WHEN** `as_ssd_data()` is given a bare data frame with no `name=`, or an unnamed list of data frames
- **THEN** it SHALL abort with an informative error stating that names cannot be derived by symbol capture here and directing the caller to `ssd_define_scenario()` (which captures the argument expression) or to supply names via a named list or `name=`

#### Scenario: scenario_datasets() delegates already-named forms while keeping symbol capture in the constructor frame
- **WHEN** `ssd_define_scenario()` receives a bare data frame (`ssddata::ccme_boron`) or an unnamed list (`list(ssddata::ccme_boron, ssddata::ccme_cadmium)`)
- **THEN** `scenario_datasets()` SHALL derive the name(s) by symbol capture in the constructor frame and then delegate the now-named value to `as_ssd_data()`, producing the same dataset names and the same `Conc` validation as before, with errors surfacing in the `ssd_define_scenario()` frame
