<!-- NEWS.md is maintained by https://fledge.cynkra.com, contributors should not edit this file -->

# ssdsims 0.0.0.9009

## Bug fixes

- Fix `ssd_run_scenario.tmbfit()` to pass fitted estimates to the rng call (#64).

## Features

- Distinguish between RNG state and seed, add glossary documentation (#61).

## Chore

- Add input validation to `*_lecuyer_cmrg_*` seed functions (#63).

- Forward `with_lecuyer_cmrg_seed()` to `local_lecuyer_cmrg_seed()`.

## Testing

- Replace `withr::with_seed()` with `with_lecuyer_cmrg_seed()` in tests (#60).

- Remove spurious `.Random.seed` warning.

- Use default pillar settings.


# ssdsims 0.0.0.9008

## Chore

- Replace `seq_up()` with `rlang::seq2()` (#55).

## Continuous integration

- Update ccache-action reference.

- Bump action version.


# ssdsims 0.0.0.9007

- Ci: Unify fledge.yaml across cynkratemplate and fledge (#86).


# ssdsims 0.0.0.9006

## Chore

- Add ccache to `.gitignore` and `.Rbuildignore`.

## Continuous integration

- Create snapshot update PR against correct branch.

- Add reference to `/apply-patch` workflow in commit message.

- Clarify rationale for not deploying on schedule.


# ssdsims 0.0.0.9005

## Chore

- Auto-update from GitHub Actions.

  Run: https://github.com/poissonconsulting/ssdsims/actions/runs/25267043302

- Auto-update from GitHub Actions.

  Run: https://github.com/poissonconsulting/ssdsims/actions/runs/22932710449

- Auto-update from GitHub Actions.

  Run: https://github.com/poissonconsulting/ssdsims/actions/runs/22789311191

- Auto-update from GitHub Actions.

  Run: https://github.com/poissonconsulting/ssdsims/actions/runs/22685862780

- Auto-update from GitHub Actions.

  Run: https://github.com/poissonconsulting/ssdsims/actions/runs/19879530192

- Auto-update from GitHub Actions.

  Run: https://github.com/poissonconsulting/ssdsims/actions/runs/19808536137

- Auto-update from GitHub Actions.

  Run: https://github.com/poissonconsulting/ssdsims/actions/runs/19751744050

- Build-ignore.

- Auto-update from GitHub Actions.

  Run: https://github.com/poissonconsulting/ssdsims/actions/runs/19489314581

- Auto-update from GitHub Actions.

  Run: https://github.com/poissonconsulting/ssdsims/actions/runs/19481934419

- Format with air (#44).

## Continuous integration

- Add fledge workflow, no-op until it is used in this repo.

- Tweak ccache action.

- Cosmetics.

- Bump action versions.

- Install clang-format-21.

- Harmonize.

- Fix running without readr.

- Adapt workflows (#47).

## Documentation

- Fix title for `?ssd_hc_sims` (#43).

## Uncategorized

- Merge branch 'main' of github.com:poissonconsulting/ssdsims.

- Merge branch 'main' of github.com:poissonconsulting/ssdsims.

- Merge pull request #46 from poissonconsulting/copilot/rename-ssd-simulate-data.


# ssdsims 0.0.0.9004

- Vectorize `ssd_fit_dists_sims()`.


# ssdsims 0.0.0.9003

-  Vectorize `ssd_hc_sims(ci_method)`.
-  Vectorize `ssd_sim_data()`


# ssdsims 0.0.0.9002

- Same as previous version.

# ssdsims 0.0.0.9001

- Merge pull request #30 from poissonconsulting/joethorley/issue21.

- Merge branch 'main' of github.com:poissonconsulting/ssdsims.

- Pass ... through `ssd_run_scenario()`.

- Add .progress = FALSE argument.

- Add `ci = FALSE` argument.

- Merge pull request #22 from poissonconsulting/fits.

- Merge pull request #20 from poissonconsulting/fits.

- Add `start_stream = 1L` argument.

- Renamed skip to start_seeds.

- Rename `ssd_get_seeds()` to `ssd_get_streams_seeds()`.

- Merge branch 'main' into seeds.

- Added `ssd_get_seeds()`.

- `generate_data()` vectorized for nrow.

- Added `ssd_generate_data()`.


# ssdsims 0.0.0.9000

- Added a `NEWS.md` file to track changes to the package.
