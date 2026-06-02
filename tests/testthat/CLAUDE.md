# Test conventions (tests/testthat)

Guidance specific to the test suite. General package conventions live in the
repo-root `CLAUDE.md`.

## Snapshot tests

- **Capture the construction code, not just the output.** When snapshotting an
  object, put the code that *builds* it inside the `expect_snapshot()` call, so
  the snapshot records the call that produced the result alongside the result
  itself. A reviewer reading `_snaps/*.md` then sees exactly which inputs led to
  the recorded output.

- **Prefer the value, not an explicit `print()`.** Call the constructor (or
  other expression) directly and rely on auto-printing — do not assign to a
  variable and then call `print()`. Auto-print dispatches the same `print`
  method, and the snapshot stays focused on the creating expression.

  ```r
  # Good — the construction call is the snapshot; auto-print shows the output
  test_that("scenario: print is stable", {
    expect_snapshot(
      ssd_define_scenario(ssddata::ccme_boron, nsim = 100L, seed = 42L)
    )
  })

  # Avoid — extra assignment + explicit print(), and (worse) the construction
  # happens outside the snapshot so the inputs are not visible in _snaps/*.md
  test_that("scenario: print is stable", {
    s <- ssd_define_scenario(ssddata::ccme_boron, nsim = 100L, seed = 42L)
    expect_snapshot(print(s))
  })
  ```

- **Snapshot errors with `expect_snapshot(error = TRUE, { ... })`, not
  `expect_error(..., regexp)`.** This records the actual error message in the
  snapshot (so wording changes are reviewed as diffs) instead of asserting a
  hand-written regex. Put the erroring call in the block; if a test exercises
  several distinct error cases, use one `expect_snapshot(error = TRUE, { ... })`
  per case (the block stops at the first error).

  ```r
  # Good
  test_that("scenario: invalid seed errors", {
    expect_snapshot(error = TRUE, {
      ssd_define_scenario(ssddata::ccme_boron, seed = 1.5)
    })
  })

  # Avoid
  test_that("scenario: invalid seed errors", {
    expect_error(ssd_define_scenario(ssddata::ccme_boron, seed = 1.5))
  })
  ```

- Exercise representative input shapes in snapshots (e.g. vector- and
  list-valued arguments), not just scalars, so the rendering paths are covered.
- Keep snapshot output deterministic: never deparse function bodies or other
  unstable representations into snapshotted output.
- Review snapshot diffs with `testthat::snapshot_review()` and accept with
  `testthat::snapshot_accept()`.

## General

- Prefix every test description with the feature name: `test_that("scenario: ...")`.
- Pin the seed for any RNG-touching test (`withr::with_seed()` /
  `local_lecuyer_cmrg_seed()`); assert `.Random.seed` is unchanged where a
  function is meant to be free of RNG side effects.
