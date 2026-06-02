# Test conventions (tests/testthat)

Guidance specific to the test suite. General package conventions live in the
repo-root `CLAUDE.md`.

## Snapshot tests

- **Capture the construction code, not just the output.** When snapshotting an
  object, wrap *both* the code that builds it and the code that renders it in a
  single `expect_snapshot({ ... })` block, so the snapshot records the call that
  produced the result alongside the result itself. A reviewer reading the
  `_snaps/*.md` file then sees exactly which inputs led to the recorded output.

  ```r
  # Good — construction + print are both in the snapshot
  test_that("scenario: print is stable", {
    expect_snapshot({
      s <- ssd_define_scenario(ssddata::ccme_boron, nsim = 100L, seed = 42L)
      print(s)
    })
  })

  # Avoid — construction happens outside the snapshot, so the inputs are not
  # visible in _snaps/*.md
  test_that("scenario: print is stable", {
    s <- ssd_define_scenario(ssddata::ccme_boron, nsim = 100L, seed = 42L)
    expect_snapshot(print(s))
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
