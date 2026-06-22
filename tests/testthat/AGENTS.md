# Test conventions (tests/testthat)

The home for all test-suite conventions. The repo-root `AGENTS.md` points
here; keep test guidance in this file rather than there.

## Files & structure

- Tests for `R/{name}.R` go in `tests/testthat/test-{name}.R`; place new tests
  next to similar existing ones.
- All new code should have an accompanying test. Keep tests minimal, with few
  comments.
- Never put code in a `test-{name}.R` file outside a `test_that()` block; use
  `tests/testthat/helper.R` (or `helper-{name}.R`) instead.
- **Prefix every test description with the OpenSpec capability name** it
  exercises, e.g. `test_that("scenario-definition: ...")` for the
  `scenario-definition` capability (see `openspec/specs/`). This ties each test
  back to the capability it covers. (Older tests pre-dating OpenSpec use the
  function name as the prefix; prefer the capability name for new work.)

## Fixtures

- Keep `targets` pipeline fixtures (the `_targets.R` a test sources) as plain
  text templates under `tests/testthat/fixtures/`, copied into a temp dir by the
  test — do not build them inline with `writeLines()`.

## Expectations

- Prefer specific expectations over `expect_true()` / `expect_false()` — they
  give better failure messages.
- When testing **errors and warnings**, do **not** use `expect_error()` /
  `expect_warning()`. Use `expect_snapshot(error = TRUE)` for errors and
  `expect_snapshot()` for warnings so the full text is captured and reviewable.
  Use one `expect_snapshot(error = TRUE, { ... })` block per distinct error
  case (the block stops at the first error).

  ```r
  # Good
  test_that("scenario-definition: invalid seed errors", {
    expect_snapshot(error = TRUE, {
      ssd_define_scenario(ssddata::ccme_boron, seed = 1.5)
    })
  })

  # Avoid
  test_that("scenario-definition: invalid seed errors", {
    expect_error(ssd_define_scenario(ssddata::ccme_boron, seed = 1.5))
  })
  ```

- Avoid the `.package` argument to `local_mocked_bindings()` (it mutates another
  package's namespace); create a mockable wrapper in this package instead.
- **Call internal (non-exported) helpers unqualified** in tests - `testthat`
  runs tests in the package namespace, so `task_axes()`, `resolve_min_pmix()`,
  `decode_obj()`, etc. resolve without `ssdsims:::`. Do not add the `:::`.

## Snapshot tests

- Write snapshot tests for any output that should be stable. Review diffs with
  `testthat::snapshot_review()` and accept with `testthat::snapshot_accept()`.
- **Capture the construction code, not just the output.** Put the code that
  *builds* the object inside the `expect_snapshot()` call, so the snapshot
  records the call that produced the result alongside the result. A reviewer
  reading `_snaps/*.md` then sees exactly which inputs led to the output.
- **Prefer the value, not an explicit `print()`.** Call the constructor (or
  other expression) directly and rely on auto-printing — do not assign to a
  variable and then call `print()`.

  ```r
  # Good — the construction call is the snapshot; auto-print shows the output
  test_that("scenario-definition: print is stable", {
    expect_snapshot(
      ssd_define_scenario(ssddata::ccme_boron, nsim = 100L, seed = 42L)
    )
  })

  # Avoid — extra assignment + explicit print(), and the construction happens
  # outside the snapshot so the inputs are not visible in _snaps/*.md
  test_that("scenario-definition: print is stable", {
    s <- ssd_define_scenario(ssddata::ccme_boron, nsim = 100L, seed = 42L)
    expect_snapshot(print(s))
  })
  ```

- Exercise representative input shapes (e.g. vector- and list-valued
  arguments), not just scalars, so the rendering paths are covered.
- Keep snapshot output deterministic: never deparse function bodies or other
  unstable representations into snapshotted output.

## RNG

- Pin the seed for any RNG-touching test (`withr::with_seed()` /
  `local_dqrng_backend()`).
- Assert `get_dqrng_state()` is unchanged where a function on the dqrng path is
  meant to be free of RNG side effects (the package's actual draw path), as in
  `test-task-shards.R`. Where base R RNG must be untouched, assert
  `.Random.seed` is unchanged.
