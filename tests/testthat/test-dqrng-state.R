test_that("parallel-safe-seeding: local_dqrng_state installs state and restores on exit", {
  local_dqrng_backend()

  dqrng::dqset.seed(7L, stream = c(3L, 4L))
  before <- get_dqrng_state()

  local({
    local_dqrng_state(42L, c(1L, 2L))
    runif(3)
  })

  # The outer stream is restored to exactly where it was before the scope.
  expect_identical(get_dqrng_state(), before)
})

test_that("parallel-safe-seeding: same (seed, state) reproduces identical draws", {
  local_dqrng_backend()

  seq_1 <- local({
    local_dqrng_state(123L, c(7L, 11L))
    runif(5)
  })
  seq_2 <- local({
    local_dqrng_state(123L, c(7L, 11L))
    runif(5)
  })
  expect_identical(seq_1, seq_2)
})

test_that("parallel-safe-seeding: surrounding stream undisturbed by a nested local_dqrng_state scope", {
  with_nested <- local({
    local_dqrng_backend()
    dqrng::dqset.seed(7L, stream = c(3L, 4L))
    first <- runif(2)
    local({
      local_dqrng_state(99L, c(5L, 6L))
      runif(2)
    })
    c(first, runif(2))
  })

  without_nested <- local({
    local_dqrng_backend()
    dqrng::dqset.seed(7L, stream = c(3L, 4L))
    c(runif(2), runif(2))
  })

  expect_identical(with_nested, without_nested)
})

test_that("parallel-safe-seeding: local_dqrng_state aborts on entry when the backend is not intact", {
  # Entry precondition: with no active (intact) backend, the upgraded entry
  # guard (chk_dqrng_backend_intact()) refuses to start the task.
  withr::defer(suppressWarnings(RNGkind("Mersenne-Twister")))
  suppressWarnings(RNGkind("Mersenne-Twister"))
  expect_false(dqrng_backend_active())
  expect_snapshot(error = TRUE, {
    local_dqrng_state(42L, c(1L, 2L))
  })
})

test_that("parallel-safe-seeding: exit witness aborts a normally-returning task that tore the backend down", {
  local_dqrng_backend()
  withr::defer(suppressWarnings(RNGkind("Mersenne-Twister")))

  # A task closure (the real per-task shape) that draws, tears the backend down,
  # then returns normally -> the deferred exit witness fires and aborts.
  torn_task <- function() {
    local_dqrng_state(1L, c(1L, 2L))
    runif(1)
    suppressWarnings(RNGkind("Mersenne-Twister"))
    "result"
  }
  expect_error(torn_task(), "dqrng backend is not intact")

  # A healthy closure: the deferred exit witness passes and the result returns.
  local_dqrng_backend()
  healthy_task <- function() {
    local_dqrng_state(2L, c(1L, 2L))
    runif(1)
    "result"
  }
  expect_identical(healthy_task(), "result")
})

test_that("parallel-safe-seeding: exit witness is skipped on error and does not mask the task's own error", {
  local_dqrng_backend()
  withr::defer(suppressWarnings(RNGkind("Mersenne-Twister")))

  # The body errors while the backend is torn down. The returnValue() gate skips
  # the exit witness on the error unwind, so the body's OWN error propagates.
  failing_task <- function() {
    local_dqrng_state(1L, c(1L, 2L))
    suppressWarnings(RNGkind("Mersenne-Twister"))
    stop("boom: the real task failure")
  }
  expect_error(failing_task(), "boom: the real task failure")
})

test_that("parallel-safe-seeding: local_dqrng_state validation", {
  local_dqrng_backend()
  expect_snapshot(error = TRUE, {
    local_dqrng_state(1.5, c(1L, 2L))
  })
  expect_snapshot(error = TRUE, {
    local_dqrng_state(42L, c(1L, 2L, 3L))
  })
})

test_that("parallel-safe-seeding: local_dqrng_state accepts NA_integer_ in the primer", {
  local_dqrng_backend()
  expect_identical(
    local({
      local_dqrng_state(42L, c(NA_integer_, 2L))
    }),
    c(NA_integer_, 2L)
  )
})

test_that("parallel-safe-seeding: with_dqrng_state evaluates code and restores state", {
  local_dqrng_backend()

  dqrng::dqset.seed(7L, stream = c(3L, 4L))
  before <- get_dqrng_state()

  draws <- with_dqrng_state(123L, c(7L, 11L), runif(5))

  expect_identical(get_dqrng_state(), before)
  expect_identical(
    draws,
    local({
      local_dqrng_state(123L, c(7L, 11L))
      runif(5)
    })
  )
})
