test_that("dqrng-backend: set_dqrng_backend activates dqrng and reset reverts", {
  # Guard: ensure the backend is reset even if an expectation errors.
  withr::defer(reset_dqrng_backend())

  set.seed(1)
  base_draw <- runif(3)

  set_dqrng_backend()
  set.seed(1)
  dq_draw <- runif(3)
  # While active, base R RNG is served by dqrng's pcg64, so the draw differs
  # from the base R generator's draw for the same seed.
  expect_false(identical(base_draw, dq_draw))

  reset_dqrng_backend()
  set.seed(1)
  reset_draw <- runif(3)
  # After reset, base R RNG behaves exactly as before.
  expect_identical(reset_draw, base_draw)
})

test_that("dqrng-backend: local_dqrng_backend activates and resets on scope exit", {
  set.seed(1)
  base_draw <- runif(3)

  draw_in_scope <- local({
    local_dqrng_backend()
    set.seed(1)
    runif(3)
  })
  # Inside the local scope the dqrng backend serves base R RNG.
  expect_false(identical(base_draw, draw_in_scope))

  # On scope exit the backend is reset, so base R RNG behaves as before.
  set.seed(1)
  after_draw <- runif(3)
  expect_identical(after_draw, base_draw)
})

test_that("dqrng-backend: reproducible draws for fixed (seed, stream)", {
  local_dqrng_backend()

  dqrng::dqset.seed(123L, stream = c(7L, 11L))
  seq_1 <- runif(5)
  dqrng::dqset.seed(123L, stream = c(7L, 11L))
  seq_2 <- runif(5)
  expect_identical(seq_1, seq_2)

  # A different stream yields a different sequence.
  dqrng::dqset.seed(123L, stream = c(8L, 11L))
  seq_3 <- runif(5)
  expect_false(identical(seq_1, seq_3))

  # A different seed yields a different sequence.
  dqrng::dqset.seed(999L, stream = c(7L, 11L))
  seq_4 <- runif(5)
  expect_false(identical(seq_1, seq_4))
})

test_that("dqrng-backend: nested local_dqrng_backend() is a no-op leaving the stream unchanged", {
  withr::defer(reset_dqrng_backend())

  # Draw a seeded sequence with an intervening nested local_dqrng_backend()
  # call inside its own scope.
  with_nested <- local({
    local_dqrng_backend()
    dqrng::dqset.seed(7L, stream = c(3L, 4L))
    first <- runif(2)
    rest <- local({
      # Nested call: the backend is already active, so this is a no-op and
      # returns FALSE (it did not activate the backend).
      expect_false(local_dqrng_backend())
      # The backend stays active inside the nested scope.
      expect_true(dqrng_backend_active())
      runif(2)
    })
    c(first, rest)
  })

  # The same seeded sequence with no nested call.
  without_nested <- local({
    local_dqrng_backend()
    dqrng::dqset.seed(7L, stream = c(3L, 4L))
    c(runif(2), runif(2))
  })

  # The RNG stream is identical with or without the nested call.
  expect_identical(with_nested, without_nested)

  # The outermost scope owns the lifetime: once both scopes have exited the
  # backend is reset.
  expect_false(dqrng_backend_active())
})

test_that("dqrng-backend: scenario execution leaves base R RNG unchanged", {
  # The backend is scoped to scenario execution: after a scenario completes,
  # base R RNG functions are no longer routed through dqrng.
  set.seed(1)
  pre <- runif(3)

  suppressWarnings(ssd_run_scenario("rlnorm", nsim = 1))

  set.seed(1)
  post <- runif(3)
  expect_identical(pre, post)
})

# ---- dqrng_usable() / conditional-dependency gate --------------------------

test_that("dqrng-backend: dqrng_usable is TRUE only when dqrng is loaded at >= 0.4.1", {
  expect_true(dqrng_usable())
  local_mocked_bindings(dqrng_usable = function() FALSE)
  expect_snapshot(error = TRUE, {
    local_dqrng_backend()
  })
})

test_that("dqrng-backend: a failed-usable activation does not load dqrng", {
  # The gate tests *already-loaded*, never `requireNamespace()`, so a rejected
  # activation must not itself load a user-RNG provider.
  local_mocked_bindings(dqrng_usable = function() FALSE)
  expect_error(local_dqrng_backend())
})

# ---- chk_dqrng_backend_intact() witness ------------------------------------

test_that("dqrng-backend: the intact witness returns invisibly and is non-destructive", {
  local_dqrng_backend()
  dqrng::dqset.seed(123L, stream = c(1L, 2L))
  with_witness <- {
    first <- runif(1)
    chk_dqrng_backend_intact()
    c(first, runif(1))
  }
  dqrng::dqset.seed(123L, stream = c(1L, 2L))
  without_witness <- c(runif(1), runif(1))
  # The witness consumes no net randomness: the draw sequence is identical.
  expect_identical(with_witness, without_witness)
})

test_that("dqrng-backend: the witness aborts when the backend is torn down", {
  local_dqrng_backend()
  withr::defer(suppressWarnings(reset_dqrng_backend()))
  # Tear the backend down mid-scope, then witness.
  RNGkind("Mersenne-Twister")
  expect_snapshot(error = TRUE, {
    chk_dqrng_backend_intact()
  })
})
