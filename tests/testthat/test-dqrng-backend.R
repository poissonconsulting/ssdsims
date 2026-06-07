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

test_that("dqrng-backend: chk_dqrng_backend_intact is intact and non-destructive when dqrng holds the slot", {
  local_dqrng_backend()
  dqrng::dqset.seed(42L, stream = c(1L, 2L))

  # Intact: dqrng is the bound generator, so the guard returns invisibly (NULL).
  expect_null(chk_dqrng_backend_intact())

  # Non-destructive: the witness advances dqrng's state then restores it, so a
  # seeded draw sequence is byte-identical with and without an intervening call.
  dqrng::dqset.seed(42L, stream = c(1L, 2L))
  without_witness <- runif(3)
  dqrng::dqset.seed(42L, stream = c(1L, 2L))
  invisible(chk_dqrng_backend_intact())
  with_witness <- runif(3)
  expect_identical(without_witness, with_witness)
})

test_that("dqrng-backend: chk_dqrng_backend_intact aborts when the backend is torn down", {
  # Tear base R's RNG down to a non-user-supplied generator (no foreign package
  # needed): dqrng no longer serves the draw, so the witness aborts. The message
  # reports the current RNGkind() and names no symbol owner (it would mislead --
  # the symbol still resolves to a loaded DLL that is not serving RNG).
  withr::defer(suppressWarnings(RNGkind("Mersenne-Twister")))
  suppressWarnings(RNGkind("Mersenne-Twister"))
  expect_false(identical(RNGkind()[1L], "user-supplied"))
  expect_snapshot(error = TRUE, chk_dqrng_backend_intact())
})
