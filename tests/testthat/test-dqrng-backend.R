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
