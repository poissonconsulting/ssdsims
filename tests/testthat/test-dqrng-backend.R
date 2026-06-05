test_that("dqrng-backend: set_dqrng_backend activates dqrng and reset reverts", {
  skip_if_not_installed("dqrng")
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
  skip_if_not_installed("dqrng")
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
  skip_if_not_installed("dqrng")
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
  skip_if_not_installed("dqrng")
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
  skip_if_not_installed("dqrng")
  # The backend is scoped to scenario execution: after a scenario completes,
  # base R RNG functions are no longer routed through dqrng.
  set.seed(1)
  pre <- runif(3)

  suppressWarnings(ssd_run_scenario("rlnorm", nsim = 1))

  set.seed(1)
  post <- runif(3)
  expect_identical(pre, post)
})

# ---- integrity witness: chk_dqrng_backend_intact() -------------------------

test_that("dqrng-backend: chk_dqrng_backend_intact is intact and non-destructive under an active backend", {
  skip_if_not_installed("dqrng")
  local_dqrng_backend()
  dqrng::dqset.seed(42L, stream = c(1L, 2L))

  # The witness advances dqrng's state with a base-R draw, then restores it:
  # the recorded state is unchanged across the call (advanced-then-restored).
  state_before <- dqrng::dqrng_get_state()
  expect_invisible(chk_dqrng_backend_intact())
  expect_identical(dqrng::dqrng_get_state(), state_before)
})

test_that("dqrng-backend: chk_dqrng_backend_intact does not perturb the draw sequence", {
  skip_if_not_installed("dqrng")
  local_dqrng_backend()

  dqrng::dqset.seed(99L, stream = c(5L, 6L))
  without_check <- c(runif(2), runif(2))

  dqrng::dqset.seed(99L, stream = c(5L, 6L))
  with_check <- c(
    runif(2),
    {
      chk_dqrng_backend_intact()
      runif(2)
    }
  )

  # The witness consumes no net randomness: the sequences are byte-identical.
  expect_identical(without_check, with_check)
})

# The witness's failure paths against a *foreign* user-supplied RNG hijack are
# validated by the `exploration/user-rng-conflict/` reprexes (case6 = the cheap
# probe is fooled but the state witness is not; case7 = naming the slot owner)
# rather than re-tested here, which would require co-loading a second user-RNG
# package (e.g. randtoolbox). Loading two user-RNG packages in one process is a
# process-global, potentially crashing act -- the very hazard this change
# guards against -- so the suite does not do it. The torn-down branch is
# covered below without activating (then corrupting) the backend.

test_that("dqrng-backend: chk_dqrng_backend_intact aborts when dqrng is not the active backend", {
  skip_if_not_installed("dqrng")
  # No backend is active here, so base R's RNG is not user-supplied: a base-R
  # draw does not advance dqrng's state, so the witness reports a torn-down
  # backend. This is the safe, in-process analogue of a mid-task teardown.
  expect_false(dqrng_backend_active())
  expect_error(chk_dqrng_backend_intact(), class = "chk_error")
  # The message reports the current (non-user-supplied) RNGkind() and names no
  # symbol owner.
  expect_error(chk_dqrng_backend_intact(), regexp = RNGkind()[1L])
})

# ---- per-task postcondition wiring -----------------------------------------

test_that("primitives: *_data_task_primer returns normally on a healthy backend", {
  skip_if_not_installed("dqrng")
  skip_if_not_installed("ssddata")
  local_dqrng_backend()

  out <- sample_data_task_primer(
    ssddata::ccme_boron,
    n_max = 6L,
    replace = FALSE,
    seed = 42L,
    primer = c(1L, 2L)
  )
  expect_s3_class(out, "data.frame")
  expect_identical(nrow(out), 6L)
})

# ---- dqrng as a conditionally-used Suggested dependency --------------------

test_that("conditional dependency: dqrng_usable() is TRUE and activation succeeds when dqrng is loaded", {
  skip_if_not_installed("dqrng")
  expect_true(dqrng_usable())
  # Activation succeeds and the backend is in effect inside the scope.
  local({
    local_dqrng_backend()
    expect_true(dqrng_backend_active())
  })
})

test_that("conditional dependency: backend aborts with actionable guidance when dqrng is not usable", {
  skip_if_not_installed("dqrng")
  # dqrng cannot be cleanly unloaded mid-session, so force the usability gate
  # FALSE to exercise the unavailable path: the backend must abort with
  # actionable guidance rather than load dqrng or fall back to base R's RNG.
  expect_false(dqrng_backend_active())
  local_mocked_bindings(dqrng_usable = function() FALSE)
  expect_error(local_dqrng_backend(), class = "chk_error")
  expect_error(local_dqrng_backend(), regexp = "library\\(dqrng\\)")
})

test_that("DESCRIPTION: dqrng is in Suggests, not Imports", {
  # Regression for the Imports -> Suggests move: with dqrng only suggested,
  # loading ssdsims does not load dqrng.
  desc_path <- system.file("DESCRIPTION", package = "ssdsims")
  skip_if(identical(desc_path, ""))
  dcf <- read.dcf(desc_path, fields = c("Imports", "Suggests"))
  expect_false(grepl("dqrng", dcf[, "Imports"], fixed = TRUE))
  expect_true(grepl("dqrng", dcf[, "Suggests"], fixed = TRUE))
})
