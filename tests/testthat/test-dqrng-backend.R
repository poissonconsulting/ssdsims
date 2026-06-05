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

# NOTE: the following tests deliberately corrupt base R's single, process-global
# user-supplied RNG slot (tearing the backend down or letting a foreign user-RNG
# package hijack it). Doing that in-process leaves dqrng's registration in a
# state that segfaults a subsequent `register_methods()` -- the exact
# process-global fragility `exploration/user-rng-conflict/` documents. So each
# runs in a fresh `callr` subprocess, where any corruption (or crash) is
# contained and cannot poison the rest of the suite. They need ssdsims installed
# (true under R CMD check, not under a bare `devtools::test()`).

test_that("dqrng-backend: chk_dqrng_backend_intact aborts when the backend is torn down", {
  skip_if_ssdsims_not_installed()
  skip_if_not_installed("dqrng")
  res <- callr::r(function() {
    suppressMessages(library(dqrng))
    ssdsims:::set_dqrng_backend()
    dqrng::dqset.seed(42L)
    # Tear the backend down: route base R RNG back to a non-user generator. The
    # witness draw then advances Mersenne-Twister, not dqrng.
    RNGkind("Mersenne-Twister")
    tryCatch(
      {
        ssdsims:::chk_dqrng_backend_intact()
        NULL
      },
      error = function(e) list(class = class(e), msg = conditionMessage(e))
    )
  })
  expect_true("chk_error" %in% res$class)
  # The message reports the current RNGkind() and does not name a symbol owner.
  expect_match(res$msg, "Mersenne-Twister")
})

test_that("dqrng-backend: chk_dqrng_backend_intact aborts and names the culprit on a foreign hijack", {
  skip_if_ssdsims_not_installed()
  skip_if_not_installed("dqrng")
  # NB: must NOT use skip_if_not_installed("randtoolbox") -- that would load
  # randtoolbox into THIS process (see the helper's note). It is loaded only in
  # the subprocess below.
  skip_if_randtoolbox_not_installed()
  res <- callr::r(function() {
    suppressMessages(library(dqrng))
    suppressMessages(library(randtoolbox)) # loaded last -> wins the slot
    # Mirror the validated `case6` recipe exactly: seed dqrng (so it has a known
    # state) but let randtoolbox -- not `register_methods()` -- own the slot.
    # (Calling `register_methods()` first and *then* hijacking is the case3
    # segfault, an explicit non-goal, so it is deliberately not exercised here.)
    dqrng::dqset.seed(42L)
    # randtoolbox takes the user_unif_rand slot while RNGkind()[1] still reads
    # "user-supplied" -- the cheap probe is fooled (case6), but the state
    # witness is not: the draw advances WELL, not dqrng.
    randtoolbox::set.generator("WELL", order = 512, version = "a", seed = 42)
    err <- tryCatch(
      {
        ssdsims:::chk_dqrng_backend_intact()
        NULL
      },
      error = function(e) list(class = class(e), msg = conditionMessage(e))
    )
    list(
      kind = RNGkind()[1L],
      err = err,
      owner = ssdsims:::rng_slot_owner(),
      providers = ssdsims:::user_rng_providers()
    )
  })
  expect_identical(res$kind, "user-supplied")
  expect_true("chk_error" %in% res$err$class)
  # The message names the owning package and lists the loaded user-RNG providers
  # (both dqrng and randtoolbox export user_unif_rand).
  expect_match(res$err$msg, "randtoolbox")
  expect_identical(res$owner, "randtoolbox")
  expect_true(all(c("dqrng", "randtoolbox") %in% res$providers))
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

test_that("primitives: *_data_task_primer aborts (in its own frame) when the backend is corrupted before the task ends", {
  # Corrupts the RNG slot mid-task, so it runs in a fresh subprocess (see the
  # note above the integrity-witness corruption tests).
  skip_if_ssdsims_not_installed()
  skip_if_not_installed("dqrng")
  skip_if_not_installed("ssddata")
  res <- callr::r(function() {
    suppressMessages(library(dqrng))
    ssdsims:::set_dqrng_backend()
    # Corrupt the backend inside the task body: the op tears the backend down,
    # so the wrapper's exit-bookend integrity check fails.
    testthat::with_mocked_bindings(
      sample_data_task = function(data, n_max, replace) {
        RNGkind("Mersenne-Twister")
        data
      },
      {
        tryCatch(
          {
            ssdsims:::sample_data_task_primer(
              ssddata::ccme_boron,
              n_max = 6L,
              replace = FALSE,
              seed = 42L,
              primer = c(1L, 2L)
            )
            NULL
          },
          error = function(e) {
            list(
              class = class(e),
              call = paste(deparse(conditionCall(e)), collapse = " ")
            )
          }
        )
      },
      .package = "ssdsims"
    )
  })
  expect_true("chk_error" %in% res$class)
  # Error origin is the user-facing per-task wrapper frame, not an internal one.
  expect_match(res$call, "sample_data_task_primer")
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

test_that("conditional dependency: backend aborts and does not load dqrng when dqrng is not loaded", {
  skip_if_ssdsims_not_installed()
  res <- callr::r(function() {
    suppressMessages(library(ssdsims))
    loaded_before <- isNamespaceLoaded("dqrng")
    msg <- tryCatch(
      ssdsims::local_dqrng_backend(),
      error = function(e) conditionMessage(e)
    )
    list(
      loaded_before = loaded_before,
      loaded_after = isNamespaceLoaded("dqrng"),
      msg = msg
    )
  })
  # dqrng is not loaded by ssdsims, and the failed activation does not load it.
  expect_false(res$loaded_before)
  expect_false(res$loaded_after)
  expect_match(res$msg, "library\\(dqrng\\)")
})

test_that("DESCRIPTION: dqrng is in Suggests, not Imports", {
  desc_path <- system.file("DESCRIPTION", package = "ssdsims")
  skip_if(identical(desc_path, ""))
  dcf <- read.dcf(desc_path, fields = c("Imports", "Suggests"))
  expect_false(grepl("dqrng", dcf[, "Imports"], fixed = TRUE))
  expect_true(grepl("dqrng", dcf[, "Suggests"], fixed = TRUE))
})

test_that("conditional dependency: loading ssdsims does not load dqrng", {
  skip_if_ssdsims_not_installed()
  loaded <- callr::r(function() {
    suppressMessages(library(ssdsims))
    isNamespaceLoaded("dqrng")
  })
  expect_false(loaded)
})
