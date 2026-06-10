# ---- generator kinds ---------------------------------------------------------

test_that("scenario-definition: ssd_gen materialises a function generator", {
  out <- ssd_gen(synth = ssdtools::ssd_rlnorm, .n = 30, .seed = 1L)
  expect_s3_class(out, "ssdsims_gen")
  expect_named(out, "synth")
  expect_s3_class(out[["synth"]], "tbl_df")
  expect_identical(nrow(out[["synth"]]), 30L)
  expect_type(out[["synth"]]$Conc, "double")
})

test_that("scenario-definition: a function-name string resolves and names the dataset", {
  from_string <- ssd_gen("ssd_rlnorm", .n = 30, .seed = 1L)
  expect_named(from_string, "ssd_rlnorm")
  from_function <- ssd_gen(
    ssd_rlnorm = ssdtools::ssd_rlnorm,
    .n = 30,
    .seed = 1L
  )
  # The string resolves to the same function and (with the same name, so the
  # same dqrng stream) produces an identical tibble.
  expect_identical(from_string, from_function)
})

test_that("scenario-definition: a string resolves in the caller environment first", {
  my_gen <- function(n) rep(1.5, n)
  out <- ssd_gen("my_gen", .n = 5, .seed = 1L)
  expect_identical(out[["my_gen"]]$Conc, rep(1.5, 5))
})

test_that("scenario-definition: tmbfit and fitdists generators materialise", {
  fit <- ssdtools::ssd_fit_dists(
    ssddata::ccme_boron,
    dists = c("lnorm", "gamma")
  )
  from_tmbfit <- ssd_gen(refit = fit[[1L]], .n = 20, .seed = 1L)
  expect_named(from_tmbfit, "refit")
  expect_identical(nrow(from_tmbfit[["refit"]]), 20L)
  expect_type(from_tmbfit[["refit"]]$Conc, "double")

  # A fitdists selects its top-weighted dist, then follows the tmbfit path.
  wt <- ssdtools::glance(fit, wt = TRUE)$wt
  from_fitdists <- ssd_gen(refit = fit, .n = 20, .seed = 1L)
  from_top <- ssd_gen(refit = fit[[which.max(wt)]], .n = 20, .seed = 1L)
  expect_identical(from_fitdists, from_top)
})

test_that("scenario-definition: several generators materialise in one call", {
  out <- ssd_gen(
    a = ssdtools::ssd_rlnorm,
    b = "ssd_rgamma",
    .n = 10,
    .seed = 1L
  )
  expect_named(out, c("a", "b"))
  expect_identical(vapply(out, nrow, integer(1)), c(a = 10L, b = 10L))
})

# ---- names ---------------------------------------------------------------

test_that("scenario-definition: ssd_gen derives names by symbol capture", {
  expect_named(ssd_gen(ssdtools::ssd_rlnorm, .n = 5, .seed = 1L), "ssd_rlnorm")
})

test_that("scenario-definition: an anonymous generator needs an explicit name", {
  expect_snapshot(error = TRUE, {
    ssd_gen(function(n) rep(1, n), .n = 5, .seed = 1L)
  })
})

test_that("scenario-definition: ssd_gen rejects duplicate names", {
  expect_snapshot(error = TRUE, {
    ssd_gen(
      x = ssdtools::ssd_rlnorm,
      x = ssdtools::ssd_rlnorm,
      .n = 5,
      .seed = 1L
    )
  })
})

# ---- input validation ------------------------------------------------------

test_that("scenario-definition: ssd_gen rejects a data frame", {
  expect_snapshot(error = TRUE, {
    ssd_gen(d = ssddata::ccme_boron, .n = 30, .seed = 1L)
  })
})

test_that("scenario-definition: ssd_gen rejects a non-generator input", {
  expect_snapshot(error = TRUE, {
    ssd_gen(d = 1:5, .n = 5, .seed = 1L)
  })
})

test_that("scenario-definition: an unresolvable function-name string aborts", {
  expect_snapshot(error = TRUE, {
    ssd_gen("no_such_generator", .n = 5, .seed = 1L)
  })
})

test_that("scenario-definition: a name resolving to a non-function aborts", {
  not_a_function <- 1.5
  expect_snapshot(error = TRUE, {
    ssd_gen("not_a_function", .n = 5, .seed = 1L)
  })
})

test_that("scenario-definition: ssd_gen requires at least one generator", {
  expect_snapshot(error = TRUE, {
    ssd_gen(.n = 5, .seed = 1L)
  })
})

test_that("scenario-definition: a generator must draw .n numeric values", {
  expect_snapshot(error = TRUE, {
    ssd_gen(short = function(n) 1, .n = 5, .seed = 1L)
  })
  expect_snapshot(error = TRUE, {
    ssd_gen(chr = function(n) letters[seq_len(n)], .n = 5, .seed = 1L)
  })
})

# ---- .n / .seed ------------------------------------------------------------

test_that("scenario-definition: .n and .seed are required", {
  expect_snapshot(error = TRUE, {
    ssd_gen(synth = ssdtools::ssd_rlnorm, .seed = 1L)
  })
  expect_snapshot(error = TRUE, {
    ssd_gen(synth = ssdtools::ssd_rlnorm, .n = 30)
  })
})

test_that("scenario-definition: seed= and n= name generators, never .seed/.n", {
  # Dot-prefixed formals are not partial-matched from a `seed=`/`n=` named
  # generator: those stay generators (here, ones that resolve by name).
  seed_gen <- function(n) rep(1, n)
  out <- ssd_gen(seed = seed_gen, n = seed_gen, .n = 5, .seed = 1L)
  expect_named(out, c("seed", "n"))
  expect_identical(nrow(out[["seed"]]), 5L)
})

# ---- reproducibility -------------------------------------------------------

test_that("scenario-definition: ssd_gen is byte-identical under one .seed", {
  one <- ssd_gen(synth = ssdtools::ssd_rlnorm, .n = 30, .seed = 42L)
  two <- ssd_gen(synth = ssdtools::ssd_rlnorm, .n = 30, .seed = 42L)
  expect_identical(one, two)
})

test_that("scenario-definition: a different .seed yields different data", {
  one <- ssd_gen(synth = ssdtools::ssd_rlnorm, .n = 30, .seed = 42L)
  other <- ssd_gen(synth = ssdtools::ssd_rlnorm, .n = 30, .seed = 43L)
  expect_false(identical(one, other))
})

test_that("scenario-definition: one .seed fans out on independent name-keyed streams", {
  out <- ssd_gen(
    a = ssdtools::ssd_rlnorm,
    b = ssdtools::ssd_rlnorm,
    .n = 30,
    .seed = 42L
  )
  # The two generators share `.seed` but draw on streams keyed by their names,
  # so their data differ ...
  expect_false(identical(out[["a"]]$Conc, out[["b"]]$Conc))
  # ... and each is reproducible independently of who else is in the call.
  alone <- ssd_gen(b = ssdtools::ssd_rlnorm, .n = 30, .seed = 42L)
  expect_identical(out[["b"]], alone[["b"]])
})

# ---- dqrng contract --------------------------------------------------------

test_that("scenario-definition: ssd_gen aborts when dqrng is not loaded", {
  local_mocked_bindings(dqrng_usable = function() FALSE)
  expect_snapshot(error = TRUE, {
    ssd_gen(synth = ssdtools::ssd_rlnorm, .n = 30, .seed = 1L)
  })
})

test_that("scenario-definition: a generator escaping dqrng aborts", {
  escape <- function(n) {
    RNGkind("Mersenne-Twister")
    stats::runif(n)
  }
  expect_snapshot(error = TRUE, {
    ssd_gen(escape = escape, .n = 5, .seed = 1L)
  })
})

test_that("scenario-definition: a pure (no-draw) generator passes", {
  out <- ssd_gen(pure = function(n) as.numeric(seq_len(n)), .n = 5, .seed = 1L)
  expect_identical(out[["pure"]]$Conc, as.numeric(1:5))
})

test_that("scenario-definition: ssd_gen leaves .Random.seed unchanged", {
  set.seed(101)
  before <- .Random.seed
  ssd_gen(synth = ssdtools::ssd_rlnorm, .n = 30, .seed = 1L)
  expect_identical(before, .Random.seed)
})
