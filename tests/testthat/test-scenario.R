# ---- ssd_data() ------------------------------------------------------------

test_that("scenario-definition: ssd_data requires a Conc column", {
  expect_snapshot(error = TRUE, {
    ssd_data(d = data.frame(x = 1:5))
  })
})

test_that("scenario-definition: ssd_data rejects a non-numeric Conc column", {
  expect_snapshot(error = TRUE, {
    ssd_data(d = data.frame(Conc = c("a", "b")))
  })
})

test_that("scenario-definition: ssd_data returns a named collection of tibbles", {
  out <- ssd_data(
    boron = data.frame(Conc = c(1, 2, 3), Species = c("a", "b", "c"))
  )
  expect_s3_class(out, "ssdsims_data")
  expect_named(out, "boron")
  expect_s3_class(out[["boron"]], "tbl_df")
  expect_identical(out[["boron"]]$Conc, c(1, 2, 3))
  expect_identical(names(out[["boron"]]), c("Conc", "Species"))
})

test_that("scenario-definition: ssd_data names via args and symbol capture", {
  expect_named(
    ssd_data(boron = ssddata::ccme_boron, cadmium = ssddata::ccme_cadmium),
    c("boron", "cadmium")
  )
  expect_named(
    ssd_data(ssddata::ccme_boron, ssddata::ccme_cadmium),
    c("ccme_boron", "ccme_cadmium")
  )
})

test_that("scenario-definition: ssd_data needs a derivable or explicit name", {
  expect_snapshot(error = TRUE, {
    ssd_data(data.frame(Conc = 1:5))
  })
})

test_that("scenario-definition: ssd_data rejects duplicate names", {
  expect_snapshot(error = TRUE, {
    ssd_data(x = ssddata::ccme_boron, x = ssddata::ccme_cadmium)
  })
})

# ---- minimal construction & declarative-only fields ------------------------

test_that("scenario-definition: minimal construction stores declarative fields", {
  s <- ssd_define_scenario(
    ssddata::ccme_boron,
    nsim = 100L,
    nrow = c(5L, 10L),
    seed = 42L
  )
  expect_s3_class(s, "ssdsims_scenario")
  expect_identical(s$seed, 42L)
  expect_identical(s$nsim, 100L)
  expect_identical(s$nrow, c(5L, 10L))
  expect_identical(s$datasets, "ccme_boron")
  expect_named(
    s$fit,
    c(
      "dists",
      "rescale",
      "computable",
      "at_boundary_ok",
      "min_pmix",
      "range_shape1",
      "range_shape2"
    )
  )
  expect_named(
    s$hc,
    c("proportion", "ci", "nboot", "est_method", "ci_method", "parametric")
  )
})

test_that("scenario-definition: stores dataset names, not data frames", {
  s <- ssd_define_scenario(ssddata::ccme_boron, seed = 1L)
  expect_type(s$datasets, "character")
  # no element of the object is a data frame
  expect_false(any(vapply(s, is.data.frame, logical(1))))
  expect_false(any(vapply(s$fit, is.data.frame, logical(1))))
})

test_that("scenario-definition: stores min_pmix by name, not as a function", {
  s <- ssd_define_scenario(ssddata::ccme_boron, seed = 1L)
  expect_type(s$fit$min_pmix, "character")
  expect_identical(s$fit$min_pmix, "ssd_min_pmix")
  # no function bodies stored anywhere in the fit grid
  expect_false(any(vapply(s$fit, is.function, logical(1))))
})

test_that("scenario-definition: min_pmix accepts names, functions, and lists", {
  # character names used as-is
  expect_identical(
    ssd_define_scenario(
      ssddata::ccme_boron,
      seed = 1L,
      min_pmix = c("default", "strict")
    )$fit$min_pmix,
    c("default", "strict")
  )
  # bare function -> derived name
  expect_identical(
    ssd_define_scenario(
      ssddata::ccme_boron,
      seed = 1L,
      min_pmix = ssdtools::ssd_min_pmix
    )$fit$min_pmix,
    "ssd_min_pmix"
  )
  # named list of functions -> list names
  expect_identical(
    ssd_define_scenario(
      ssddata::ccme_boron,
      seed = 1L,
      min_pmix = list(strict = ssdtools::ssd_min_pmix)
    )$fit$min_pmix,
    "strict"
  )
  # unnamed list of functions -> derived names
  expect_identical(
    ssd_define_scenario(
      ssddata::ccme_boron,
      seed = 1L,
      min_pmix = list(ssdtools::ssd_min_pmix)
    )$fit$min_pmix,
    "ssd_min_pmix"
  )
})

test_that("scenario-definition: min_pmix rejects non-function list elements", {
  expect_snapshot(error = TRUE, {
    ssd_define_scenario(ssddata::ccme_boron, seed = 1L, min_pmix = list(1))
  })
})

test_that("scenario-definition: partition_by defaults are populated", {
  s <- ssd_define_scenario(ssddata::ccme_boron, seed = 1L)
  expect_identical(
    s$partition_by,
    list(
      data = c("dataset", "sim", "replace"),
      fit = c("dataset", "sim", "rescale"),
      hc = c("dataset", "sim")
    )
  )
})

test_that("scenario-definition: upload defaults to NULL", {
  s <- ssd_define_scenario(ssddata::ccme_boron, seed = 1L)
  expect_null(s$upload)
})

test_that("scenario-definition: construction leaves .Random.seed unchanged", {
  set.seed(101)
  before <- .Random.seed
  ssd_define_scenario(ssddata::ccme_boron, seed = 7L)
  expect_identical(before, .Random.seed)
})

# ---- dataset input API -----------------------------------------------------

test_that("scenario-definition: accepts an ssd_data() collection", {
  s <- ssd_define_scenario(
    ssd_data(boron = ssddata::ccme_boron, cadmium = ssddata::ccme_cadmium),
    seed = 1L
  )
  expect_identical(s$datasets, c("boron", "cadmium"))
})

test_that("scenario-definition: ssd_data() collection plus name= is an error", {
  expect_snapshot(error = TRUE, {
    ssd_define_scenario(
      ssd_data(boron = ssddata::ccme_boron),
      name = "x",
      seed = 1L
    )
  })
})

test_that("scenario-definition: single data frame derives an implicit name", {
  s <- ssd_define_scenario(ssddata::ccme_boron, seed = 1L)
  expect_identical(s$datasets, "ccme_boron")
})

test_that("scenario-definition: single data frame accepts an explicit name", {
  s <- ssd_define_scenario(ssddata::ccme_boron, name = "boron_data", seed = 1L)
  expect_identical(s$datasets, "boron_data")
})

test_that("scenario-definition: named list uses the list names", {
  s <- ssd_define_scenario(
    list(boron = ssddata::ccme_boron, cadmium = ssddata::ccme_cadmium),
    seed = 1L
  )
  expect_identical(s$datasets, c("boron", "cadmium"))
})

test_that("scenario-definition: unnamed list derives names per element", {
  s <- ssd_define_scenario(
    list(ssddata::ccme_boron, ssddata::ccme_cadmium),
    seed = 1L
  )
  expect_identical(s$datasets, c("ccme_boron", "ccme_cadmium"))
})

test_that("scenario-definition: named list plus name= is an error", {
  expect_snapshot(error = TRUE, {
    ssd_define_scenario(
      list(boron = ssddata::ccme_boron),
      name = "x",
      seed = 1L
    )
  })
})

test_that("scenario-definition: data frame literal with no derivable name errors", {
  expect_snapshot(error = TRUE, {
    ssd_define_scenario(data.frame(Conc = 1:5), seed = 1L)
  })
})

test_that("scenario-definition: bad data in a list aborts via ssd_data", {
  expect_snapshot(error = TRUE, {
    ssd_define_scenario(list(good = ssddata::ccme_boron, bad = 1:5), seed = 1L)
  })
})

# ---- ci = FALSE rejects bootstrap-only knobs -------------------------------

test_that("scenario-definition: ci = FALSE rejects an explicit nboot", {
  expect_snapshot(error = TRUE, {
    ssd_define_scenario(
      ssddata::ccme_boron,
      seed = 1L,
      ci = FALSE,
      nboot = 500
    )
  })
})

test_that("scenario-definition: ci = FALSE rejects ci_method and parametric", {
  expect_snapshot(error = TRUE, {
    ssd_define_scenario(
      ssddata::ccme_boron,
      seed = 1L,
      ci = FALSE,
      ci_method = "MACL"
    )
  })
  expect_snapshot(error = TRUE, {
    ssd_define_scenario(
      ssddata::ccme_boron,
      seed = 1L,
      ci = FALSE,
      parametric = FALSE
    )
  })
})

test_that("scenario-definition: ci = FALSE alone is fine with default knobs", {
  expect_s3_class(
    ssd_define_scenario(ssddata::ccme_boron, seed = 1L, ci = FALSE),
    "ssdsims_scenario"
  )
})

test_that("scenario-definition: ci = c(FALSE, TRUE) retains bootstrap knobs", {
  s <- ssd_define_scenario(
    ssddata::ccme_boron,
    seed = 1L,
    ci = c(FALSE, TRUE),
    nboot = c(100, 1000),
    ci_method = "weighted_samples"
  )
  expect_identical(s$hc$ci, c(FALSE, TRUE))
  expect_identical(s$hc$nboot, c(100, 1000))
})

# ---- argument validation ---------------------------------------------------

test_that("scenario-definition: invalid seed errors", {
  expect_snapshot(error = TRUE, {
    ssd_define_scenario(ssddata::ccme_boron, seed = c(1L, 2L))
  })
  expect_snapshot(error = TRUE, {
    ssd_define_scenario(ssddata::ccme_boron, seed = 1.5)
  })
  expect_snapshot(error = TRUE, {
    ssd_define_scenario(ssddata::ccme_boron, seed = NULL)
  })
})

test_that("scenario-definition: out-of-range nrow errors", {
  expect_snapshot(error = TRUE, {
    ssd_define_scenario(ssddata::ccme_boron, seed = 1L, nrow = 4L)
  })
  expect_snapshot(error = TRUE, {
    ssd_define_scenario(ssddata::ccme_boron, seed = 1L, nrow = 1001L)
  })
})

# ---- print method ----------------------------------------------------------

test_that("scenario-definition: print is stable for a single dataset", {
  expect_snapshot(
    ssd_define_scenario(
      ssddata::ccme_boron,
      nsim = 100L,
      nrow = c(5L, 10L),
      seed = 42L
    )
  )
})

test_that("scenario-definition: print is stable for multiple datasets and vector knobs", {
  expect_snapshot(
    ssd_define_scenario(
      list(boron = ssddata::ccme_boron, cadmium = ssddata::ccme_cadmium),
      nsim = 50L,
      nrow = c(5L, 6L, 10L),
      seed = 1L,
      rescale = c(FALSE, TRUE),
      computable = c(FALSE, TRUE),
      at_boundary_ok = c(TRUE, FALSE),
      range_shape1 = list(c(0.05, 20), c(0.1, 10)),
      proportion = c(0.05, 0.1),
      ci = c(FALSE, TRUE),
      nboot = c(100, 1000),
      est_method = c("multi", "geometric"),
      ci_method = c("weighted_samples", "MACL"),
      parametric = c(TRUE, FALSE)
    )
  )
})
