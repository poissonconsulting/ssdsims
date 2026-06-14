# ---- ssd_scenario_data() ---------------------------------------------------

test_that("scenario-definition: ssd_scenario_data requires a Conc column", {
  expect_snapshot(error = TRUE, {
    ssd_scenario_data(d = data.frame(x = 1:5))
  })
})

test_that("scenario-definition: ssd_scenario_data rejects a non-numeric Conc column", {
  expect_snapshot(error = TRUE, {
    ssd_scenario_data(d = data.frame(Conc = c("a", "b")))
  })
})

test_that("scenario-definition: ssd_scenario_data returns a named collection of tibbles", {
  out <- ssd_scenario_data(
    boron = data.frame(Conc = c(1, 2, 3), Species = c("a", "b", "c"))
  )
  expect_s3_class(out, "ssdsims_data")
  expect_named(out, "boron")
  expect_s3_class(out[["boron"]], "tbl_df")
  expect_identical(out[["boron"]]$Conc, c(1, 2, 3))
  expect_identical(names(out[["boron"]]), c("Conc", "Species"))
})

test_that("scenario-definition: ssd_scenario_data names via args and symbol capture", {
  expect_named(
    ssd_scenario_data(
      boron = ssddata::ccme_boron,
      cadmium = ssddata::ccme_cadmium
    ),
    c("boron", "cadmium")
  )
  expect_named(
    ssd_scenario_data(ssddata::ccme_boron, ssddata::ccme_cadmium),
    c("ccme_boron", "ccme_cadmium")
  )
})

test_that("scenario-definition: ssd_scenario_data needs a derivable or explicit name", {
  expect_snapshot(error = TRUE, {
    ssd_scenario_data(data.frame(Conc = 1:5))
  })
})

test_that("scenario-definition: ssd_scenario_data rejects duplicate names", {
  expect_snapshot(error = TRUE, {
    ssd_scenario_data(x = ssddata::ccme_boron, x = ssddata::ccme_cadmium)
  })
})

# ---- composing ssd_gen() into ssd_scenario_data() --------------------------

test_that("scenario-definition: an unnamed ssd_gen() argument is flattened in", {
  out <- ssd_scenario_data(
    boron = ssddata::ccme_boron,
    ssd_gen(synth = ssdtools::ssd_rlnorm, .n = 30, .seed = 1L)
  )
  expect_s3_class(out, "ssdsims_data")
  expect_named(out, c("boron", "synth"))
  # Each member is a plain `Conc` tibble; no ssdsims_gen class survives.
  expect_false(any(vapply(out, \(d) inherits(d, "ssdsims_gen"), logical(1))))
  expect_identical(nrow(out[["synth"]]), 30L)
})

test_that("scenario-definition: splicing ssd_gen() is equivalent to the unnamed form", {
  flattened <- ssd_scenario_data(
    boron = ssddata::ccme_boron,
    ssd_gen(synth = ssdtools::ssd_rlnorm, .n = 30, .seed = 1L)
  )
  spliced <- ssd_scenario_data(
    boron = ssddata::ccme_boron,
    !!!ssd_gen(synth = ssdtools::ssd_rlnorm, .n = 30, .seed = 1L)
  )
  expect_identical(flattened, spliced)
})

test_that("scenario-definition: names derived across mixed data-frame and generator inputs", {
  out <- ssd_scenario_data(
    ssddata::ccme_boron,
    !!!ssd_gen(ssdtools::ssd_rlnorm, .n = 30, .seed = 1L)
  )
  expect_named(out, c("ccme_boron", "ssd_rlnorm"))
})

test_that("scenario-definition: a duplicate name across mixed inputs aborts", {
  expect_snapshot(error = TRUE, {
    ssd_scenario_data(
      synth = ssddata::ccme_boron,
      ssd_gen(synth = ssdtools::ssd_rlnorm, .n = 30, .seed = 1L)
    )
  })
})

test_that("scenario-definition: a named ssd_gen() argument is rejected", {
  expect_snapshot(error = TRUE, {
    ssd_scenario_data(
      g = ssd_gen(synth = ssdtools::ssd_rlnorm, .n = 30, .seed = 1L)
    )
  })
})
