# `ssd_design()` collection constructor + name->value consistency contract.

make_scenario <- function(data = ssd_scenario_data(ssddata::ccme_boron), ...) {
  ssd_define_scenario(data, nsim = 2L, seed = 42L, ...)
}

test_that("names are derived from the argument expression or explicit", {
  base <- make_scenario()
  wide <- make_scenario(nrow = c(5L, 10L))
  design <- ssd_design(base, wide = wide)
  expect_s3_class(design, "ssdsims_design")
  expect_identical(names(design), c("base", "wide"))
})

test_that("input order is preserved", {
  a <- make_scenario()
  b <- make_scenario()
  c <- make_scenario()
  expect_identical(names(ssd_design(c, a, b)), c("c", "a", "b"))
})

test_that("a design of one is valid and uniformly shaped", {
  base <- make_scenario()
  design <- ssd_design(base)
  expect_s3_class(design, "ssdsims_design")
  expect_identical(names(design), "base")
  expect_length(design, 1L)
})

test_that("an empty call aborts", {
  expect_error(ssd_design(), "at least one scenario")
})

test_that("a non-scenario element aborts", {
  base <- make_scenario()
  expect_error(ssd_design(base, "nope"), "must be an `ssdsims_scenario`")
})

test_that("duplicate names abort", {
  base <- make_scenario()
  expect_error(ssd_design(a = base, a = base), "must be unique")
})

test_that("unsafe and empty names abort", {
  base <- make_scenario()
  expect_error(ssd_design(`1bad` = base), "must start with a letter")
  expect_error(ssd_design(`a-b` = base), "must start with a letter")
})

test_that("an undrivable name aborts", {
  expect_error(ssd_design(make_scenario()), "Unable to derive a name")
})

test_that("inconsistent dataset bindings abort", {
  boron <- ssd_scenario_data(d = ssddata::ccme_boron)
  other <- ssd_scenario_data(d = ssddata::ccme_cadmium)
  a <- make_scenario(data = boron)
  b <- make_scenario(data = other)
  expect_error(ssd_design(a, b), "dataset name .* binds different values")
})

test_that("differing partition_by aborts", {
  a <- make_scenario()
  b <- make_scenario()
  b$partition_by <- list(
    sample = "dataset",
    fit = character(0),
    hc = character(0)
  )
  expect_error(ssd_design(a, b), "same `partition_by`")
})

test_that("consistent bindings across members are accepted", {
  data <- ssd_scenario_data(ssddata::ccme_boron)
  a <- ssd_define_scenario(data, nsim = 2L, seed = 42L, nrow = c(5L, 10L))
  b <- ssd_define_scenario(data, nsim = 2L, seed = 42L, nrow = c(6L, 7L))
  expect_s3_class(ssd_design(a, b), "ssdsims_design")
})

test_that("construction is RNG-free", {
  base <- make_scenario()
  set.seed(1L)
  before <- .Random.seed
  ssd_design(base, base2 = make_scenario())
  expect_identical(.Random.seed, before)
})

test_that("differing seeds across members are allowed", {
  data <- ssd_scenario_data(ssddata::ccme_boron)
  a <- ssd_define_scenario(data, nsim = 2L, seed = 42L)
  b <- ssd_define_scenario(data, nsim = 2L, seed = 43L)
  design <- ssd_design(a, b)
  expect_identical(
    vapply(design, function(s) s$seed, integer(1L)),
    c(a = 42L, b = 43L)
  )
})
