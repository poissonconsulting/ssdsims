test_that("ssd_pmix: names taken from argument names", {
  default <- function(n) 0.05
  strict <- function(n) 0.1
  p <- ssd_pmix(default = default, strict = strict)
  expect_s3_class(p, "ssdsims_pmix")
  expect_identical(names(p), c("default", "strict"))
  expect_identical(unclass(p), list(default = default, strict = strict))
})

test_that("ssd_pmix: prints a successful collection", {
  expect_snapshot(
    ssd_pmix(ssd_min_pmix = ssdtools::ssd_min_pmix, strict = function(n) 0.1)
  )
})

test_that("ssd_pmix: derives a name from a bare pkg::name reference", {
  p <- ssd_pmix(ssdtools::ssd_min_pmix)
  expect_identical(names(p), "ssd_min_pmix")
  expect_identical(p[["ssd_min_pmix"]], ssdtools::ssd_min_pmix)
})

test_that("ssd_pmix: derives a name from a bare symbol reference", {
  my_fun <- function(n) 0.05
  p <- ssd_pmix(my_fun)
  expect_identical(names(p), "my_fun")
})

test_that("ssd_pmix: rejects a non-function entry", {
  expect_snapshot(error = TRUE, ssd_pmix(1))
})

test_that("ssd_pmix: rejects a name-string entry (no string resolution)", {
  expect_snapshot(error = TRUE, ssd_pmix("ssd_min_pmix"))
})

test_that("ssd_pmix: rejects a multi-argument function", {
  expect_snapshot(error = TRUE, ssd_pmix(bad = function(a, b) 0.05))
})

test_that("ssd_pmix: rejects duplicate names", {
  f <- function(n) 0.05
  expect_snapshot(error = TRUE, ssd_pmix(a = f, a = f))
})

test_that("ssd_pmix: requires at least one function", {
  expect_snapshot(error = TRUE, ssd_pmix())
})

test_that("ssd_pmix: an unnamed bare literal cannot derive a name", {
  expect_snapshot(error = TRUE, ssd_pmix(function(n) 0.05))
})
