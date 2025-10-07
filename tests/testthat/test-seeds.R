test_that("ssd_get_seeds no streams or seeds", {
  expect_identical(ssd_get_seeds(nstreams = 0L), list())
  expect_identical(ssd_get_seeds(nseeds = 0L), list(list()))
  expect_identical(ssd_get_seeds(nseeds = 0L, nstreams = 0L), list())
  expect_identical(ssd_get_seeds(nseeds = 0L, nstreams = 2L), list(list(),list()))
})

test_that("ssd_get_seeds no streams or seeds", {
 #expect_snapshot(ssd_get_seeds(as.integer(10), nseeds = 2L))
  expect_snapshot(withr::with_seed(10, ssd_get_seeds(nseeds = 2L)))
  expect_snapshot(withr::with_seed(10, ssd_get_seeds(nseeds = 2L, nstreams = 2L)))
 
})
