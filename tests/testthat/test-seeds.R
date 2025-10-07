test_that("ssd_get_seeds no streams or seeds", {
  expect_identical(ssd_get_seeds(nstreams = 0L), list())
  expect_identical(ssd_get_seeds(nseeds = 0L), list(list()))
  expect_identical(ssd_get_seeds(nseeds = 0L, nstreams = 0L), list())
  expect_identical(ssd_get_seeds(nseeds = 0L, nstreams = 2L), list(list(),list()))
})

test_that("ssd_get_seeds streams and skip", {
  expect_snapshot(withr::with_seed(10, ssd_get_seeds(nseeds = 2L)))
  expect_snapshot(withr::with_seed(10, ssd_get_seeds(nseeds = 2L, nstreams = 2L)))
  expect_snapshot(withr::with_seed(10, ssd_get_seeds(nseeds = 1L, nstreams = 2L, skip = 1L))) 
  expect_identical(withr::with_seed(10, ssd_get_seeds(nseeds = 2L, nstreams = 2L))[[2]][2],
                   withr::with_seed(10, ssd_get_seeds(nseeds = 1L, nstreams = 2L, skip = 1L))[[2]])
})

test_that("ssd_get_seeds advances seed by 1", {
  withr::with_seed(10, {
    seed <- globalenv()$.Random.seed
    expect_snapshot(globalenv()$.Random.seed)
    ssd_get_seeds(nseeds = 2L)
    seed2 <- globalenv()$.Random.seed 
    expect_snapshot(globalenv()$.Random.seed)
    set.seed(10)
    expect_identical(seed, globalenv()$.Random.seed)
    runif(1)
    expect_identical(seed2, globalenv()$.Random.seed)
  }
  )
})

test_that("ssd_get_seeds pass seed", {
 expect_snapshot(ssd_get_seeds(10, nseeds = 2L))
 expect_identical(ssd_get_seeds(10, nseeds = 2L), withr::with_seed(10, ssd_get_seeds(10, nseeds = 2L)))
})
