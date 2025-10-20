test_that("get_seeds_streams no streams or seeds", {
  expect_identical(get_seeds_streams(nstreams = 0L), list())
  expect_identical(get_seeds_streams(nseeds = 0L), list(list()))
  expect_identical(get_seeds_streams(nseeds = 0L, nstreams = 0L), list())
  expect_identical(get_seeds_streams(nseeds = 0L, nstreams = 2L), list(list(),list()))
})

test_that("get_seeds_streams streams and start_seed", {
  expect_snapshot(withr::with_seed(10, get_seeds_streams(nseeds = 2L)))
  expect_snapshot(withr::with_seed(10, get_seeds_streams(nseeds = 2L, nstreams = 2L)))
  expect_snapshot(withr::with_seed(10, get_seeds_streams(nseeds = 1L, nstreams = 2L, start_seed= 1L))) 
  expect_identical(withr::with_seed(10, get_seeds_streams(nseeds = 2L, nstreams = 2L))[[2]][2],
                   withr::with_seed(10, get_seeds_streams(nseeds = 1L, nstreams = 2L, start_seed= 2L))[[2]])
})

test_that("get_seeds_streams advances seed by 1", {
  withr::with_seed(10, {
    seed <- globalenv()$.Random.seed
    expect_snapshot(globalenv()$.Random.seed)
    get_seeds_streams(nseeds = 2L)
    seed2 <- globalenv()$.Random.seed 
    expect_snapshot(globalenv()$.Random.seed)
    set.seed(10)
    expect_identical(seed, globalenv()$.Random.seed)
    runif(1)
    expect_identical(seed2, globalenv()$.Random.seed)
  }
  )
})

test_that("get_seeds_streams pass seed", {
 expect_snapshot(get_seeds_streams(10, nseeds = 2L))
 expect_identical(get_seeds_streams(10, nseeds = 2L), withr::with_seed(10, get_seeds_streams(10, nseeds = 2L)))
})
