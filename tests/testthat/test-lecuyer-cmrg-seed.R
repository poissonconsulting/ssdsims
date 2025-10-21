test_that("get_lecuyer_cmrg_seed_stream no seeds", {
  expect_identical(get_lecuyer_cmrg_seed_stream(nsim = 0L), list())
})

test_that("get_lecuyer_cmrg_seed_stream repeatable", {
  expect_snapshot(withr::with_seed(10, get_lecuyer_cmrg_seed_stream()))
  expect_identical(withr::with_seed(10, get_lecuyer_cmrg_seed_stream()),
                  withr::with_seed(10, get_lecuyer_cmrg_seed_stream()))
})

test_that("get_lecuyer_cmrg_seed_stream seed fast enough", {
  expect_snapshot(withr::with_seed(10, get_lecuyer_cmrg_seed_stream(start_sim = 10^5)))
})

test_that("get_lecuyer_cmrg_seed_stream stream fast enough", {
  expect_snapshot(withr::with_seed(10, get_lecuyer_cmrg_seed_stream(stream = 10^5)))
})

test_that("get_lecuyer_cmrg_seed_stream seed stream fast enough", {
  expect_snapshot(withr::with_seed(10, get_lecuyer_cmrg_seed_stream(start_sim = 10^5, stream = 10^5)))
})

test_that("get_lecuyer_cmrg_seed_stream seeds stream fast enough", {
  seeds <- withr::with_seed(10, get_lecuyer_cmrg_seed_stream(start_sim = 10^5, stream = 10^5, nsim = 10^5))
  expect_length(seeds, 10^5)
  expect_snapshot(seeds[[10^5]])
})

test_that("get_lecuyer_cmrg_seed_stream repeatable multiple seeds", {
  expect_snapshot(withr::with_seed(10, get_lecuyer_cmrg_seed_stream(nsim = 2L)))
  expect_identical(withr::with_seed(10, get_lecuyer_cmrg_seed_stream(nsim = 2L))[2],
                   withr::with_seed(10, get_lecuyer_cmrg_seed_stream(nsim = 1L, start_sim = 2L)))
})

test_that("get_lecuyer_cmrg_seed_stream differs multiple seeds", {
  expect_false(identical(withr::with_seed(10, get_lecuyer_cmrg_seed_stream()),
                  withr::with_seed(10, get_lecuyer_cmrg_seed_stream(start_sim = 2L))))
})

test_that("get_lecuyer_cmrg_seed_stream repeatable other starts", {
  expect_snapshot(withr::with_seed(42, get_lecuyer_cmrg_seed_stream()))
  expect_identical(withr::with_seed(42, get_lecuyer_cmrg_seed_stream()),
                  withr::with_seed(42, get_lecuyer_cmrg_seed_stream()))
})

test_that("get_lecuyer_cmrg_seed_stream differs other starts seeds", {
  expect_false(identical(withr::with_seed(10, get_lecuyer_cmrg_seed_stream()),
                  withr::with_seed(42, get_lecuyer_cmrg_seed_stream())))
})

test_that("get_lecuyer_cmrg_seed_stream seeds repeatable with other seed types", {
  expect_snapshot(with_lecuyer_cmrg_seed(10, get_lecuyer_cmrg_seed_stream()))
  expect_identical(with_lecuyer_cmrg_seed(10, get_lecuyer_cmrg_seed_stream()),
                  with_lecuyer_cmrg_seed(10, get_lecuyer_cmrg_seed_stream()))
})

test_that("get_lecuyer_cmrg_seed_stream seeds differ with other seed types", {
  expect_false(identical(withr::with_seed(10, get_lecuyer_cmrg_seed_stream(), .rng_kind = "Mersenne-Twister"),
                  with_lecuyer_cmrg_seed(10, get_lecuyer_cmrg_seed_stream())))
})

test_that("get_lecuyer_cmrg_seed_stream passses seed", {
 expect_identical(withr::with_seed(37, get_lecuyer_cmrg_seed_stream(10)), 
                  withr::with_seed(42, get_lecuyer_cmrg_seed_stream(10)))
})

test_that("get_lecuyer_cmrg_seed_stream does not advance seed", {
  withr::with_seed(10, {
    seed <- globalenv()$.Random.seed
    get_lecuyer_cmrg_seed_stream(nsim = 100L)
    expect_identical(globalenv()$.Random.seed, seed)
  }
  )
})

test_that("get_lecuyer_cmrg_seed_stream local withr work", {
  local_lecuyer_cmrg_seed(10)
  seed <- get_lecuyer_cmrg_seed_stream()
  with_lecuyer_cmrg_seed(10, {
    expect_identical(seed, get_lecuyer_cmrg_seed_stream())
})
})