test_that("get_lecuyer_cmrg_seeds_stream no seeds", {
  expect_identical(
    get_lecuyer_cmrg_seeds_stream(
      seed = NULL,
      nsim = 0L,
      stream = 1L,
      start_sim = 1L
    ),
    list()
  )
})

test_that("get_lecuyer_cmrg_seeds_stream repeatable", {
  expect_snapshot(withr::with_seed(
    10,
    get_lecuyer_cmrg_seeds_stream(
      seed = NULL,
      nsim = 1L,
      stream = 1L,
      start_sim = 1L
    )
  ))
  expect_identical(
    withr::with_seed(
      10,
      get_lecuyer_cmrg_seeds_stream(
        seed = NULL,
        nsim = 1L,
        stream = 1L,
        start_sim = 1L
      )
    ),
    withr::with_seed(
      10,
      get_lecuyer_cmrg_seeds_stream(
        seed = NULL,
        nsim = 1L,
        stream = 1L,
        start_sim = 1L
      )
    )
  )
})

test_that("get_lecuyer_cmrg_seeds_stream seed fast enough", {
  expect_snapshot(withr::with_seed(
    10,
    get_lecuyer_cmrg_seeds_stream(
      seed = NULL,
      nsim = 1L,
      stream = 1L,
      start_sim = 10^5
    )
  ))
})

test_that("get_lecuyer_cmrg_seeds_stream stream fast enough", {
  expect_snapshot(withr::with_seed(
    10,
    get_lecuyer_cmrg_seeds_stream(
      seed = NULL,
      nsim = 1L,
      start_sim = 1L,
      stream = 10^5
    )
  ))
})

test_that("get_lecuyer_cmrg_seeds_stream seed stream fast enough", {
  expect_snapshot(withr::with_seed(
    10,
    get_lecuyer_cmrg_seeds_stream(
      seed = NULL,
      nsim = 1L,
      start_sim = 10^5,
      stream = 10^5
    )
  ))
})

test_that("get_lecuyer_cmrg_seeds_stream seeds stream fast enough", {
  seeds <- withr::with_seed(
    10,
    get_lecuyer_cmrg_seeds_stream(
      seed = NULL,
      start_sim = 10^5,
      stream = 10^5,
      nsim = 10^5
    )
  )
  expect_length(seeds, 10^5)
  expect_snapshot(seeds[[10^5]])
})

test_that("get_lecuyer_cmrg_seeds_stream repeatable multiple seeds", {
  expect_snapshot(withr::with_seed(
    10,
    get_lecuyer_cmrg_seeds_stream(
      seed = NULL,
      nsim = 2L,
      stream = 1L,
      start_sim = 1L
    )
  ))
  expect_identical(
    withr::with_seed(
      10,
      get_lecuyer_cmrg_seeds_stream(
        seed = NULL,
        nsim = 2L,
        stream = 1L,
        start_sim = 1L
      )
    )[2],
    withr::with_seed(
      10,
      get_lecuyer_cmrg_seeds_stream(
        seed = NULL,
        nsim = 1L,
        start_sim = 2L,
        stream = 1L
      )
    )
  )
})

test_that("get_lecuyer_cmrg_seeds_stream differs multiple seeds", {
  expect_false(identical(
    withr::with_seed(
      10,
      get_lecuyer_cmrg_seeds_stream(
        seed = NULL,
        nsim = 1L,
        stream = 1L,
        start_sim = 1L
      )
    ),
    withr::with_seed(
      10,
      get_lecuyer_cmrg_seeds_stream(
        seed = NULL,
        start_sim = 2L,
        nsim = 1L,
        stream = 1L
      )
    )
  ))
})

test_that("get_lecuyer_cmrg_seeds_stream repeatable other starts", {
  expect_snapshot(withr::with_seed(
    42,
    get_lecuyer_cmrg_seeds_stream(
      seed = NULL,
      nsim = 1L,
      stream = 1L,
      start_sim = 1L
    )
  ))
  expect_identical(
    withr::with_seed(
      42,
      get_lecuyer_cmrg_seeds_stream(
        seed = NULL,
        nsim = 1L,
        stream = 1L,
        start_sim = 1L
      )
    ),
    withr::with_seed(
      42,
      get_lecuyer_cmrg_seeds_stream(
        seed = NULL,
        nsim = 1L,
        stream = 1L,
        start_sim = 1L
      )
    )
  )
})

test_that("get_lecuyer_cmrg_seeds_stream differs other starts seeds", {
  expect_false(identical(
    withr::with_seed(
      10,
      get_lecuyer_cmrg_seeds_stream(
        seed = NULL,
        nsim = 1L,
        stream = 1L,
        start_sim = 1L
      )
    ),
    withr::with_seed(
      42,
      get_lecuyer_cmrg_seeds_stream(
        seed = NULL,
        nsim = 1L,
        stream = 1L,
        start_sim = 1L
      )
    )
  ))
})

test_that("get_lecuyer_cmrg_seeds_stream seeds repeatable with other seed types", {
  expect_snapshot(with_lecuyer_cmrg_seed(
    10,
    get_lecuyer_cmrg_seeds_stream(
      seed = NULL,
      nsim = 1L,
      stream = 1L,
      start_sim = 1L
    )
  ))
  expect_identical(
    with_lecuyer_cmrg_seed(
      10,
      get_lecuyer_cmrg_seeds_stream(
        seed = NULL,
        nsim = 1L,
        stream = 1L,
        start_sim = 1L
      )
    ),
    with_lecuyer_cmrg_seed(
      10,
      get_lecuyer_cmrg_seeds_stream(
        seed = NULL,
        nsim = 1L,
        stream = 1L,
        start_sim = 1L
      )
    )
  )
})

test_that("get_lecuyer_cmrg_seeds_stream seeds differ with other seed types", {
  expect_false(identical(
    withr::with_seed(
      10,
      get_lecuyer_cmrg_seeds_stream(
        seed = NULL,
        nsim = 1L,
        stream = 1L,
        start_sim = 1L
      ),
      .rng_kind = "Mersenne-Twister"
    ),
    with_lecuyer_cmrg_seed(
      10,
      get_lecuyer_cmrg_seeds_stream(
        seed = NULL,
        nsim = 1L,
        stream = 1L,
        start_sim = 1L
      )
    )
  ))
})

test_that("get_lecuyer_cmrg_seeds_stream passses seed", {
  expect_identical(
    withr::with_seed(
      37,
      get_lecuyer_cmrg_seeds_stream(10, nsim = 1L, stream = 1L, start_sim = 1L)
    ),
    withr::with_seed(
      42,
      get_lecuyer_cmrg_seeds_stream(10, nsim = 1L, stream = 1L, start_sim = 1L)
    )
  )
})

test_that("get_lecuyer_cmrg_seeds_stream does not advance seed", {
  withr::with_seed(10, {
    seed <- globalenv()$.Random.seed
    get_lecuyer_cmrg_seeds_stream(
      seed = NULL,
      nsim = 100L,
      stream = 1L,
      start_sim = 1L
    )
    expect_identical(globalenv()$.Random.seed, seed)
  })
})

test_that("get_lecuyer_cmrg_seeds_stream local withr work", {
  local_lecuyer_cmrg_seed(10)
  seed <- get_lecuyer_cmrg_seeds_stream(
    seed = NULL,
    nsim = 1L,
    stream = 1L,
    start_sim = 1L
  )
  with_lecuyer_cmrg_seed(10, {
    expect_identical(
      seed,
      get_lecuyer_cmrg_seeds_stream(
        seed = NULL,
        nsim = 1L,
        stream = 1L,
        start_sim = 1L
      )
    )
  })
})

gen_lecuyer_runif <- function(seed) {
  local_lecuyer_cmrg_seed(seed)
  runif(3)
}

test_that("local_lecuyer_cmrg_seed different seeds produce different outcomes", {
  expect_false(identical(gen_lecuyer_runif(10), gen_lecuyer_runif(42)))
})

test_that("local_lecuyer_cmrg_seed different seeds produce different outcomes (snapshot)", {
  expect_snapshot(gen_lecuyer_runif(10))
  expect_snapshot(gen_lecuyer_runif(42))
})

test_that("with_lecuyer_cmrg_seed different seeds produce different outcomes", {
  expect_identical(
    with_lecuyer_cmrg_seed(10, runif(3)),
    gen_lecuyer_runif(10)
  )
  expect_false(identical(
    with_lecuyer_cmrg_seed(10, runif(3)),
    with_lecuyer_cmrg_seed(42, runif(3))
  ))
})

test_that("with_lecuyer_cmrg_seed different seeds produce different outcomes (snapshot)", {
  expect_snapshot(with_lecuyer_cmrg_seed(10, runif(3)))
  expect_snapshot(with_lecuyer_cmrg_seed(42, runif(3)))
})

test_that("get_lecuyer_cmrg_seeds_stream return values state different sub-streams", {
  states <- withr::with_seed(
    10,
    get_lecuyer_cmrg_seeds_stream(
      seed = NULL,
      nsim = 2L,
      stream = 1L,
      start_sim = 1L
    )
  )
  expect_false(identical(
    with_lecuyer_cmrg_state(states[[1]], runif(3)),
    with_lecuyer_cmrg_state(states[[2]], runif(3))
  ))
})

test_that("get_lecuyer_cmrg_seeds_stream return values state different sub-streams (snapshot)", {
  states <- withr::with_seed(
    10,
    get_lecuyer_cmrg_seeds_stream(
      seed = NULL,
      nsim = 2L,
      stream = 1L,
      start_sim = 1L
    )
  )
  expect_snapshot(with_lecuyer_cmrg_state(states[[1]], runif(3)))
  expect_snapshot(with_lecuyer_cmrg_state(states[[2]], runif(3)))
})

test_that("get_lecuyer_cmrg_seed_stream return values state different streams", {
  s1 <- withr::with_seed(
    10,
    get_lecuyer_cmrg_seed_stream(seed = NULL, stream = 1L, start_sim = 1L)
  )
  s2 <- withr::with_seed(
    10,
    get_lecuyer_cmrg_seed_stream(seed = NULL, stream = 2L, start_sim = 1L)
  )
  expect_false(identical(
    with_lecuyer_cmrg_state(s1, runif(3)),
    with_lecuyer_cmrg_state(s2, runif(3))
  ))
})

test_that("local_lecuyer_cmrg_state different states produce different outcomes", {
  states <- withr::with_seed(
    10,
    get_lecuyer_cmrg_seeds_stream(
      seed = NULL,
      nsim = 2L,
      stream = 1L,
      start_sim = 1L
    )
  )
  gen <- function(state) {
    local_lecuyer_cmrg_state(state)
    runif(3)
  }
  expect_false(identical(gen(states[[1]]), gen(states[[2]])))
})
