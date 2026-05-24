test_that("get_lecuyer_cmrg_stream_states no seeds", {
  expect_identical(
    get_lecuyer_cmrg_stream_states(
      seed = NULL,
      nsim = 0L,
      stream = 1L,
      start_sim = 1L
    ),
    list()
  )
})

test_that("get_lecuyer_cmrg_stream_states repeatable", {
  expect_snapshot(with_lecuyer_cmrg_seed(
    10,
    get_lecuyer_cmrg_stream_states(
      seed = NULL,
      nsim = 1L,
      stream = 1L,
      start_sim = 1L
    )
  ))
  expect_identical(
    with_lecuyer_cmrg_seed(
      10,
      get_lecuyer_cmrg_stream_states(
        seed = NULL,
        nsim = 1L,
        stream = 1L,
        start_sim = 1L
      )
    ),
    with_lecuyer_cmrg_seed(
      10,
      get_lecuyer_cmrg_stream_states(
        seed = NULL,
        nsim = 1L,
        stream = 1L,
        start_sim = 1L
      )
    )
  )
})

test_that("get_lecuyer_cmrg_stream_states seed fast enough", {
  expect_snapshot(with_lecuyer_cmrg_seed(
    10,
    get_lecuyer_cmrg_stream_states(
      seed = NULL,
      nsim = 1L,
      stream = 1L,
      start_sim = 10^5
    )
  ))
})

test_that("get_lecuyer_cmrg_stream_states stream fast enough", {
  expect_snapshot(with_lecuyer_cmrg_seed(
    10,
    get_lecuyer_cmrg_stream_states(
      seed = NULL,
      nsim = 1L,
      start_sim = 1L,
      stream = 10^5
    )
  ))
})

test_that("get_lecuyer_cmrg_stream_states seed stream fast enough", {
  expect_snapshot(with_lecuyer_cmrg_seed(
    10,
    get_lecuyer_cmrg_stream_states(
      seed = NULL,
      nsim = 1L,
      start_sim = 10^5,
      stream = 10^5
    )
  ))
})

test_that("get_lecuyer_cmrg_stream_states seeds stream fast enough", {
  seeds <- with_lecuyer_cmrg_seed(
    10,
    get_lecuyer_cmrg_stream_states(
      seed = NULL,
      start_sim = 10^5,
      stream = 10^5,
      nsim = 10^5
    )
  )
  expect_length(seeds, 10^5)
  expect_snapshot(seeds[[10^5]])
})

test_that("get_lecuyer_cmrg_stream_states repeatable multiple seeds", {
  expect_snapshot(with_lecuyer_cmrg_seed(
    10,
    get_lecuyer_cmrg_stream_states(
      seed = NULL,
      nsim = 2L,
      stream = 1L,
      start_sim = 1L
    )
  ))
  expect_identical(
    with_lecuyer_cmrg_seed(
      10,
      get_lecuyer_cmrg_stream_states(
        seed = NULL,
        nsim = 2L,
        stream = 1L,
        start_sim = 1L
      )
    )[2],
    with_lecuyer_cmrg_seed(
      10,
      get_lecuyer_cmrg_stream_states(
        seed = NULL,
        nsim = 1L,
        start_sim = 2L,
        stream = 1L
      )
    )
  )
})

test_that("get_lecuyer_cmrg_stream_states differs multiple seeds", {
  expect_false(identical(
    with_lecuyer_cmrg_seed(
      10,
      get_lecuyer_cmrg_stream_states(
        seed = NULL,
        nsim = 1L,
        stream = 1L,
        start_sim = 1L
      )
    ),
    with_lecuyer_cmrg_seed(
      10,
      get_lecuyer_cmrg_stream_states(
        seed = NULL,
        start_sim = 2L,
        nsim = 1L,
        stream = 1L
      )
    )
  ))
})

test_that("get_lecuyer_cmrg_stream_states repeatable other starts", {
  expect_snapshot(with_lecuyer_cmrg_seed(
    42,
    get_lecuyer_cmrg_stream_states(
      seed = NULL,
      nsim = 1L,
      stream = 1L,
      start_sim = 1L
    )
  ))
  expect_identical(
    with_lecuyer_cmrg_seed(
      42,
      get_lecuyer_cmrg_stream_states(
        seed = NULL,
        nsim = 1L,
        stream = 1L,
        start_sim = 1L
      )
    ),
    with_lecuyer_cmrg_seed(
      42,
      get_lecuyer_cmrg_stream_states(
        seed = NULL,
        nsim = 1L,
        stream = 1L,
        start_sim = 1L
      )
    )
  )
})

test_that("get_lecuyer_cmrg_stream_states differs other starts seeds", {
  expect_false(identical(
    with_lecuyer_cmrg_seed(
      10,
      get_lecuyer_cmrg_stream_states(
        seed = NULL,
        nsim = 1L,
        stream = 1L,
        start_sim = 1L
      )
    ),
    with_lecuyer_cmrg_seed(
      42,
      get_lecuyer_cmrg_stream_states(
        seed = NULL,
        nsim = 1L,
        stream = 1L,
        start_sim = 1L
      )
    )
  ))
})

test_that("get_lecuyer_cmrg_stream_states seeds repeatable with other seed types", {
  expect_snapshot(with_lecuyer_cmrg_seed(
    10,
    get_lecuyer_cmrg_stream_states(
      seed = NULL,
      nsim = 1L,
      stream = 1L,
      start_sim = 1L
    )
  ))
  expect_identical(
    with_lecuyer_cmrg_seed(
      10,
      get_lecuyer_cmrg_stream_states(
        seed = NULL,
        nsim = 1L,
        stream = 1L,
        start_sim = 1L
      )
    ),
    with_lecuyer_cmrg_seed(
      10,
      get_lecuyer_cmrg_stream_states(
        seed = NULL,
        nsim = 1L,
        stream = 1L,
        start_sim = 1L
      )
    )
  )
})

test_that("get_lecuyer_cmrg_stream_states seeds differ with other seed types", {
  expect_false(identical(
    withr::with_seed(
      10,
      get_lecuyer_cmrg_stream_states(
        seed = NULL,
        nsim = 1L,
        stream = 1L,
        start_sim = 1L
      ),
      .rng_kind = "Mersenne-Twister"
    ),
    with_lecuyer_cmrg_seed(
      10,
      get_lecuyer_cmrg_stream_states(
        seed = NULL,
        nsim = 1L,
        stream = 1L,
        start_sim = 1L
      )
    )
  ))
})

test_that("get_lecuyer_cmrg_stream_states passses seed", {
  expect_identical(
    with_lecuyer_cmrg_seed(
      37,
      get_lecuyer_cmrg_stream_states(10, nsim = 1L, stream = 1L, start_sim = 1L)
    ),
    with_lecuyer_cmrg_seed(
      42,
      get_lecuyer_cmrg_stream_states(10, nsim = 1L, stream = 1L, start_sim = 1L)
    )
  )
})

test_that("get_lecuyer_cmrg_stream_states does not advance seed", {
  with_lecuyer_cmrg_seed(10, {
    seed <- globalenv()$.Random.seed
    get_lecuyer_cmrg_stream_states(
      seed = NULL,
      nsim = 100L,
      stream = 1L,
      start_sim = 1L
    )
    expect_identical(globalenv()$.Random.seed, seed)
  })
})

test_that("get_lecuyer_cmrg_stream_states local withr work", {
  local_lecuyer_cmrg_seed(10)
  seed <- get_lecuyer_cmrg_stream_states(
    seed = NULL,
    nsim = 1L,
    stream = 1L,
    start_sim = 1L
  )
  with_lecuyer_cmrg_seed(10, {
    expect_identical(
      seed,
      get_lecuyer_cmrg_stream_states(
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

test_that("get_lecuyer_cmrg_stream_states return values state different sub-streams", {
  states <- withr::with_seed(
    10,
    get_lecuyer_cmrg_stream_states(
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

test_that("get_lecuyer_cmrg_stream_states return values state different sub-streams (snapshot)", {
  states <- withr::with_seed(
    10,
    get_lecuyer_cmrg_stream_states(
      seed = NULL,
      nsim = 2L,
      stream = 1L,
      start_sim = 1L
    )
  )
  expect_snapshot(with_lecuyer_cmrg_state(states[[1]], runif(3)))
  expect_snapshot(with_lecuyer_cmrg_state(states[[2]], runif(3)))
})

test_that("get_lecuyer_cmrg_stream_state return values state different streams", {
  s1 <- withr::with_seed(
    10,
    get_lecuyer_cmrg_stream_state(seed = NULL, stream = 1L, start_sim = 1L)
  )
  s2 <- withr::with_seed(
    10,
    get_lecuyer_cmrg_stream_state(seed = NULL, stream = 2L, start_sim = 1L)
  )
  expect_false(identical(
    with_lecuyer_cmrg_state(s1, runif(3)),
    with_lecuyer_cmrg_state(s2, runif(3))
  ))
})

test_that("local_lecuyer_cmrg_state different states produce different outcomes", {
  states <- withr::with_seed(
    10,
    get_lecuyer_cmrg_stream_states(
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

test_that("local_lecuyer_cmrg_seed rejects non-scalar seed", {
  state <- withr::with_seed(
    10,
    get_lecuyer_cmrg_stream_state(seed = NULL, stream = 1L, start_sim = 1L)
  )
  expect_snapshot(local_lecuyer_cmrg_seed(state), error = TRUE)
  expect_snapshot(local_lecuyer_cmrg_seed(integer()), error = TRUE)
  expect_snapshot(local_lecuyer_cmrg_seed(NA_integer_), error = TRUE)
  expect_snapshot(local_lecuyer_cmrg_seed("10"), error = TRUE)
})

test_that("local_lecuyer_cmrg_seed rejects non-environment .local_envir", {
  expect_snapshot(
    local_lecuyer_cmrg_seed(10, .local_envir = "env"),
    error = TRUE
  )
})

test_that("local_lecuyer_cmrg_state rejects non-integer state", {
  expect_snapshot(local_lecuyer_cmrg_state(10), error = TRUE)
  expect_snapshot(local_lecuyer_cmrg_state(1:6), error = TRUE)
  expect_snapshot(
    local_lecuyer_cmrg_state(c(1L, 2L, 3L, 4L, 5L, 6L, NA_integer_)),
    error = TRUE
  )
  expect_snapshot(local_lecuyer_cmrg_state("a"), error = TRUE)
})

test_that("local_lecuyer_cmrg_state rejects non-environment .local_envir", {
  state <- withr::with_seed(
    10,
    get_lecuyer_cmrg_stream_state(seed = NULL, stream = 1L, start_sim = 1L)
  )
  expect_snapshot(
    local_lecuyer_cmrg_state(state, .local_envir = "env"),
    error = TRUE
  )
})
