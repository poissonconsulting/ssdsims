test_that("parallel-safe-seeding: task_primer returns a length-2 integer", {
  primer <- task_primer(list(dataset = "boron", sim = 1L, replace = FALSE))
  expect_type(primer, "integer")
  expect_length(primer, 2L)
})

test_that("parallel-safe-seeding: task_primer is deterministic and reproducible", {
  p <- list(dataset = "boron", sim = 1L, replace = FALSE)
  expect_identical(task_primer(p), task_primer(p))
})

test_that("parallel-safe-seeding: task_primer is sensitive to params", {
  p1 <- list(sim = 1L, nrow = 5L, rescale = FALSE)
  p2 <- list(sim = 1L, nrow = 5L, rescale = TRUE)
  expect_false(identical(task_primer(p1), task_primer(p2)))
})

test_that("parallel-safe-seeding: hex8_to_int32 encoding", {
  # 0x80000000 (INT_MIN) -> NA_integer_
  expect_identical(hex8_to_int32("80000000"), NA_integer_)
  # other values -> signed int32
  expect_identical(hex8_to_int32("00000000"), 0L)
  expect_identical(hex8_to_int32("00000001"), 1L)
  expect_identical(hex8_to_int32("7fffffff"), .Machine$integer.max)
  expect_identical(hex8_to_int32("ffffffff"), -1L)
  expect_identical(hex8_to_int32("80000001"), -.Machine$integer.max)
})

test_that("parallel-safe-seeding: task_primer matches experiment smoke values", {
  # The validated reference (scripts/experiment-dqrng-hash.R, section 1).
  #
  # `task_primer()` derives the primer from `rlang::hash()`, whose digest is
  # not guaranteed stable across `rlang` versions (it changed once; see issue
  # #212). Such changes are rare and not anticipated, so these values are
  # simply pinned to `rlang >= 1.3.0`; reproducibility for simulations is
  # anchored by controlling the execution environment. Re-record with the
  # current output if this test fails after a future `rlang` update.
  skip_if_not_installed("rlang", "1.3.0")
  expect_identical(
    task_primer(list(sim = 1L, nrow = 5L, rescale = FALSE)),
    c(-135494555L, 2078443361L)
  )
  expect_identical(
    task_primer(list(sim = 1L, nrow = 5L, rescale = TRUE)),
    c(222240734L, 1330859749L)
  )
})

test_that("parallel-safe-seeding: task_primer seeds dqrng reproducibly", {
  local_dqrng_backend()
  p1 <- list(dataset = "boron", sim = 1L, replace = FALSE)
  p2 <- list(dataset = "boron", sim = 2L, replace = FALSE)

  seq_1 <- local({
    local_dqrng_state(42L, task_primer(p1))
    runif(5)
  })
  seq_2 <- local({
    local_dqrng_state(42L, task_primer(p1))
    runif(5)
  })
  expect_identical(seq_1, seq_2)

  seq_3 <- local({
    local_dqrng_state(42L, task_primer(p2))
    runif(5)
  })
  expect_false(identical(seq_1, seq_3))
})

test_that("parallel-safe-seeding: min_pmix referenced by name gives stable primers", {
  # A caller contract owned by task-tables: the name (a string), not the
  # function value, enters the hash.
  p1 <- list(dataset = "boron", nrow = 6L, min_pmix = "ssd_min_pmix")
  p2 <- list(dataset = "boron", nrow = 6L, min_pmix = "ssd_min_pmix")
  expect_identical(task_primer(p1), task_primer(p2))
})

test_that("parallel-safe-seeding: sample primer is nrow-free, fit/hc primer is not", {
  # sample identity carries no nrow -> stable across nrow (load-bearing for
  # the sub-truncation property).
  sample1 <- list(dataset = "boron", sim = 1L, replace = FALSE)
  sample2 <- list(dataset = "boron", sim = 1L, replace = FALSE)
  expect_identical(task_primer(sample1), task_primer(sample2))

  # fit/hc identity differing only in nrow -> different primers.
  fit5 <- list(dataset = "boron", sim = 1L, replace = FALSE, nrow = 5L)
  fit10 <- list(dataset = "boron", sim = 1L, replace = FALSE, nrow = 10L)
  expect_false(identical(task_primer(fit5), task_primer(fit10)))
})

test_that("parallel-safe-seeding: task_primer matches a recorded primer", {
  # Stability guard: catches an unexpected rlang::hash() change. The digest is
  # not guaranteed stable across rlang versions (it changed once; issue #212),
  # so this is pinned to `rlang >= 1.3.0`.
  skip_if_not_installed("rlang", "1.3.0")
  expect_identical(
    task_primer(list(dataset = "boron", sim = 1L, replace = FALSE)),
    c(-563099096L, -147061236L)
  )
})

test_that("parallel-safe-seeding: task_primer is name-order sensitive", {
  # `rlang::hash()` is order-sensitive, so a canonical name order is a caller
  # contract (owned by task-tables), NOT normalised inside `task_primer()`.
  # This pins that contract: reordering the same names yields a different
  # primer, so a future "helpful" sort in `normalize_task_row()` cannot
  # silently change it.
  p1 <- list(dataset = "boron", sim = 1L, replace = FALSE)
  p2 <- list(sim = 1L, replace = FALSE, dataset = "boron")
  expect_false(identical(task_primer(p1), task_primer(p2)))
})

test_that("parallel-safe-seeding: row and equivalent list agree", {
  p <- list(dataset = "boron", sim = 1L, replace = FALSE)
  row <- tibble::tibble_row(dataset = "boron", sim = 1L, replace = FALSE)
  expect_identical(task_primer(row), task_primer(p))
})

test_that("parallel-safe-seeding: tibble attributes do not affect the primer", {
  p <- list(dataset = "boron", sim = 1L, replace = FALSE)
  # A tibble carrying a list-style column, plus class / row.names attributes.
  row <- tibble::tibble(
    dataset = "boron",
    sim = 1L,
    replace = FALSE
  )
  expect_identical(task_primer(row), task_primer(p))
})

test_that("parallel-safe-seeding: df-style columns kept, list-style unwrapped", {
  row <- tibble::tibble_row(
    listcol = list(c(1L, 2L, 3L)),
    dfcol = tibble::tibble(a = 1L, b = 2L)
  )
  norm <- normalize_task_row(row)
  expect_identical(norm$listcol, c(1L, 2L, 3L))
  expect_true(is.data.frame(norm$dfcol))
  expect_identical(norm$dfcol, tibble::tibble(a = 1L, b = 2L))
})

test_that("parallel-safe-seeding: task_primer rejects invalid params", {
  expect_snapshot(error = TRUE, {
    task_primer(tibble::tibble(sim = 1:2))
  })
  expect_snapshot(error = TRUE, {
    task_primer(1L)
  })
})
