# ---- minimal construction & declarative-only fields ------------------------

test_that("scenario-definition: minimal construction stores declarative fields", {
  s <- ssd_define_scenario(
    ssd_scenario_data(ssddata::ccme_boron),
    nsim = 100L,
    seed = 42L,
    nrow = c(5L, 10L)
  )
  expect_s3_class(s, "ssdsims_scenario")
  expect_identical(s$seed, 42L)
  expect_identical(s$nsim, 100L)
  expect_identical(s$nrow, c(5L, 10L))
  expect_identical(s$datasets, "ccme_boron")
  expect_named(
    s$fit,
    c(
      "rescale",
      "computable",
      "at_boundary_ok",
      "min_pmix",
      "range_shape1",
      "range_shape2",
      "dists"
    )
  )
  expect_named(
    s$hc,
    c(
      "est_method",
      "proportion",
      "ci",
      "nboot",
      "ci_method",
      "parametric",
      "samples",
      "distsets"
    )
  )
  expect_identical(s$hc$ci, FALSE)
  expect_identical(s$nrow_max, 1000L)
  # The default `dists` is a single-set `ssd_distset()`; the fit stores the
  # union and the hc stores the named sets.
  expect_identical(names(s$hc$distsets), "BCANZ")
  expect_identical(s$fit$dists, sort(ssdtools::ssd_dists_bcanz()))
})

# ---- nrow_max (sample-level draw-size setting) -------------------------------

test_that("scenario-definition: nrow_max defaults to 1000L and is a setting, not an axis", {
  s <- ssd_define_scenario(
    ssd_scenario_data(ssddata::ccme_boron),
    nsim = 2L,
    seed = 1L
  )
  expect_identical(s$nrow_max, 1000L)
  expect_false("nrow_max" %in% task_axes("sample"))
})

test_that("scenario-definition: nrow_max must be a whole number", {
  expect_snapshot(error = TRUE, {
    ssd_define_scenario(
      ssd_scenario_data(ssddata::ccme_boron),
      nsim = 2L,
      seed = 1L,
      nrow_max = 10.5
    )
  })
})

test_that("scenario-definition: nrow exceeding nrow_max errors, citing nrow_max", {
  # `nrow_max` is the fixed draw size and so the universal `nrow` ceiling: an
  # `nrow` above it can sub-truncate no draw, regardless of `replace`. The
  # error cites `nrow_max`'s value so a raised ceiling is discoverable.
  expect_snapshot(error = TRUE, {
    ssd_define_scenario(
      ssd_scenario_data(ssddata::ccme_boron),
      nsim = 2L,
      seed = 1L,
      nrow = 50L,
      nrow_max = 20L
    )
  })
  # The user's report: a high `nrow` is accepted once `nrow_max` admits it.
  expect_s3_class(
    ssd_define_scenario(
      ssd_scenario_data(ssddata::ccme_boron),
      nsim = 1L,
      seed = 42L,
      nrow = c(10L, 10000L),
      nrow_max = 10000L,
      replace = c(TRUE, FALSE)
    ),
    "ssdsims_scenario"
  )
})

test_that("scenario-definition: the default replace = TRUE frees nrow from the dataset size", {
  # ccme_boron has 28 rows; with the default (replace = TRUE) the draw is
  # nrow_max rows, so nrow may exceed the dataset size.
  s <- ssd_define_scenario(
    ssd_scenario_data(ssddata::ccme_boron),
    nsim = 2L,
    seed = 1L,
    nrow = 50L
  )
  expect_identical(s$replace, TRUE)
  expect_identical(unique(ssd_scenario_sample_tasks(s)$replace), TRUE)
})

test_that("scenario-definition: an infeasible replace = FALSE nrow is discarded, feasible ones kept", {
  # ccme_boron has 28 rows; under replace = FALSE the permutation cap is 28, so
  # nrow = 30 is silently discarded while nrow = 10 survives - construction
  # succeeds and the fit grid carries only the feasible nrow.
  s <- ssd_define_scenario(
    ssd_scenario_data(ssddata::ccme_boron),
    nsim = 1L,
    seed = 1L,
    nrow = c(10L, 30L),
    replace = FALSE
  )
  fit <- ssd_scenario_fit_tasks(s)
  expect_setequal(fit$nrow, 10L)
  expect_identical(unique(fit$replace), FALSE)
})

test_that("scenario-definition: mixed replace discards an nrow infeasible for the FALSE draw", {
  # boron has 28 rows; nrow = 50 > 28, so the replace = FALSE cell at nrow = 50
  # is silently discarded (no abort, no warning) while the replace = TRUE cell
  # (capped at nrow_max) survives.
  expect_no_warning(
    s <- ssd_define_scenario(
      ssd_scenario_data(ssddata::ccme_boron),
      nsim = 1L,
      seed = 1L,
      nrow = 50L,
      replace = c(FALSE, TRUE)
    )
  )
  combos <- unique(ssd_scenario_fit_tasks(s)[c("replace", "nrow")])
  expect_true(any(combos$replace & combos$nrow == 50L))
  expect_false(any(!combos$replace & combos$nrow == 50L))
})

test_that("scenario-definition: the replace = FALSE discard is per dataset", {
  # `big` (40 rows) keeps its replace = FALSE cell at nrow = 35; `small`
  # (20 rows) drops it. Both keep their replace = TRUE cell.
  s <- ssd_define_scenario(
    ssd_scenario_data(
      big = data.frame(Conc = exp(seq(-1, 2, length.out = 40))),
      small = data.frame(Conc = exp(seq(-1, 2, length.out = 20)))
    ),
    nsim = 1L,
    seed = 1L,
    nrow = 35L,
    replace = c(FALSE, TRUE)
  )
  fit <- ssd_scenario_fit_tasks(s)
  expect_setequal(fit$dataset[!fit$replace & fit$nrow == 35L], "big")
  expect_setequal(fit$dataset[fit$replace & fit$nrow == 35L], c("big", "small"))
})

test_that("scenario-definition: the fit task table excludes the infeasible nrow+FALSE cell end to end", {
  # ccme_boron has 28 rows. With nrow = c(10, 20, 30) and replace = c(TRUE,
  # FALSE), every (nrow, replace) cell is feasible except (30, FALSE) - 30 > 28
  # so the permutation draw cannot supply it. The fit task table must carry the
  # full cross-join minus exactly that one cell.
  stopifnot(nrow(ssddata::ccme_boron) == 28L)
  s <- ssd_define_scenario(
    ssd_scenario_data(ssddata::ccme_boron),
    nsim = 1L,
    seed = 42L,
    nrow = c(10L, 20L, 30L),
    replace = c(TRUE, FALSE)
  )
  combos <- unique(ssd_scenario_fit_tasks(s)[c("nrow", "replace")])
  key <- paste(combos$nrow, combos$replace)
  # Full 3x2 cross-join minus exactly (nrow = 30, replace = FALSE).
  expect_setequal(
    key,
    c("10 TRUE", "20 TRUE", "30 TRUE", "10 FALSE", "20 FALSE")
  )
  expect_false("30 FALSE" %in% key)
})

test_that("scenario-definition: an all-infeasible replace = FALSE grid aborts", {
  # replace = FALSE only, every nrow above every dataset's cap -> empty grid,
  # so the constructor aborts rather than returning a scenario that produces
  # nothing.
  expect_snapshot(error = TRUE, {
    ssd_define_scenario(
      ssd_scenario_data(ssddata::ccme_boron),
      nsim = 1L,
      seed = 1L,
      nrow = 50L,
      replace = FALSE
    )
  })
})

test_that("scenario-definition: nrow at the effective draw size is accepted", {
  expect_s3_class(
    ssd_define_scenario(
      ssd_scenario_data(ssddata::ccme_boron),
      nsim = 2L,
      seed = 1L,
      nrow = 20L,
      replace = TRUE,
      nrow_max = 20L
    ),
    "ssdsims_scenario"
  )
  expect_s3_class(
    ssd_define_scenario(
      ssd_scenario_data(ssddata::ccme_boron),
      nsim = 2L,
      seed = 1L,
      nrow = nrow(ssddata::ccme_boron),
      replace = FALSE
    ),
    "ssdsims_scenario"
  )
})

test_that("scenario-definition: non-ci-gated settings precede ci, gated knobs follow", {
  fmls <- names(formals(ssd_define_scenario))
  # The non-`ci`-gated settings (`nrow_max`, `dists`, `est_method`, `proportion`
  # — all valid and meaningful when `ci = FALSE`, led by the sample-level
  # `nrow_max`) come first, then `ci`, then the knobs it gates: the bootstrap
  # axes `nboot`/`ci_method`/`parametric` (rejected when `ci = FALSE`) and
  # `samples` (only retains bootstrap draws). Contiguous, after the last
  # structural axis (`range_shape2`) and before the partitioning args.
  group <- c(
    "nrow_max",
    "dists",
    "est_method",
    "proportion",
    "ci",
    "nboot",
    "ci_method",
    "parametric",
    "samples"
  )
  idx <- match(group, fmls)
  expect_identical(idx, seq(idx[[1L]], length.out = length(group)))
  expect_identical(fmls[[idx[[1L]] - 1L]], "range_shape2")
  expect_identical(fmls[[max(idx) + 1L]], "partition_by")
})

test_that("scenario-definition: stores dataset names, not data frames", {
  s <- ssd_define_scenario(
    ssd_scenario_data(ssddata::ccme_boron),
    nsim = 2L,
    seed = 1L
  )
  expect_type(s$datasets, "character")
  # no element of the object is a data frame
  expect_false(any(vapply(s, is.data.frame, logical(1))))
  expect_false(any(vapply(s$fit, is.data.frame, logical(1))))
})

test_that("scenario-definition: stores min_pmix by name, not as a function", {
  s <- ssd_define_scenario(
    ssd_scenario_data(ssddata::ccme_boron),
    nsim = 2L,
    seed = 1L
  )
  expect_type(s$fit$min_pmix, "character")
  expect_identical(s$fit$min_pmix, "ssd_min_pmix")
  # no function bodies stored anywhere in the fit grid
  expect_false(any(vapply(s$fit, is.function, logical(1))))
})

test_that("scenario-definition: min_pmix accepts an ssd_pmix() collection", {
  default <- function(n) 0.05
  strict <- function(n) 0.1
  s <- ssd_define_scenario(
    ssd_scenario_data(ssddata::ccme_boron),
    nsim = 2L,
    seed = 1L,
    min_pmix = ssd_pmix(default = default, strict = strict)
  )
  expect_identical(s$fit$min_pmix, c("default", "strict"))
  expect_identical(s$min_pmix_fns, list(default = default, strict = strict))
})

test_that("scenario-definition: a supplied ssd_pmix() function is materialised under its name", {
  my_fun <- function(n) 0.05
  s <- ssd_define_scenario(
    ssd_scenario_data(ssddata::ccme_boron),
    nsim = 2L,
    seed = 1L,
    min_pmix = ssd_pmix(my_fun = my_fun)
  )
  expect_identical(s$fit$min_pmix, "my_fun")
  expect_identical(s$min_pmix_fns, list(my_fun = my_fun))
})

test_that("scenario-definition: min_pmix rejects a bare function", {
  data <- ssd_scenario_data(ssddata::ccme_boron)
  expect_snapshot(error = TRUE, {
    ssd_define_scenario(
      data,
      nsim = 2L,
      seed = 1L,
      min_pmix = ssdtools::ssd_min_pmix
    )
  })
})

test_that("scenario-definition: min_pmix rejects a plain list", {
  data <- ssd_scenario_data(ssddata::ccme_boron)
  expect_snapshot(error = TRUE, {
    ssd_define_scenario(
      data,
      nsim = 2L,
      seed = 1L,
      min_pmix = list(ssdtools::ssd_min_pmix)
    )
  })
})

test_that("scenario-definition: min_pmix rejects a character vector of names", {
  data <- ssd_scenario_data(ssddata::ccme_boron)
  expect_snapshot(error = TRUE, {
    ssd_define_scenario(data, nsim = 2L, seed = 1L, min_pmix = "ssd_min_pmix")
  })
})

test_that("scenario-definition: an indirectly-supplied list value still aborts cleanly", {
  # The form the old expression-inference path mishandled; it now aborts with the
  # same actionable `ssd_pmix()` message, not an obscure internal frame.
  data <- ssd_scenario_data(ssddata::ccme_boron)
  fns <- list(ssdtools::ssd_min_pmix)
  expect_snapshot(error = TRUE, {
    ssd_define_scenario(data, nsim = 2L, seed = 1L, min_pmix = fns)
  })
})

test_that("scenario-accessors: materialisation does not change fit-task primers", {
  # Same min_pmix name, functions with different bodies -> identical primers.
  f_a <- function(n) 0.05
  f_b <- function(n) 0.10
  s_a <- ssd_define_scenario(
    ssd_scenario_data(ssddata::ccme_boron),
    nsim = 1L,
    seed = 1L,
    nrow = 6L,
    min_pmix = ssd_pmix(shared = f_a)
  )
  s_b <- ssd_define_scenario(
    ssd_scenario_data(ssddata::ccme_boron),
    nsim = 1L,
    seed = 1L,
    nrow = 6L,
    min_pmix = ssd_pmix(shared = f_b)
  )
  # The stored functions differ, but the name (the identity surface) does not.
  expect_false(identical(s_a$min_pmix_fns, s_b$min_pmix_fns))
  expect_identical(s_a$fit$min_pmix, s_b$fit$min_pmix)
  fit_a <- ssd_scenario_fit_tasks(s_a)
  fit_b <- ssd_scenario_fit_tasks(s_b)
  expect_identical(
    task_primer(fit_a[1, task_axes("fit")]),
    task_primer(fit_b[1, task_axes("fit")])
  )
})

test_that("scenario-definition: partition_by three-step defaults are populated", {
  s <- ssd_define_scenario(
    ssd_scenario_data(ssddata::ccme_boron),
    nsim = 2L,
    seed = 1L
  )
  expect_identical(
    s$partition_by,
    list(
      sample = c("dataset", "sim", "replace"),
      fit = c("dataset", "sim", "nrow", "rescale"),
      hc = c("dataset", "sim")
    )
  )
})

test_that("scenario-definition: a valid partition_by override is stored verbatim", {
  pb <- list(
    sample = c("dataset", "sim", "replace"),
    fit = c("dataset", "sim", "nrow", "rescale"),
    hc = c("dataset", "sim", "nrow")
  )
  s <- ssd_define_scenario(
    ssd_scenario_data(ssddata::ccme_boron),
    nsim = 2L,
    seed = 1L,
    partition_by = pb
  )
  expect_identical(s$partition_by, pb)
})

test_that("scenario-definition: a partial partition_by defaults the unnamed steps", {
  s <- ssd_define_scenario(
    ssd_scenario_data(ssddata::ccme_boron),
    nsim = 2L,
    seed = 1L,
    partition_by = list(fit = c("dataset", "sim"))
  )
  expect_identical(
    s$partition_by,
    list(
      sample = c("dataset", "sim", "replace"),
      fit = c("dataset", "sim"),
      hc = c("dataset", "sim")
    )
  )
})

test_that("scenario-definition: partition_by rejects an unknown axis", {
  expect_snapshot(error = TRUE, {
    ssd_define_scenario(
      ssd_scenario_data(ssddata::ccme_boron),
      nsim = 2L,
      seed = 1L,
      partition_by = list(
        sample = c("dataset", "nboot"),
        fit = c("dataset", "sim"),
        hc = c("dataset", "sim")
      )
    )
  })
})

test_that("scenario-definition: partition_by rejects ci as an hc axis", {
  expect_snapshot(error = TRUE, {
    ssd_define_scenario(
      ssd_scenario_data(ssddata::ccme_boron),
      nsim = 2L,
      seed = 1L,
      partition_by = list(hc = c("dataset", "sim", "ci"))
    )
  })
})

test_that("scenario-definition: bundle rejects ci as an hc axis", {
  expect_snapshot(error = TRUE, {
    ssd_define_scenario(
      ssd_scenario_data(ssddata::ccme_boron),
      nsim = 2L,
      seed = 1L,
      bundle = list(hc = "ci")
    )
  })
})

test_that("scenario-definition: partition_by rejects nrow under the sample step", {
  expect_snapshot(error = TRUE, {
    ssd_define_scenario(
      ssd_scenario_data(ssddata::ccme_boron),
      nsim = 2L,
      seed = 1L,
      partition_by = list(
        sample = c("dataset", "sim", "nrow"),
        fit = c("dataset", "sim", "nrow"),
        hc = c("dataset", "sim")
      )
    )
  })
})

test_that("scenario-definition: partition_by accepts nrow as a fit/hc path axis", {
  pb <- list(
    sample = c("dataset", "sim", "replace"),
    fit = c("dataset", "sim", "nrow"),
    hc = c("dataset", "sim", "nrow")
  )
  s <- ssd_define_scenario(
    ssd_scenario_data(ssddata::ccme_boron),
    nsim = 2L,
    seed = 1L,
    partition_by = pb
  )
  expect_identical(s$partition_by, pb)
})

test_that("scenario-definition: partition_by rejects duplicate or NA axis names", {
  expect_snapshot(error = TRUE, {
    ssd_define_scenario(
      ssd_scenario_data(ssddata::ccme_boron),
      nsim = 2L,
      seed = 1L,
      partition_by = list(
        sample = c("dataset", "dataset"),
        fit = c("dataset", "sim"),
        hc = c("dataset", "sim")
      )
    )
  })
  expect_snapshot(error = TRUE, {
    ssd_define_scenario(
      ssd_scenario_data(ssddata::ccme_boron),
      nsim = 2L,
      seed = 1L,
      partition_by = list(
        sample = c("dataset", NA_character_),
        fit = c("dataset", "sim"),
        hc = c("dataset", "sim")
      )
    )
  })
})

test_that("scenario-definition: a parent-inconsistent split is accepted (no cross-step check)", {
  # `fit` shards on `replace` but its parent `sample` does not - steps partition
  # independently; the m:n parent-shard relationship is resolved at the read
  # layer, so this is accepted.
  pb <- list(
    sample = c("dataset", "sim"),
    fit = c("dataset", "sim", "replace"),
    hc = c("dataset", "sim")
  )
  s <- ssd_define_scenario(
    ssd_scenario_data(ssddata::ccme_boron),
    nsim = 2L,
    seed = 1L,
    partition_by = pb
  )
  expect_identical(s$partition_by, pb)
})

test_that("scenario-definition: bundle normalises to the path complement", {
  s <- ssd_define_scenario(
    ssd_scenario_data(ssddata::ccme_boron),
    nsim = 2L,
    seed = 1L,
    bundle = list(
      fit = c(
        "computable",
        "at_boundary_ok",
        "min_pmix",
        "range_shape1",
        "range_shape2"
      )
    )
  )
  expect_identical(
    s$partition_by$fit,
    c("dataset", "sim", "replace", "nrow", "rescale")
  )
  # unnamed steps keep their defaults
  expect_identical(s$partition_by$hc, c("dataset", "sim"))
})

test_that("scenario-definition: partition_by and bundle mix across steps", {
  s <- ssd_define_scenario(
    ssd_scenario_data(ssddata::ccme_boron),
    nsim = 2L,
    seed = 1L,
    partition_by = list(sample = c("dataset", "sim")),
    bundle = list(fit = c("computable", "at_boundary_ok"))
  )
  expect_identical(s$partition_by$sample, c("dataset", "sim"))
  expect_identical(
    s$partition_by$fit,
    setdiff(task_axes("fit"), c("computable", "at_boundary_ok"))
  )
  expect_identical(s$partition_by$hc, c("dataset", "sim"))
})

test_that("scenario-definition: a step named in both partition_by and bundle errors", {
  expect_snapshot(error = TRUE, {
    ssd_define_scenario(
      ssd_scenario_data(ssddata::ccme_boron),
      nsim = 2L,
      seed = 1L,
      partition_by = list(fit = c("dataset", "sim")),
      bundle = list(fit = c("computable"))
    )
  })
})

test_that("scenario-definition: scenario_partition_axes splits path and inner", {
  s <- ssd_define_scenario(
    ssd_scenario_data(ssddata::ccme_boron),
    nsim = 2L,
    seed = 1L
  )
  # inner = complement of task_axes(step)
  fit_axes <- scenario_partition_axes(s, "fit")
  expect_identical(fit_axes$path, c("dataset", "sim", "nrow", "rescale"))
  expect_identical(
    fit_axes$inner,
    setdiff(task_axes("fit"), c("dataset", "sim", "nrow", "rescale"))
  )
  expect_identical(
    fit_axes$inner,
    c(
      "replace",
      "computable",
      "at_boundary_ok",
      "min_pmix",
      "range_shape1",
      "range_shape2"
    )
  )
  # vocabularies equal task_axes(step)
  for (step in c("sample", "fit", "hc")) {
    ax <- scenario_partition_axes(s, step)
    expect_identical(sort(c(ax$path, ax$inner)), sort(task_axes(step)))
  }
})

test_that("scenario-definition: all-axes-in-path yields no inner axes", {
  s <- ssd_define_scenario(
    ssd_scenario_data(ssddata::ccme_boron),
    nsim = 2L,
    seed = 1L,
    partition_by = list(
      sample = task_axes("sample"),
      fit = task_axes("fit"),
      hc = task_axes("hc")
    )
  )
  expect_length(scenario_partition_axes(s, "hc")$inner, 0L)
})

test_that("scenario-definition: no upload field on the scenario", {
  s <- ssd_define_scenario(
    ssd_scenario_data(ssddata::ccme_boron),
    nsim = 2L,
    seed = 1L
  )
  expect_false("upload" %in% names(s))
})

test_that("scenario-definition: construction leaves .Random.seed unchanged", {
  set.seed(101)
  before <- .Random.seed
  ssd_define_scenario(
    ssd_scenario_data(ssddata::ccme_boron),
    nsim = 2L,
    seed = 7L
  )
  expect_identical(before, .Random.seed)
})

test_that("scenario-definition: construction with a generator dataset leaves .Random.seed unchanged", {
  set.seed(101)
  before <- .Random.seed
  ssd_define_scenario(
    ssd_scenario_data(
      boron = ssddata::ccme_boron,
      ssd_gen(synth = ssdtools::ssd_rlnorm, .n = 10, .seed = 1L)
    ),
    nsim = 2L,
    seed = 7L
  )
  expect_identical(before, .Random.seed)
})

# ---- dataset input API -----------------------------------------------------

test_that("scenario-definition: accepts an ssd_scenario_data() collection", {
  s <- ssd_define_scenario(
    ssd_scenario_data(
      boron = ssddata::ccme_boron,
      cadmium = ssddata::ccme_cadmium
    ),
    nsim = 2L,
    seed = 1L
  )
  expect_identical(s$datasets, c("boron", "cadmium"))
})

test_that("scenario-definition: single dataset name derives via the collection", {
  s <- ssd_define_scenario(
    ssd_scenario_data(ssddata::ccme_boron),
    nsim = 2L,
    seed = 1L
  )
  expect_identical(s$datasets, "ccme_boron")
})

test_that("scenario-definition: accepts a collection with generator datasets", {
  s <- ssd_define_scenario(
    ssd_scenario_data(
      boron = ssddata::ccme_boron,
      ssd_gen(synth = ssdtools::ssd_rlnorm, .n = 30, .seed = 1L)
    ),
    nsim = 2L,
    seed = 1L
  )
  expect_identical(s$datasets, c("boron", "synth"))
  # The materialised generator is an ordinary tibble in `$data`,
  # indistinguishable from a data-frame dataset.
  expect_s3_class(s$data[["synth"]], "tbl_df")
  expect_identical(nrow(s$data[["synth"]]), 30L)
  expect_false(any(
    vapply(s$data, \(d) inherits(d, "ssdsims_gen"), logical(1))
  ))
})

test_that("scenario-definition: a bare data frame is rejected", {
  expect_snapshot(error = TRUE, {
    ssd_define_scenario(ssddata::ccme_boron, nsim = 2L, seed = 1L)
  })
})

test_that("scenario-definition: a bare list is rejected", {
  expect_snapshot(error = TRUE, {
    ssd_define_scenario(
      list(boron = ssddata::ccme_boron, cadmium = ssddata::ccme_cadmium),
      nsim = 2L,
      seed = 1L
    )
  })
})

test_that("scenario-definition: the dropped name= argument is rejected", {
  expect_snapshot(error = TRUE, {
    ssd_define_scenario(
      ssd_scenario_data(ssddata::ccme_boron),
      nsim = 2L,
      seed = 1L,
      name = "boron_data"
    )
  })
})

# ---- ci = FALSE rejects bootstrap-only knobs -------------------------------

test_that("scenario-definition: ci = FALSE rejects an explicit nboot", {
  expect_snapshot(error = TRUE, {
    ssd_define_scenario(
      ssd_scenario_data(ssddata::ccme_boron),
      nsim = 2L,
      seed = 1L,
      ci = FALSE,
      nboot = 500
    )
  })
})

test_that("scenario-definition: ci = FALSE rejects ci_method and parametric", {
  expect_snapshot(error = TRUE, {
    ssd_define_scenario(
      ssd_scenario_data(ssddata::ccme_boron),
      nsim = 2L,
      seed = 1L,
      ci = FALSE,
      ci_method = "MACL"
    )
  })
  expect_snapshot(error = TRUE, {
    ssd_define_scenario(
      ssd_scenario_data(ssddata::ccme_boron),
      nsim = 2L,
      seed = 1L,
      ci = FALSE,
      parametric = FALSE
    )
  })
})

test_that("scenario-definition: ci = FALSE alone is fine with default knobs", {
  expect_s3_class(
    ssd_define_scenario(
      ssd_scenario_data(ssddata::ccme_boron),
      nsim = 2L,
      seed = 1L,
      ci = FALSE
    ),
    "ssdsims_scenario"
  )
})

test_that("scenario-definition: a vector ci is rejected", {
  expect_snapshot(error = TRUE, {
    ssd_define_scenario(
      ssd_scenario_data(ssddata::ccme_boron),
      nsim = 2L,
      seed = 1L,
      ci = c(FALSE, TRUE)
    )
  })
})

test_that("scenario-definition: scalar ci = TRUE retains bootstrap knobs", {
  s <- ssd_define_scenario(
    ssd_scenario_data(ssddata::ccme_boron),
    nsim = 2L,
    seed = 1L,
    ci = TRUE,
    nboot = c(100, 1000),
    ci_method = "weighted_samples"
  )
  expect_identical(s$hc$ci, TRUE)
  expect_identical(s$hc$nboot, c(100, 1000))
})

# ---- argument validation ---------------------------------------------------

test_that("scenario-definition: seed is required", {
  expect_snapshot(error = TRUE, {
    ssd_define_scenario(ssd_scenario_data(ssddata::ccme_boron), nsim = 2L)
  })
})

test_that("scenario-definition: nsim is required", {
  expect_snapshot(error = TRUE, {
    ssd_define_scenario(ssd_scenario_data(ssddata::ccme_boron), seed = 1L)
  })
})

test_that("scenario-definition: invalid seed errors", {
  expect_snapshot(error = TRUE, {
    ssd_define_scenario(
      ssd_scenario_data(ssddata::ccme_boron),
      nsim = 2L,
      seed = c(1L, 2L)
    )
  })
  expect_snapshot(error = TRUE, {
    ssd_define_scenario(
      ssd_scenario_data(ssddata::ccme_boron),
      nsim = 2L,
      seed = 1.5
    )
  })
  expect_snapshot(error = TRUE, {
    ssd_define_scenario(
      ssd_scenario_data(ssddata::ccme_boron),
      nsim = 2L,
      seed = NULL
    )
  })
})

test_that("scenario-definition: out-of-range nrow errors", {
  expect_snapshot(error = TRUE, {
    ssd_define_scenario(
      ssd_scenario_data(ssddata::ccme_boron),
      nsim = 2L,
      seed = 1L,
      nrow = 4L
    )
  })
  expect_snapshot(error = TRUE, {
    ssd_define_scenario(
      ssd_scenario_data(ssddata::ccme_boron),
      nsim = 2L,
      seed = 1L,
      nrow = 1001L
    )
  })
})

# ---- print method ----------------------------------------------------------

test_that("scenario-definition: print is stable for a single dataset", {
  expect_snapshot(
    ssd_define_scenario(
      ssd_scenario_data(ssddata::ccme_boron),
      nsim = 100L,
      seed = 42L,
      nrow = c(5L, 10L)
    )
  )
})

test_that("scenario-definition: print is stable for multiple datasets and vector knobs", {
  expect_snapshot(
    ssd_define_scenario(
      ssd_scenario_data(
        boron = ssddata::ccme_boron,
        cadmium = ssddata::ccme_cadmium
      ),
      nsim = 50L,
      seed = 1L,
      nrow = c(5L, 6L, 10L),
      rescale = c(FALSE, TRUE),
      computable = c(FALSE, TRUE),
      at_boundary_ok = c(TRUE, FALSE),
      range_shape1 = list(c(0.05, 20), c(0.1, 10)),
      est_method = c("multi", "geometric"),
      proportion = c(0.05, 0.1),
      ci = TRUE,
      nboot = c(100, 1000),
      ci_method = c("weighted_samples", "MACL"),
      parametric = c(TRUE, FALSE)
    )
  )
})

test_that("scenario-definition: print is stable for generator and mixed inputs", {
  # A materialised generator prints exactly as a data-frame dataset (the
  # dataset axis is just a name; no descriptor rides on the scenario).
  expect_snapshot(
    ssd_define_scenario(
      ssd_scenario_data(ssd_gen(
        synth = ssdtools::ssd_rlnorm,
        .n = 30,
        .seed = 1L
      )),
      nsim = 10L,
      seed = 42L
    )
  )
  expect_snapshot(
    ssd_define_scenario(
      ssd_scenario_data(
        boron = ssddata::ccme_boron,
        !!!ssd_gen(synth = ssdtools::ssd_rlnorm, .n = 30, .seed = 1L)
      ),
      nsim = 10L,
      seed = 42L
    )
  )
})

# ---- samples (output-retention scalar) -------------------------------------

test_that("scenario-definition: samples defaults FALSE and is stored on hc", {
  expect_false(
    ssd_define_scenario(
      ssd_scenario_data(ssddata::ccme_boron),
      nsim = 1L,
      seed = 1L
    )$hc$samples
  )
  expect_true(
    ssd_define_scenario(
      ssd_scenario_data(ssddata::ccme_boron),
      nsim = 1L,
      seed = 1L,
      samples = TRUE
    )$hc$samples
  )
})

test_that("scenario-definition: samples must be a flag", {
  expect_snapshot(error = TRUE, {
    ssd_define_scenario(
      ssd_scenario_data(ssddata::ccme_boron),
      nsim = 1L,
      seed = 1L,
      samples = c(TRUE, FALSE)
    )
  })
})
