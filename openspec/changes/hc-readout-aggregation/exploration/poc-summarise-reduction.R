# Proof of concept: reframe the per-cell hc demand reduction as a vectorised
# `summarise(.by = hc_id)` with a singleton fast-path, plus duckplyr (`.prudence
# = "stingy"`) for the group-size partition and the `ci` routing join.
#
# Goal: replace the `group_split()` + per-cell `for` loop in `design_hc_assembly()`
# (R/design-targets.R) with
#   1. a DuckDB group-size count (only the scalar `hc_id` column crosses over),
#   2. a singleton fast-path (n == 1 cells need no reduction - the row IS the
#      cell, its demand already final - and these are expected to dominate),
#   3. ONE grouped `summarise()` over only the multi-member cells, and
#   4. the `ci = FALSE` -> `ci = TRUE` routing expressed as a join.
#
# This script validates the reframed assembly against the CURRENT
# `design_hc_assembly()` (the oracle) on several designs, then benchmarks both on
# a large, singleton-heavy design.
#
# Run with:  Rscript --no-environ openspec/changes/hc-readout-aggregation/exploration/poc-summarise-reduction.R

suppressMessages(devtools::load_all(quiet = TRUE))

# ---- the reframed assembly (mirrors design_hc_assembly's contract) ----------

assembly_new <- function(members, ref, count_engine = c("duckplyr", "dplyr")) {
  count_engine <- match.arg(count_engine)
  nms <- names(members)
  readout_key <- function(s) {
    list(s$hc$proportion, s$hc$est_method, s$hc$ci, s$hc$samples)
  }
  uniform <- all(vapply(
    members[-1L],
    function(s) identical(readout_key(s), readout_key(members[[1L]])),
    logical(1L)
  ))
  est_method_varies <- !all(vapply(
    members[-1L],
    function(s) identical(s$hc$est_method, members[[1L]]$hc$est_method),
    logical(1L)
  ))
  no_filter <- rlang::set_names(vector("list", length(nms)), nms)

  if (uniform) {
    return(list(
      shards = union_shards(members, "hc"),
      serving = lapply(members, function(s) ssd_scenario_hc_tasks(s)$hc_id),
      proportion = no_filter,
      est_method = no_filter
    ))
  }

  flat <- list()
  for (nm in nms) {
    s <- members[[nm]]
    tbl <- tibble::as_tibble(ssd_scenario_hc_tasks(s))
    tbl$seed <- s$seed
    tbl$primer <- task_primers(tbl, "hc")
    tbl$.dem_prop <- rep(list(s$hc$proportion), nrow(tbl))
    tbl$.dem_em <- rep(list(s$hc$est_method), nrow(tbl))
    tbl$.dem_ci <- s$hc$ci
    tbl$.dem_samples <- s$hc$samples
    flat[[nm]] <- tbl
  }
  all_hc <- dplyr::bind_rows(flat)

  # (1) group sizes. Only the scalar `hc_id` column crosses into DuckDB, so the
  # list-columns (primer, range_shape*, the demand) never trip duckplyr.
  if (count_engine == "duckplyr") {
    sizes <- dplyr::collect(dplyr::summarise(
      duckplyr::as_duckdb_tibble(all_hc["hc_id"], prudence = "stingy"),
      .n = dplyr::n(),
      .by = "hc_id"
    ))
  } else {
    sizes <- dplyr::summarise(
      dplyr::group_by(all_hc["hc_id"], hc_id),
      .n = dplyr::n(),
      .groups = "drop"
    )
  }
  multi_ids <- sizes$hc_id[sizes$.n > 1L]

  demand_cols <- c(".dem_prop", ".dem_em", ".dem_ci", ".dem_samples")
  is_multi <- all_hc$hc_id %in% multi_ids

  # (2) singleton fast-path: each such row is already its own cell with final
  # demand - no reduction.
  single_cells <- all_hc[!is_multi, , drop = FALSE]

  # (3) ONE grouped summarise over the (rare) multi-member cells; rejoin the
  # constant axis/primer columns from the first row of each group.
  multi_rows <- all_hc[is_multi, , drop = FALSE]
  if (nrow(multi_rows)) {
    reduced <- dplyr::summarise(
      dplyr::group_by(multi_rows, hc_id),
      .dem_prop = list(sort(unique(unlist(.dem_prop)))),
      .dem_em = list(unique(unlist(.dem_em))),
      .dem_ci = any(.dem_ci),
      .dem_samples = any(.dem_samples),
      .groups = "drop"
    )
    skel <- multi_rows[
      !duplicated(multi_rows$hc_id),
      setdiff(names(multi_rows), demand_cols),
      drop = FALSE
    ]
    multi_cells <- dplyr::left_join(skel, reduced, by = "hc_id")
  } else {
    multi_cells <- single_cells[0, , drop = FALSE]
  }
  cells <- dplyr::bind_rows(single_cells, multi_cells)

  # (4) ci routing as a join: each ci = FALSE cell -> first coincident ci = TRUE
  # cell at the same (fit_id, distset).
  ci_true <- cells[cells$.dem_ci, , drop = FALSE]
  ci_false <- cells[!cells$.dem_ci, , drop = FALSE]
  route <- character(0L)
  if (nrow(ci_true) && nrow(ci_false)) {
    serving_lookup <- dplyr::summarise(
      dplyr::group_by(ci_true, fit_id, distset),
      serving = dplyr::first(hc_id),
      .groups = "drop"
    )
    routed <- dplyr::left_join(
      ci_false[c("hc_id", "fit_id", "distset")],
      serving_lookup,
      by = c("fit_id", "distset")
    )
    served <- routed[!is.na(routed$serving), , drop = FALSE]
    route <- rlang::set_names(served$serving, served$hc_id)

    # fold each served ci = FALSE cell's demand into its serving ci = TRUE cell.
    served_dem <- dplyr::left_join(
      served["hc_id"],
      ci_false[c("hc_id", ".dem_prop", ".dem_em", ".dem_samples")],
      by = "hc_id"
    )
    served_dem$serving <- served$serving
    fold <- dplyr::summarise(
      dplyr::group_by(served_dem, serving),
      add_prop = list(unique(unlist(.dem_prop))),
      add_em = list(unique(unlist(.dem_em))),
      add_samp = any(.dem_samples),
      .groups = "drop"
    )
    pos <- match(fold$serving, ci_true$hc_id)
    for (k in seq_len(nrow(fold))) {
      i <- pos[[k]]
      ci_true$.dem_prop[[i]] <- sort(unique(c(
        ci_true$.dem_prop[[i]],
        fold$add_prop[[k]]
      )))
      ci_true$.dem_em[[i]] <- unique(c(ci_true$.dem_em[[i]], fold$add_em[[k]]))
      ci_true$.dem_samples[i] <- ci_true$.dem_samples[i] || fold$add_samp[[k]]
    }
  }
  unserved_false <- ci_false[
    !(ci_false$hc_id %in% names(route)),
    ,
    drop = FALSE
  ]
  computed <- dplyr::bind_rows(ci_true, unserved_false)

  serving <- lapply(members, function(s) {
    ids <- ssd_scenario_hc_tasks(s)$hc_id
    mapped <- route[ids]
    ifelse(is.na(mapped), ids, unname(mapped))
  })

  computed$proportion <- computed$.dem_prop
  computed$est_method <- computed$.dem_em
  computed$ci <- computed$.dem_ci
  computed$samples <- computed$.dem_samples
  computed <- computed[,
    setdiff(names(computed), demand_cols),
    drop = FALSE
  ]
  path_axes <- scenario_partition_axes(ref, "hc")$path
  grp <- dplyr::group_by(computed, dplyr::across(dplyr::all_of(path_axes)))
  shards <- dplyr::group_keys(grp)
  shards$tasks <- dplyr::group_split(grp, .keep = TRUE)

  list(
    shards = tibble::as_tibble(shards),
    serving = serving,
    proportion = lapply(members, function(s) s$hc$proportion),
    est_method = if (est_method_varies) {
      lapply(members, function(s) s$hc$est_method)
    } else {
      no_filter
    }
  )
}

# ---- normalise an assembly result to a comparable structure -----------------

normalise <- function(res) {
  # every computed task across all shards -> (hc_id, sorted proportion, sorted
  # est_method, ci, samples), order-independent.
  task_rows <- dplyr::bind_rows(res$shards$tasks)
  has_demand <- all(
    c("proportion", "est_method", "ci", "samples") %in% names(task_rows)
  )
  cells <- if (!has_demand) {
    # uniform path: no demand columns; key on hc_id alone.
    data.frame(hc_id = sort(task_rows$hc_id))
  } else {
    key <- vapply(
      seq_len(nrow(task_rows)),
      function(i) {
        paste(
          task_rows$hc_id[[i]],
          paste(sort(task_rows$proportion[[i]]), collapse = ","),
          paste(sort(task_rows$est_method[[i]]), collapse = ","),
          task_rows$ci[[i]],
          task_rows$samples[[i]],
          sep = "|"
        )
      },
      character(1L)
    )
    sort(key)
  }
  serving <- lapply(res$serving, function(x) sort(unname(x)))
  list(
    cells = cells,
    serving = serving,
    proportion = res$proportion,
    est_method = res$est_method
  )
}

check_equiv <- function(label, design) {
  ref_members <- function(d) {
    seeds <- vapply(d, function(s) s$seed, integer(1L))
    d[seeds == seeds[[1L]]]
  }
  members <- ref_members(design)
  ref <- design_reference_scenario(members)
  cur <- normalise(design_hc_assembly(members, ref))
  new_dp <- normalise(assembly_new(members, ref, "duckplyr"))
  new_pl <- normalise(assembly_new(members, ref, "dplyr"))
  ok <- identical(cur, new_dp) && identical(cur, new_pl)
  cat(sprintf("[%s] equivalent: %s\n", label, ok))
  if (!ok) {
    cat("  --- current vs duckplyr-new diff ---\n")
    cat("  cells identical:", identical(cur$cells, new_dp$cells), "\n")
    cat("  serving identical:", identical(cur$serving, new_dp$serving), "\n")
    cat(
      "  proportion identical:",
      identical(cur$proportion, new_dp$proportion),
      "\n"
    )
    cat(
      "  est_method identical:",
      identical(cur$est_method, new_dp$est_method),
      "\n"
    )
  }
  ok
}

data <- ssd_scenario_data(
  a = data.frame(Conc = exp(seq(-1, 2, length.out = 30)))
)
pb <- list(
  sample = c("dataset", "sim"),
  fit = c("dataset", "sim"),
  hc = c("dataset", "sim")
)

# readout-only difference (proportion) -> multi-member hc cells
d_readout <- ssd_design(
  lo = ssd_define_scenario(
    data,
    nsim = 3L,
    seed = 42L,
    nrow = 6L,
    dists = ssd_distset(lnorm = "lnorm"),
    proportion = 0.05,
    partition_by = pb
  ),
  hi = ssd_define_scenario(
    data,
    nsim = 3L,
    seed = 42L,
    nrow = 6L,
    dists = ssd_distset(lnorm = "lnorm"),
    proportion = 0.1,
    partition_by = pb
  )
)

# ci mix -> singleton hc cells + routing
d_ci <- ssd_design(
  slow = ssd_define_scenario(
    data,
    nsim = 4L,
    seed = 42L,
    nrow = 6L,
    dists = ssd_distset(lnorm = "lnorm"),
    ci = FALSE,
    partition_by = pb
  ),
  fast = ssd_define_scenario(
    data,
    nsim = 2L,
    seed = 42L,
    nrow = 6L,
    dists = ssd_distset(lnorm = "lnorm"),
    ci = TRUE,
    nboot = 10L,
    partition_by = pb
  )
)

# est_method varies + multi-dist + multiple proportions
d_em <- ssd_design(
  m = ssd_define_scenario(
    data,
    nsim = 2L,
    seed = 42L,
    nrow = 10L,
    dists = ssd_distset(both = c("lnorm", "gamma")),
    est_method = "multi",
    proportion = c(0.05, 0.1),
    partition_by = pb
  ),
  a = ssd_define_scenario(
    data,
    nsim = 2L,
    seed = 42L,
    nrow = 10L,
    dists = ssd_distset(both = c("lnorm", "gamma")),
    est_method = "arithmetic",
    proportion = c(0.05, 0.2),
    partition_by = pb
  )
)

# distset coverage (uniform readouts -> uniform path)
d_distset <- ssd_design(
  one = ssd_define_scenario(
    data,
    nsim = 1L,
    seed = 42L,
    nrow = 10L,
    dists = ssd_distset(one = "lnorm"),
    partition_by = pb
  ),
  both = ssd_define_scenario(
    data,
    nsim = 1L,
    seed = 42L,
    nrow = 10L,
    dists = ssd_distset(one = "lnorm", two = c("lnorm", "gamma")),
    partition_by = pb
  )
)

cat("\n=== equivalence vs current design_hc_assembly ===\n")
results <- c(
  check_equiv("readout-only", d_readout),
  check_equiv("ci-mix", d_ci),
  check_equiv("est_method+proportion", d_em),
  check_equiv("distset (uniform path)", d_distset)
)
cat("\nall equivalent:", all(results), "\n")

# ---- benchmark on a large, singleton-heavy design ---------------------------

cat("\n=== benchmark (large singleton-heavy design) ===\n")
# many sims, two members differing in ci so most hc cells are singletons; a
# realistic settings-comparison scale.
big_slow <- ssd_define_scenario(
  data,
  nsim = 600L,
  seed = 42L,
  nrow = c(5L, 8L),
  dists = ssd_distset(lnorm = "lnorm"),
  ci = FALSE,
  partition_by = pb
)
big_fast <- ssd_define_scenario(
  data,
  nsim = 30L,
  seed = 42L,
  nrow = c(5L, 8L),
  dists = ssd_distset(lnorm = "lnorm"),
  ci = TRUE,
  nboot = 100L,
  partition_by = pb
)
big <- ssd_design(slow = big_slow, fast = big_fast)
big_members <- big[c("slow", "fast")]
big_ref <- design_reference_scenario(big_members)

n_tasks <- nrow(ssd_scenario_hc_tasks(big_slow)) +
  nrow(ssd_scenario_hc_tasks(big_fast))
cat("total member hc tasks (rows of all_hc):", n_tasks, "\n")

bench <- function(label, f, times = 5L) {
  f() # warm
  t <- replicate(times, system.time(f())[["elapsed"]])
  cat(sprintf("  %-22s median %.3fs  (min %.3f)\n", label, median(t), min(t)))
}
bench("current (split+loop)", function() {
  design_hc_assembly(big_members, big_ref)
})
bench("new (duckplyr count)", function() {
  assembly_new(big_members, big_ref, "duckplyr")
})
bench("new (dplyr count)", function() {
  assembly_new(big_members, big_ref, "dplyr")
})

cat("\nequivalence on the big design:", check_equiv("big", big), "\n")
