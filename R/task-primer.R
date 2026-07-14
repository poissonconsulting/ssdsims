# Per-task primer derivation for the dqrng path.
#
# `TARGETS-DESIGN.md` §2 roots all per-task RNG in a single scenario `seed`
# plus a per-task **primer** passed to dqrng's `stream` argument:
# `dqrng::dqset.seed(seed, stream = task_primer(params))`. This file lifts the
# derivation validated in `scripts/experiment-dqrng-hash.R` (section 1) into a
# tested package function: `rlang::hash()` gives a 32-hex-char xxhash128
# digest, the first 16 hex chars (64 bits) are packed into two int32s for
# dqrng's `stream`, and the `0x80000000` bit pattern maps to `NA_integer_`
# (dqrng reads it as INT_MIN), recovering the full 64 bits.

# Convert an 8-hex-char slice to a signed int32. `strtoi(., 16L)` overflows on
# 8 hex digits, so split into two 16-bit halves, combine in double precision,
# and wrap `u >= 2^31` to the signed range. `0x80000000` (INT_MIN) lands on
# `NA_integer_` because R cannot represent INT_MIN as a non-NA integer; dqrng
# accepts NA in `stream` and treats it as INT_MIN.
hex8_to_int32 <- function(hex8) {
  u <- strtoi(substr(hex8, 1L, 4L), base = 16L) *
    65536 +
    strtoi(substr(hex8, 5L, 8L), base = 16L)
  suppressWarnings(
    if (u < 2147483648) as.integer(u) else as.integer(u - 4294967296)
  )
}

# Normalise a single-row data frame to a canonical plain list -- the inverse of
# `tibble::tibble_row()`. Drop all attributes, then decide per column:
# df-style (nested data-frame) columns are kept as data frames, length-1
# list-style columns are unwrapped to their element, and atomic columns become
# their scalar value. This makes a one-row tibble hash identically to the
# equivalent plain list (class / row.names / pillar attributes do not enter the
# hash).
normalize_task_row <- function(row) {
  cols <- as.list(row)
  out <- purrr::map(cols, function(v) {
    if (is.data.frame(v)) {
      v
    } else if (is.list(v)) {
      v[[1L]]
    } else {
      v
    }
  })
  rlang::set_names(out, names(cols))
}

#' Derive a Per-task Primer from its Parameters
#'
#' Derives the per-task **primer** -- a length-2 integer vector -- from
#' `rlang::hash(params)`, suitable for the `stream` argument of
#' [dqrng::dqset.seed()]. Together with the scenario `seed`, the primer fully
#' specifies a task's RNG starting point:
#' `dqrng::dqset.seed(seed, stream = task_primer(params))`. It pairs with
#' [local_dqrng_state()], which installs the `(seed, primer)` pair under an
#' active [local_dqrng_backend()] scope.
#'
#' @details
#' The primer packs 64 bits of the `rlang::hash()` digest (xxhash128) as
#' `c(hi32, lo32)`. Each 32-bit half is encoded as a signed int32, with the
#' reserved bit pattern `0x80000000` (INT_MIN, which R cannot represent as a
#' non-`NA` integer) mapped to `NA_integer_`; dqrng accepts `NA_integer_` in
#' `stream` and treats it as INT_MIN, so the encoding recovers the full 64 bits
#' of stream entropy.
#'
#' `params` may be a plain named list or a single-row data frame (one row of a
#' `{sample,fit,hc}_tasks` table). A data-frame row is normalised to a
#' canonical plain list -- the inverse of `tibble::tibble_row()` -- by dropping
#' all attributes, unwrapping length-1 list-style columns to their element, and
#' leaving df-style (nested data-frame) columns as data frames, before hashing.
#' The primer is therefore identical whether derived from the row or from the
#' equivalent plain list. Note that `rlang::hash()` is order-sensitive, so the
#' plain list must use the **same name order** as the task-table columns to
#' reproduce the row's primer (assembling `params` in a canonical column order
#' is part of the `task-tables` caller contract below).
#'
#' `task_primer()` normalises **structure, not meaning**: it hashes whatever
#' `params` it is given. The canonical, name-keyed representation is a caller
#' contract assembled where `params` is built (`task-tables`, over the
#' `task-lists` tables). Per the three-step model the RNG-consuming steps each
#' take a primer over their task identity:
#'
#' * **sample** -- keyed `(dataset, sim, replace)` only. `nrow` is deliberately
#'   absent: every `nrow` shares one draw (sized by the scenario's `nrow_max`
#'   setting) that the `fit` step truncates inline (`head(sample, nrow)`,
#'   RNG-free, no separate primer), so excluding `nrow` is load-bearing for the
#'   sub-truncation property (`TARGETS-DESIGN.md` §5).
#' * **fit** -- the parent `sample` identity plus `nrow` and the fit-grid row
#'   (`rescale`, `computable`, `at_boundary_ok`, `min_pmix` name,
#'   `range_shape1`, `range_shape2`). `nrow` IS part of the fit primer: a fit on
#'   a different truncation is a genuinely different computation.
#' * **hc** -- the parent `fit` identity plus the hc-grid row (`nboot`,
#'   `est_method`, `ci_method`, `parametric`). `ci` is a scalar hc setting
#'   applied uniformly and read from the scenario, not part of the task
#'   identity, so it is never a primer field (nor a task-row column).
#'
#' Function-valued parameters (e.g. `min_pmix`) MUST be referenced **by name**,
#' not by function value, so a recompile or JIT does not move a task's primer.
#'
#' The primer is derived from [rlang::hash()], a fast within-session identity
#' hash rather than a portable checksum, so its digest is not *guaranteed*
#' stable across `rlang` versions. Such changes are rare and not anticipated,
#' but have happened, and would shift primers -- and therefore simulation
#' seeds. Reproducibility is therefore anchored by pinning the execution
#' environment (including the `rlang` version) for a given simulation run.
#'
#' @param params A plain named list of task parameters, or a single-row data
#'   frame (one task-table row).
#' @return An integer vector of length 2 -- the per-task primer -- to pass as
#'   the `stream` argument of [dqrng::dqset.seed()] (via [local_dqrng_state()]).
#' @seealso [local_dqrng_state()], [local_dqrng_backend()].
#' @export
#' @examples
#' task_primer(list(dataset = "boron", sim = 1L, replace = FALSE))
task_primer <- function(params) {
  call <- environment()
  if (is.data.frame(params)) {
    if (nrow(params) != 1L) {
      chk::abort_chk(
        "`params` must be a single-row data frame, ",
        "not one with ",
        nrow(params),
        " rows.",
        call = call
      )
    }
    params <- normalize_task_row(params)
  } else if (!is.list(params)) {
    chk::abort_chk(
      "`params` must be a plain list or a single-row data frame.",
      call = call
    )
  }
  h <- rlang::hash(params)
  c(
    hex8_to_int32(substr(h, 1L, 8L)),
    hex8_to_int32(substr(h, 9L, 16L))
  )
}
