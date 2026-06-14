# Generator materialisation for scenario datasets (scenario-input-types).
#
# `ssd_gen()` is the generation half of the two-helper dataset API:
# `ssd_scenario_data()` assembles and validates, `ssd_gen()` materialises
# generator-style inputs - once, reproducibly, under dqrng - to the same
# validated `Conc` tibbles. A materialised generator is a *fixture the
# scenario resamples*, indistinguishable downstream from a data-frame dataset
# (no descriptor rides on the scenario; provenance is the deferred
# `dataset-provenance` change). Generation is deliberately separate from
# `ssd_define_scenario()`, which stays RNG-free.

#' Materialise Generator Datasets for a Simulation Scenario
#'
#' Accepts generator-style inputs - the same set the legacy
#' [ssd_run_scenario()] dispatches over - and materialises each, once, to a
#' validated tibble with a numeric `Conc` column of `.n` rows. The four
#' generator kinds are:
#'
#' * a **function** taking the number of rows as its first argument (e.g.
#'   `ssdtools::ssd_rlnorm`);
#' * a **function-name string** (e.g. `"ssd_rlnorm"`), resolved as a bare name
#'   in the caller's environment and then the `ssdtools` namespace (never via
#'   parsing code); the string is also the dataset name;
#' * a **`tmbfit`** object (one distribution of a [ssdtools::ssd_fit_dists()]
#'   fit): drawn from the matching `ssd_r<dist>` function with the fit's
#'   estimates;
#' * a **`fitdists`** object: the top-weighted distribution is selected and
#'   drawn as for a `tmbfit`.
#'
#' A data frame is **not** a generator - pass it directly to
#' [ssd_scenario_data()].
#'
#' The result is an `ssdsims_gen` collection designed to compose with
#' [ssd_scenario_data()] in two equivalent ways: as an unnamed argument
#' (flattened in) or spliced with `!!!`:
#'
#' ```r
#' ssd_scenario_data(boron = ccme_boron, ssd_gen(synth = ssd_rlnorm, .n = 30, .seed = 1L))
#' ssd_scenario_data(boron = ccme_boron, !!!ssd_gen(synth = ssd_rlnorm, .n = 30, .seed = 1L))
#' ```
#'
#' @details
#' # Reproducibility: `.seed` and the name-keyed streams
#'
#' `.n` and `.seed` are **required**: irreproducible or unsized generation is
#' impossible by construction. They are dot-prefixed formals, so they are never
#' absorbed into `...` and a generator named `seed =` or `n =` is never
#' partial-matched onto them. Each generator draws under a scoped dqrng state
#' seeded by `.seed` with its dataset **name** as the dqrng stream
#' ([task_primer()] over `list(dataset = name)`), so a single `.seed` fans out
#' across all generators in the call on independent, name-keyed streams. One
#' `.n` applies per call; for differing sizes, splice several `ssd_gen()`
#' calls together. The global `.Random.seed` is unchanged on return.
#'
#' Names are taken from the argument names where supplied, otherwise the
#' function-name string itself, otherwise derived from the argument expression
#' by symbol capture (e.g. `ssdtools::ssd_rlnorm` becomes `"ssd_rlnorm"`); an
#' input with no derivable name (e.g. an anonymous function literal) must be
#' given an explicit name. Names must be unique across the call.
#'
#' # dqrng-backed generation
#'
#' Generation draws under the dqrng `pcg64` backend ([local_dqrng_backend()]),
#' with each generator seeded through [local_dqrng_state()], which brackets the
#' draw with the per-task dqrng-integrity witness: it aborts if a generator
#' escapes the backend (e.g. switches `RNGkind()` and draws from base R), since
#' such draws are not reproducible under `.seed`. A generator that consumes no
#' randomness passes.
#'
#' @param ... One or more generator inputs (a function, a function-name
#'   string, a `fitdists` object, or a `tmbfit` object), optionally named.
#' @param .n A scalar whole number: the number of rows each generator
#'   materialises. Required. (This is the generated population size; the
#'   scenario's `nrow` controls the resample/truncation sizes.)
#' @param .seed A scalar whole number: the base seed for generation.
#'   Required, and independent of the scenario's `seed`.
#' @return An `ssdsims_gen` object: a named list of validated `Conc` tibbles
#'   of `.n` rows, for use within (or splicing into) [ssd_scenario_data()].
#' @seealso [ssd_scenario_data()], [ssd_define_scenario()],
#'   [local_dqrng_backend()], [task_primer()].
#' @export
#' @examples
#' ssd_gen(synth = ssdtools::ssd_rlnorm, .n = 30, .seed = 42)
#'
#' # one .seed fans out across generators on independent name-keyed streams
#' ssd_gen(a = ssdtools::ssd_rlnorm, b = ssdtools::ssd_rlnorm, .n = 30, .seed = 42)
#'
#' # composes with data frames in ssd_scenario_data()
#' ssd_scenario_data(
#'   boron = ssddata::ccme_boron,
#'   ssd_gen(synth = ssdtools::ssd_rlnorm, .n = 30, .seed = 42)
#' )
ssd_gen <- function(..., .n, .seed) {
  call <- environment()
  if (missing(.n)) {
    chk::abort_chk(
      "`.n` must be supplied (a scalar whole number): ",
      "the number of rows each generator materialises.",
      call = call
    )
  }
  if (missing(.seed)) {
    chk::abort_chk(
      "`.seed` must be supplied (a scalar whole number): ",
      "`ssd_gen()` generates data, so generation must be seeded.",
      call = call
    )
  }
  chk::chk_whole_number(.n)
  chk::chk_gt(.n)
  chk::chk_whole_number(.seed)

  inputs <- rlang::list2(...)
  exprs <- rlang::enexprs(...)
  if (length(inputs) == 0L) {
    chk::abort_chk("`ssd_gen()` requires at least one generator.", call = call)
  }
  env <- rlang::caller_env()
  arg_nms <- names(inputs)
  if (is.null(arg_nms)) {
    arg_nms <- rep("", length(inputs))
  }

  # Resolve every input to a draw function and a name before any RNG is
  # touched, so validation errors fire without drawing. Plain loop (not
  # `purrr`) so aborts surface as `ssd_gen()` (error-call-origin rule).
  nms <- character(length(inputs))
  fns <- vector("list", length(inputs))
  for (i in seq_along(inputs)) {
    nm <- if (nzchar(arg_nms[[i]])) arg_nms[[i]] else NULL
    gen <- classify_gen(
      inputs[[i]],
      exprs[[i]],
      name = nm,
      env = env,
      call = call
    )
    if (is.null(gen$name)) {
      chk::abort_chk(
        "Unable to derive a name for generator ",
        i,
        "; supply a name (e.g. `ssd_gen(synth = ...)`).",
        call = call
      )
    }
    nms[[i]] <- gen$name
    fns[[i]] <- gen$fn
  }
  if (anyDuplicated(nms)) {
    chk::abort_chk("Generator names must be unique.", call = call)
  }

  # Restore the global `.Random.seed` on return (backend registration advances
  # it); registered before the backend so it is reinstated after the reset.
  withr::local_preserve_seed()
  local_dqrng_backend()

  out <- vector("list", length(inputs))
  for (i in seq_along(inputs)) {
    out[[i]] <- gen_materialise(
      fns[[i]],
      nms[[i]],
      n = .n,
      seed = .seed,
      call = call
    )
  }
  structure(rlang::set_names(out, nms), class = "ssdsims_gen")
}

#' Classify a single `ssd_gen()` input and resolve it to a draw function.
#'
#' Returns `list(name = , fn = )` where `fn(n)` draws `n` values and `name` is
#' the dataset name so far (the argument name, or - for the character form -
#' the string itself, or the symbol-captured expression; `NULL` when none is
#' derivable). Dispatch is most-specific-first (`tmbfit` before `fitdists`,
#' matching the legacy `ssd_sim_data()` S3 methods); a `data.frame` is
#' rejected with a pointer to `ssd_scenario_data()`. Aborts in the context of
#' `call` (the user-facing function).
#' @noRd
classify_gen <- function(value, expr, name, env, call = rlang::caller_env()) {
  if (inherits(value, "tmbfit")) {
    pars <- ssdtools::estimates(value)
    rfun_name <- paste0("ssd_r", value$dist)
    rfun <- rlang::env_get(
      rlang::ns_env("ssdtools"),
      rfun_name,
      default = NULL
    )
    if (!rlang::is_function(rfun)) {
      chk::abort_chk(
        gen_what(name),
        " is a `tmbfit` for distribution ",
        encodeString(value$dist, quote = "\""),
        ", which has no matching `ssdtools::",
        rfun_name,
        "()` draw function.",
        call = call
      )
    }
    name <- name %||% expr_to_name(expr)
    return(list(name = name, fn = function(n) rlang::exec(rfun, n, !!!pars)))
  }
  if (inherits(value, "fitdists")) {
    # Top-weighted distribution, then the tmbfit path.
    wt <- ssdtools::glance(value, wt = TRUE)$wt
    return(classify_gen(value[[which.max(wt)]], expr, name, env, call = call))
  }
  if (is.data.frame(value)) {
    chk::abort_chk(
      gen_what(name),
      " is a data frame, which is not a generator; ",
      "pass it directly to `ssd_scenario_data()`.",
      call = call
    )
  }
  if (is.character(value)) {
    if (length(value) != 1L || is.na(value)) {
      chk::abort_chk(
        gen_what(name),
        " must be a single function-name string.",
        call = call
      )
    }
    # Bare-name lookup only (no `eval(parse())`): the caller's environment
    # first, then the ssdtools namespace.
    fn <- get0(value, envir = env, mode = "function", inherits = TRUE)
    if (is.null(fn)) {
      fn <- get0(
        value,
        envir = rlang::ns_env("ssdtools"),
        mode = "function",
        inherits = FALSE
      )
    }
    if (is.null(fn)) {
      chk::abort_chk(
        "Generator name ",
        encodeString(value, quote = "\""),
        " does not resolve to a function (searched the calling environment ",
        "and the ssdtools namespace).",
        call = call
      )
    }
    # The string is also the dataset name (unless an argument name was given).
    return(list(name = name %||% value, fn = fn))
  }
  if (is.function(value)) {
    return(list(name = name %||% expr_to_name(expr), fn = value))
  }
  chk::abort_chk(
    gen_what(name),
    " must be a generator: a function, a function-name string, a `fitdists` ",
    "object, or a `tmbfit` object.",
    call = call
  )
}

#' Label an `ssd_gen()` input for an error message.
#' @noRd
gen_what <- function(name) {
  if (is.null(name)) {
    "Each `ssd_gen()` input"
  } else {
    paste0("Generator `", name, "`")
  }
}

#' Materialise one generator to a validated `Conc` tibble of `n` rows.
#'
#' Installs the per-generator dqrng state - `seed` as the base seed, the
#' dataset name as the stream (`task_primer(list(dataset = name))`) - scoped
#' to this frame, draws, then verifies via `chk_dqrng_backend_intact()` that
#' the draw actually came from dqrng (a generator that escaped the backend is
#' not reproducible under `seed`, so it aborts; a no-draw generator passes).
#' @noRd
gen_materialise <- function(fn, name, n, seed, call = rlang::caller_env()) {
  local_dqrng_state(seed, task_primer(list(dataset = name)))
  conc <- fn(n)
  chk_dqrng_backend_intact(call = call)
  if (!is.numeric(conc) || length(conc) != n) {
    chk::abort_chk(
      "Generator `",
      name,
      "` must draw a numeric vector of length `.n` (",
      n,
      "); it returned ",
      if (is.numeric(conc)) {
        paste0("a numeric vector of length ", length(conc))
      } else {
        paste0("an object of type ", encodeString(typeof(conc), quote = "\""))
      },
      ".",
      call = call
    )
  }
  tibble::tibble(Conc = conc)
}
