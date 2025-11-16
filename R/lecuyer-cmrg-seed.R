rinteger <- function(n = 1L) {
  if (n == 0) {
    integer(0)
  }
  mx <- 2147483647L
  as.integer(stats::runif(n, -mx, mx))
}

# internal function from withr
has_seed <- function() {
  exists(".Random.seed", globalenv(), mode = "integer", inherits = FALSE)
}

# internal function from withr
get_seed <- function() {
  if (!has_seed()) {
    return(NULL)
  }
  list(
    random_seed = get(
      ".Random.seed",
      globalenv(),
      mode = "integer",
      inherits = FALSE
    ),
    rng_kind = RNGkind()
  )
}

# internal function from withr
restore_rng_kind <- function(kind) {
  RNGkind <- get("RNGkind")
  RNGkind(kind[[1]], normal.kind = kind[[2]])
  sample_kind <- kind[[3]]
  if (identical(sample_kind, "Rounding")) {
    suppressWarnings(RNGkind(sample.kind = sample_kind))
  } else {
    RNGkind(sample.kind = sample_kind)
  }
  NULL
}

set_seed <- function(seed) {
  restore_rng_kind(seed$rng_kind)
  if (is.null(seed$seed)) {
    assign(".Random.seed", seed$random_seed, globalenv())
  } else {
    set.seed(seed$seed)
  }
  invisible(get_seed())
}

#' Local L'Euyer-CMRG Seed
#' @inheritParams withr::local_seed
#' @seealso [`withr::local_seed()`]
#' @export
#' @examples
#'
#' local_lecuyer_cmrg_seed(42)
#' runif(3)
local_lecuyer_cmrg_seed <- function(seed, .local_envir = parent.frame()) {
  withr::local_seed(
    seed,
    .local_envir = .local_envir,
    .rng_kind = "L'Ecuyer-CMRG",
    .rng_normal_kind = "Inversion",
    .rng_sample_kind = "Rejection"
  )
}

#' With L'Euyer-CMRG Seed
#' @inheritParams withr::with_seed
#' @seealso [`withr::with_seed()`]
#' @export
#' @examples
#'
#' with_lecuyer_cmrg_seed(42, {
#' runif(3)
#' })
with_lecuyer_cmrg_seed <- function(seed, code) {
  force(seed)
  withr::with_seed(
    seed,
    code,
    .rng_kind = "L'Ecuyer-CMRG",
    .rng_normal_kind = "Inversion",
    .rng_sample_kind = "Rejection"
  )
}

get_lecuyer_cmrg_seed <- function() {
  RNGkind("L'Ecuyer-CMRG", "Inversion", "Rejection")
  set.seed(rinteger(1))
  globalenv()$.Random.seed
}

# inspired by furrr:::generate_seed_streams
get_lecuyer_cmrg_seeds_stream <- function(seed, nsim, stream, start_sim) {
  if (nsim == 0) {
    return(list())
  }

  oseed <- get_seed()
  on.exit(set_seed(oseed))

  if (!is.null(seed)) {
    set.seed(seed)
  }

  seeds <- vector("list", length = nsim)
  seeds[[1]] <- parallel::nextRNGStream(get_lecuyer_cmrg_seed())
  for (i in seq_len(stream - 1)) {
    seeds[[1]] <- parallel::nextRNGStream(seeds[[1]])
  }
  for (i in seq_len(start_sim - 1)) {
    seeds[[1]] <- parallel::nextRNGSubStream(seeds[[1]])
  }
  for (i in seq_len(nsim - 1)) {
    seeds[[i + 1]] <- parallel::nextRNGSubStream(seeds[[i]])
  }
  seeds
}

get_lecuyer_cmrg_seed_stream <- function(seed = NULL, stream, start_sim) {
  get_lecuyer_cmrg_seeds_stream(
    seed = seed,
    nsim = 1L,
    stream = stream,
    start_sim = start_sim
  )[[1]]
}
