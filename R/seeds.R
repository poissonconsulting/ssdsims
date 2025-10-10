rinteger <- function(n = 1L) {
  if (n == 0) integer(0)
  mx <- 2147483647L
  as.integer(stats::runif(n, -mx, mx))
}

# from withr
has_seed <- function () {
    exists(".Random.seed", globalenv(), mode = "integer", inherits = FALSE)
}

# from withr
get_seed <- function () 
{
    if (!has_seed()) {
        return(NULL)
    }
    list(random_seed = get(".Random.seed", globalenv(), mode = "integer", 
        inherits = FALSE), rng_kind = RNGkind())
}

# modified from withr
set_seed <- function (seed, advance = FALSE) 
{
    restore_rng_kind(seed$rng_kind)
    if (is.null(seed$seed)) {
        assign(".Random.seed", seed$random_seed, globalenv())
    }
    else {
        set.seed(seed$seed)
    }
    if (advance) {
      fun <- if (is.null(seed)) suppressWarnings else identity
      fun(stats::runif(1))
    }
  invisible(get_seed())
 }

restore_rng_kind <- function (kind) 
{
    RNGkind <- get("RNGkind")
    RNGkind(kind[[1]], normal.kind = kind[[2]])
    sample_kind <- kind[[3]]
    if (identical(sample_kind, "Rounding")) {
        suppressWarnings(RNGkind(sample.kind = sample_kind))
    }
    else {
        RNGkind(sample.kind = sample_kind)
    }
    NULL
}

get_lecyer_cmrg_seed <- function() {
  seed <- get_seed()
  on.exit(set_seed(seed))
  RNGkind("L'Ecuyer-CMRG")
  set.seed(rinteger(1))
  globalenv()$.Random.seed
}

get_sub_seeds <- function(seed, start_seed, nseeds) {
  if(nseeds == 0) {
    return(list())
  }
  for(i in seq_len(start_seed - 1)) {
    seed <- parallel::nextRNGSubStream(seed)
  }
  seeds <- vector("list", length = nseeds)
  seeds[[1]] <- seed
  for(i in seq_len(nseeds-1)) {
    seeds[[i+1]] <- parallel::nextRNGSubStream(seeds[[i]])
  }
  seeds
}

#' Get L'Ecuyer-CMRG Streams Seeds
#'
#' Gets a list of streams of L'Ecuyer-CMRG seeds.
#'
#' @inheritParams params
#'
#' @return A list of lists of L'Ecuyer-CMRG seeds.
#' @export
#'
#' @examples
#' withr::with_seed(10,
#' ssd_get_seeds_streams(nseeds = 2, nstreams = 2)
#')
#' withr::with_seed(10,
#' ssd_get_seeds_streams(nseeds = 1, nstreams = 2, start_seed= 2)
#')
# inspired by furrr:::generate_seed_streams
ssd_get_seeds_streams <- function(seed = NULL, ..., nseeds = 1L, nstreams = 1L, start_seed = 1L, start_stream = 1L) {
  chk::chk_null_or(seed, vld = chk::vld_whole_number)
  chk::chk_unused(...)
  chk::chk_count(nseeds)
  chk::chk_count(nstreams)
  chk::chk_count(start_seed)
  chk::chk_gt(start_seed)
  chk::chk_count(start_stream)
  chk::chk_gt(start_stream)

  if(nstreams == 0L) {
    return(list())
  }

  oseed <- get_seed()
  on.exit(set_seed(oseed, advance = TRUE))

  if(!is.null(seed)) {
    set.seed(seed)
  }

  seed <- get_lecyer_cmrg_seed()
  seeds <- vector("list", length = nstreams)
  seeds[[1]] <- parallel::nextRNGStream(seed)
  for(i in seq_len(start_stream - 1)) {
    seed[[1]] <- parallel::nextRNGStream(seed[[1]])
  }
  for (i in seq_len(nstreams - 1)) {
    seeds[[i+1]] <- parallel::nextRNGStream(seeds[[i]])
  }
  for(i in seq_len(nstreams)) {
    seeds[[i]] <- get_sub_seeds(seeds[[i]], start_seed = start_seed, nseeds = nseeds)
  }
  seeds
}
  