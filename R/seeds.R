rinteger <- function(n = 1L) {
  if (n == 0) integer(0)
  mx <- 2147483647L
  as.integer(stats::runif(n, -mx, mx))
}

get_random_seed <- function() {
  globalenv()$.Random.seed
}

set_random_seed <- function(seed, advance = FALSE) {
  env <- globalenv()
  env$.Random.seed <- seed
  if (advance) {
    fun <- if (is.null(seed)) suppressWarnings else identity
    fun(stats::runif(1))
  }
  invisible(env$.Random.seed)
}

get_lecyer_cmrg_seed <- function() {
  seed <- get_random_seed()
  on.exit(set_random_seed(seed))
  RNGkind("L'Ecuyer-CMRG")
  set.seed(rinteger(1))
  get_random_seed()
}

get_sub_seeds <- function(seed, skip, nseeds) {
  if(nseeds == 0) {
    return(list())
  }
  for(i in seq_len(skip)) {
    seed <- parallel::nextRNGSubStream(seed)
  }
  seeds <- vector("list", length = nseeds)
  seeds[[1]] <- seed
  for(i in seq_len(nseeds-1)) {
    seeds[[i+1]] <- parallel::nextRNGSubStream(seeds[[i]])
  }
  seeds
}

#' Get L'Ecuyer-CMRG Seeds
#'
#' Generates a list of streams of L'Ecuyer-CMRG seeds.
#'
#' @param seed An integer of the starting seed or NULL.
#' @param ... Unused.
#' @param nseeds A count of the number of new seeds to generate for each stream.
#' @param nstreams A count of the number of streams.
#' @param skip A count of number of seeds to skip before generating new seeds for each stream.
#'
#' @return A list of lists of L'Ecuyer-CMRG seeds.
#' @export
#'
#' @examples
#' withr::with_seed(10,
#' ssd_get_seeds(nseeds = 2, nstreams = 2)
#')
#' withr::with_seed(10,
#' ssd_get_seeds(nseeds = 1, nstreams = 2, skip = 2)
#')
# inspired by furrr:::generate_seed_streams
ssd_get_seeds <- function(seed = NULL, ..., nseeds = 100L, nstreams = 1L, skip = 0L) {
  chk::chk_null_or(seed, vld = chk::vld_whole_number)
  chk::chk_unused(...)
  chk::chk_count(nseeds)
  chk::chk_count(nstreams)
  chk::chk_count(skip)

  if(nstreams == 0L) {
    return(list())
  }

  oseed <- get_random_seed()
  on.exit(set_random_seed(oseed, advance = TRUE))

  if(!is.null(seed)) {
    stop()
  }

  seed <- get_lecyer_cmrg_seed()
  seeds <- vector("list", length = nstreams)
  seeds[[1]] <- parallel::nextRNGStream(seed)
  for (i in seq_len(nstreams - 1)) {
    seeds[[i+1]] <- parallel::nextRNGStream(seeds[[i]])
  }
  for(i in seq_len(nstreams)) {
    seeds[[i]] <- get_sub_seeds(seeds[[i]], skip = skip, nseeds = nseeds)
  }
  seeds
}
