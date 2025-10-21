rinteger <- function(n = 1L) {
  if (n == 0) integer(0)
  mx <- 2147483647L
  as.integer(stats::runif(n, -mx, mx))
}

get_lecuyer_cmrg_seed <- function() {
  seed <- get_seed()
  on.exit(set_seed(seed))
  RNGkind("L'Ecuyer-CMRG", "Inversion", "Rejection")
  set.seed(rinteger(1))
  globalenv()$.Random.seed
}

# inspired by furrr:::generate_seed_streams
get_seed_stream <- function(seed = NULL, ..., nseeds = 1L, stream = 1L, start_seed = 1L) {
  chk::chk_null_or(seed, vld = chk::vld_whole_number)
  chk::chk_unused(...)
  chk::chk_count(nseeds)
  chk::chk_count(stream)
  chk::chk_gt(stream)
  chk::chk_count(start_seed)
  chk::chk_gt(start_seed)

  if(nseeds == 0) {
    return(list())
  }

  oseed <- get_seed()
  on.exit(set_seed(oseed, advance = TRUE))

  if(!is.null(seed)) {
    set.seed(seed)
  }

  seeds <- vector("list", length = nseeds)
  seeds[[1]] <- parallel::nextRNGStream(get_lecuyer_cmrg_seed())
  for(i in seq_len(stream - 1)) {
    seeds[[1]] <- parallel::nextRNGStream(seeds[[1]])
  }
  for(i in seq_len(start_seed - 1)) {
    seeds[[1]] <- parallel::nextRNGSubStream(seeds[[1]])
  }
  for(i in seq_len(nseeds-1)) {
    seeds[[i+1]] <- parallel::nextRNGSubStream(seeds[[i]])
  }
  seeds
}
  