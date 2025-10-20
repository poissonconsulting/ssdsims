rinteger <- function(n = 1L) {
  if (n == 0) integer(0)
  mx <- 2147483647L
  as.integer(stats::runif(n, -mx, mx))
}

get_lecyer_cmrg_seed <- function() {
  seed <- get_seed()
  on.exit(set_seed(seed))
  RNGkind("L'Ecuyer-CMRG", "Inversion", "Rejection")
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
  