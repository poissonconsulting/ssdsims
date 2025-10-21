rinteger <- function(n = 1L) {
  if (n == 0) integer(0)
  mx <- 2147483647L
  as.integer(stats::runif(n, -mx, mx))
}

# internal function from withr
has_seed <- function () {
  exists(".Random.seed", globalenv(), mode = "integer", inherits = FALSE)
}

# internal function from withr
get_seed <- function () 
{
  if (!has_seed()) {
    return(NULL)
  }
  list(random_seed = get(".Random.seed", globalenv(), mode = "integer", 
  inherits = FALSE), rng_kind = RNGkind())
}

# internal function from withr
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

set_seed <- function (seed) 
{
  restore_rng_kind(seed$rng_kind)
  if (is.null(seed$seed)) {
    assign(".Random.seed", seed$random_seed, globalenv())
  }
  else {
    set.seed(seed$seed)
  }
  invisible(get_seed())
}

local_lecuyer_cmrg_seed <- function(seed, .local_envir = parent.frame()) 
{
  withr::local_seed(seed, 
    .local_envir = .local_envir,
    .rng_kind =  "L'Ecuyer-CMRG",
    .rng_normal_kind = "Inversion",
    .rng_sample_kind = "Rejection")
}

with_lecuyer_cmrg_seed <- function(seed, code) {
  force(seed)
  withr::with_seed(seed, 
    code,
    .rng_kind =  "L'Ecuyer-CMRG",
    .rng_normal_kind = "Inversion",
    .rng_sample_kind = "Rejection")
}

get_lecuyer_cmrg_seed <- function() {
  RNGkind("L'Ecuyer-CMRG", "Inversion", "Rejection")
  set.seed(rinteger(1))
  globalenv()$.Random.seed
}

# inspired by furrr:::generate_seed_streams
get_lecuyer_cmrg_seed_stream <- function(seed = NULL, ..., nseed = 1L, stream = 1L, start_seed = 1L) {
  chk::chk_null_or(seed, vld = chk::vld_whole_number)
  chk::chk_unused(...)
  chk::chk_count(nseed)
  chk::chk_count(stream)
  chk::chk_gt(stream)
  chk::chk_count(start_seed)
  chk::chk_gt(start_seed)

  if(nseed == 0) {
    return(list())
  }

  oseed <- get_seed()
  on.exit(set_seed(oseed))

  if(!is.null(seed)) {
    set.seed(seed)
  }

  seeds <- vector("list", length = nseed)
  seeds[[1]] <- parallel::nextRNGStream(get_lecuyer_cmrg_seed())
  for(i in seq_len(stream - 1)) {
    seeds[[1]] <- parallel::nextRNGStream(seeds[[1]])
  }
  for(i in seq_len(start_seed - 1)) {
    seeds[[1]] <- parallel::nextRNGSubStream(seeds[[1]])
  }
  for(i in seq_len(nseed-1)) {
    seeds[[i+1]] <- parallel::nextRNGSubStream(seeds[[i]])
  }
  seeds
}
  