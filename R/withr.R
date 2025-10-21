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
