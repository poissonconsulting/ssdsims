slice_sample_seed <- function(data, n, replace, seed) {
  with_lecuyer_cmrg_seed(seed, {
    data |>
      dplyr::slice_sample(n = n, replace = replace)
  })
}

do_call_seed <- function(what, args, seed) {
  with_lecuyer_cmrg_seed(seed, {
    do.call(what, args = args)
  })
}
