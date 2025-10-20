

slice_sample_seed <- function(data, seed, n = nrow, replace = replace) {

  print(seed)
  data %<>%
    slice_sample(x, n = nrow, replace = replace)
}
