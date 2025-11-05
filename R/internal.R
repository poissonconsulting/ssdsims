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

seq_up <- function(from, to) {
  if(to < from) return(integer())
  seq(from, to)
}

fit_dists_seed <- function(data, sim, stream, seed, dists, silent, ...) {
   seed <- get_lecuyer_cmrg_seed_stream(seed = seed, start_sim = sim, stream = stream)
  ## TODO: handle failure of all model to fit!!
  with_lecuyer_cmrg_seed(seed, {
    fit <- ssdtools::ssd_fit_dists(data, dists = dists, silent = silent, nrow = 5L, ...)
  })
  fit
}

hc_seed <- function(data, sim, stream, ci_method, seed, proportion, ci, save_to, ...) {
  seed <- get_lecuyer_cmrg_seed_stream(seed = seed, start_sim = sim, stream = stream)
  ## TODO: handle failures
  with_lecuyer_cmrg_seed(seed, {
    hc<- ssdtools::ssd_hc(data, proportion = proportion, ci = ci, ci_method = ci_method, ...)
  })
  dplyr::select(hc, !"ci_method") 
}

run_scenario <- function(x, ..., dists, proportion, ci, ci_method, .progress = .progress) {
  .args <- list(...)

  fit_dists_formals <- methods::formalArgs(ssdtools::ssd_fit_dists)
  hc_formals <- methods::formalArgs(utils::argsAnywhere("ssd_hc.fitdists"))

  .args_fit <- .args[names(.args) %in% fit_dists_formals]
  .args_hc <- .args[names(.args) %in% hc_formals]

  .args_unused <- names(.args[!names(.args) %in% c(fit_dists_formals, hc_formals)])
  .n <- length(.args_unused)

  if(.n) {
      chk::abort_chk("the following %n argument%s %r unrecognised: ", chk::cc(.args_unused), n = .n)
  }

  .args_fit <- list(x = x, dists = dists, .progress = .progress) |>
    c(.args_fit)

  x <- do.call("ssd_fit_dists_sims", .args_fit)

  .args_hc <- list(x = x, proportion = proportion, ci = ci, ci_method = ci_method, .progress = .progress) |>
    c(.args_hc)

  do.call("ssd_hc_sims", .args_hc)
}