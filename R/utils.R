sim_seq <- function(start_sim, nsim) {
  chk::chk_count(nsim)
  chk::chk_count(start_sim)
  chk::chk_gt(start_sim)

  seq(start_sim, start_sim + nsim - 1L)
}
