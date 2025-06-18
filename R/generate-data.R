
ssd_generate_data <- function(x, ...) UseMethod("ssd_generate_data")

#' @export
ssd_generate_data.data.frame <- function(x, ..., replace = FALSE, nrow = 6L, nsims = 100L) {
  chk::check_dim(x, dim = base::nrow, values = c(5, 10000))
  chk::chk_flag(replace)
  chk::chk_count(nrow)
  chk::chk_gte(nrow, 5)
  chk::chk_lte(nrow, nrow(x))
  chk::chk_count(nsims)
  chk::chk_range(nsims, c(1, 10000))
  
  nsims |>
    seq_len() |>
    purrr::map(\(n) dplyr::slice_sample(x, n = nrow, replace = replace)) |>
    purrr::map(\(.x) dplyr::mutate(.x, row = seq_len(nrow))) |>
    purrr::map2(seq_len(nsims), \(.x, .y) dplyr::mutate(.x, sim = .y)) |>
    dplyr::bind_rows()
}

#' @export
ssd_generate_data.fitdists <- function(x, ..., dist = "top", nrow = 6L, nsims = 100L) {
  chk::chk_string(dist)
  chk::chk_subset(dist, c("multi", "top", names(x)))
  
  wch <- dist
  if(dist == "top") {
    weight <- ssdtools::glance(x, wt = TRUE)$wt
    wch <- which.max(weight)
  }
  
  ssd_generate_data(x[[wch]], nrow = nrow, nsims = nsims)
}

#' @export
ssd_generate_data.tmbfit <- function(x, ..., nrow = 6L, nsims = 100L) {
  pars <- ssdtools::estimates(x)
  x <- x$dist
  ssd_generate_data(x, pars = pars, nrow = nrow, nsims = nsims)
}

#' @export
ssd_generate_data.character <- function(x, ..., pars = list(), nrow = 6L, nsims = 100L) {
  chk::chk_string(x)
  chk::chk_subset(x, ssdtools::ssd_dists_all())
  chk::chk_list(pars)
  
  fun <- paste0("ssdtools::ssd_r", x)
  fun <- eval(parse(text = fun))
  
  ssd_generate_data(fun, args = as.list(pars), nrow = nrow, nsims = nsims)
}

#' @export
ssd_generate_data.function <- function(x, ..., args = list(), nrow = 6L, nsims = 100L) {
  chk::chk_list(args)
  chk::chk_count(nrow)
  chk::chk_range(nrow, c(5, 1000))
  chk::chk_count(nsims)
  chk::chk_range(nsims, c(1, 10000))
  
  args$n <- nrow
  
  nsims |>
    seq_len() |>
    purrr::map(\(n) do.call(x, args = args)) |>
    purrr::map(\(.x) dplyr::tibble(Conc = .x)) |>
    purrr::map(\(.x) dplyr::mutate(.x, row = seq_len(nrow))) |>
    purrr::map2(seq_len(nsims), \(.x, .y) dplyr::mutate(.x, sim = .y)) |>
    dplyr::bind_rows()
}
