#' Normalise Input Data for a Simulation Scenario
#'
#' Coerces input to a tibble and validates the species sensitivity
#' distribution data contract: a numeric `Conc` column must be present.
#' This is the single entry point through which [ssd_define_scenario()]
#' forwards input data, so every later step inherits one validation path.
#'
#' Additional columns are preserved unchanged.
#'
#' @param data A data frame (or object coercible to one) with a numeric
#'   `Conc` column.
#' @return A tibble with a validated numeric `Conc` column and any
#'   additional columns preserved.
#' @export
#' @examples
#' ssd_data(ssddata::ccme_boron)
ssd_data <- function(data) {
  chk::chk_data(data)
  if (!"Conc" %in% names(data)) {
    chk::abort_chk(
      "`data` must have a column named `Conc`."
    )
  }
  data <- dplyr::as_tibble(data)
  chk::chk_numeric(data$Conc, x_name = "Column `Conc`")
  data
}
