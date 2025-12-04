get_chk_exports <- function() {
  chk_exports <- getNamespaceExports("chk")
  cli::cli_alert_info("Loaded {length(chk_exports)} chk package exports")
  chk_exports
}
