.onLoad <- function(libname, pkgname) {
  get_chk_exports <<- memoise::memoise(get_chk_exports)
}
