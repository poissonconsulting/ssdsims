
filename <- function(i, dist, prefix, ext = NULL, sep = "_") {
  paste0(prefix, sep, stringr::str_pad(i, width = 9, pad = "0"), sep, dist, ext)
}

filepath <- function(i, dist, save_to, prefix = "fit", ext = ".rds") {
  file.path(save_to, filename(i, dist, prefix = prefix, ext = ext))
}