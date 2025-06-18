# Copyright 2015-2023 Province of British Columbia
# Copyright 2021 Environment and Climate Change Canada
# Copyright 2023-2024 Australian Government Department of Climate Change,
# Energy, the Environment and Water
#
#    Licensed under the Apache License, Version 2.0 (the "License");
#    you may not use this file except in compliance with the License.
#    You may obtain a copy of the License at
#
#       https://www.apache.org/licenses/LICENSE-2.0
#
#    Unless required by applicable law or agreed to in writing, software
#    distributed under the License is distributed on an "AS IS" BASIS,
#    WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
#    See the License for the specific language governing permissions and
#    limitations under the License.

local_multisession <- function(.local_envir = parent.frame(), workers = 2) {
  oldDoPar <- doFuture::registerDoFuture()
  withr::defer_parent(with(oldDoPar, foreach::setDoPar(fun = fun, data = data, info = info)))
  oldPlan <- future::plan("future::multisession", workers = workers)
  withr::defer_parent(future::plan(oldPlan))
  invisible(oldDoPar)
}

save_csv <- function(x) {
  path <- tempfile(fileext = ".csv")
  readr::write_csv(x, path)
  path
}

expect_snapshot_data <- function(x, name, digits = 6) {
  fun <- function(x) if(is.numeric(x)) signif(x, digits = digits) else x
  lapply_fun <- function(x) I(lapply(x, fun))
  x <- dplyr::mutate(x, dplyr::across(where(is.numeric), fun))
  x <- dplyr::mutate(x, dplyr::across(where(is.list), lapply_fun))
  path <- save_csv(x)
  testthat::expect_snapshot_file(
    path,
    paste0(name, ".csv"),
    compare = testthat::compare_file_text
  )
}