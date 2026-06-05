# The cluster template's connectivity/prerequisite check is a standalone
# preflight (`preflight.R`, which carries the probe body), kept OUT of the main
# pipeline so `_targets.R` stays a clean scenario-and-factory definition. We test
# the probe function directly (no scheduler) and assert the shipped `_targets.R`
# graph carries no probe target.

# Source the shipped `preflight.R` up to (not including) the controller calls and
# return its probe function. `preflight.R` sources `controller.R` (which builds a
# SLURM controller) and then submits a job, so we cannot source it wholesale off
# a cluster; instead we read it and evaluate just the `ssdsims_cluster_probe`
# definition.
cluster_probe_fun <- function() {
  template_dir <- system.file(
    "targets-templates",
    "cluster",
    package = "ssdsims"
  )
  testthat::skip_if(!nzchar(template_dir))
  lines <- readLines(file.path(template_dir, "preflight.R"))
  start <- grep("^ssdsims_cluster_probe <- function", lines)
  end <- grep("^\\}", lines)
  end <- min(end[end > start])
  env <- new.env()
  eval(parse(text = lines[start:end]), envir = env)
  env$ssdsims_cluster_probe
}

test_that("cluster-pipeline: the preflight probe returns a worker witness when prerequisites hold", {
  skip_if_not_installed("ssdsims")
  probe <- cluster_probe_fun()
  witness <- probe(expected = NULL)
  expect_named(witness, c("node", "r_version", "time"))
  expect_identical(witness$r_version, as.character(getRversion()))
  # A matching expected version also passes.
  major_minor <- paste(getRversion()[1, 1:2], collapse = ".")
  expect_named(probe(expected = major_minor), c("node", "r_version", "time"))
})

test_that("cluster-pipeline: the preflight probe aborts when the worker R version is wrong", {
  probe <- cluster_probe_fun()
  # The message embeds the worker's actual R version (machine-specific), so a
  # snapshot would be non-deterministic; match the stable failure prefix.
  expect_error(
    probe(expected = "0.0"),
    "Cluster probe FAILED \\[R version\\]"
  )
})

test_that("cluster-pipeline: the main pipeline carries no probe target", {
  skip_targets()
  skip_if_not_installed("crew.cluster")
  template_dir <- system.file(
    "targets-templates",
    "cluster",
    package = "ssdsims"
  )
  skip_if(!nzchar(template_dir))
  dir <- withr::local_tempdir()
  # `_targets.R` sources only `controller.R` (the scenario is inline).
  file.copy(file.path(template_dir, c("_targets.R", "controller.R")), dir)
  withr::local_dir(dir)
  net <- targets::tar_network(targets_only = TRUE, callr_function = NULL)
  vertices <- net$vertices$name
  # The pipeline is the clean scenario shards + summary; the probe is preflight,
  # not a pipeline target.
  expect_false("probe" %in% vertices)
  expect_gt(sum(grepl("^sample_step_", vertices)), 0L)
  expect_true("summary" %in% vertices)
})
