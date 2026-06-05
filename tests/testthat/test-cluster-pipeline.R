# The cluster template needs a SLURM scheduler to *run*, so we cannot
# `tar_make()` it in CI. Instead we statically analyse the shipped template's
# target graph (no scheduler, no jobs dispatched) to assert the probe gates the
# scenario shards — the one piece of wiring this template adds around the shared
# factory.

test_that("cluster-pipeline: the shipped template gates every entry shard on the probe", {
  skip_targets()
  skip_if_not_installed("crew.cluster")
  template_dir <- system.file(
    "targets-templates",
    "cluster",
    package = "ssdsims"
  )
  skip_if(!nzchar(template_dir))
  dir <- withr::local_tempdir()
  # `_targets.R` sources `functions.R` (the probe + gating helper) and
  # `scenario.R`, so copy all three.
  file.copy(
    file.path(template_dir, c("_targets.R", "functions.R", "scenario.R")),
    dir
  )
  withr::local_dir(dir)

  # Static graph only (callr_function = NULL runs in-process; no SLURM jobs).
  net <- targets::tar_network(targets_only = TRUE, callr_function = NULL)
  vertices <- net$vertices$name
  edges <- net$edges

  # The probe is a target, and it gates every entry-step (`sample`) shard: an
  # edge runs from `probe` into each, so `tar_make()` builds the probe first and
  # a probe failure blocks the whole DAG.
  expect_true("probe" %in% vertices)
  sample_shards <- vertices[grepl("^sample_step_", vertices)]
  expect_gt(length(sample_shards), 0L)
  gated <- edges$to[edges$from == "probe"]
  expect_setequal(gated, sample_shards)
})
