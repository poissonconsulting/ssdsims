test_that("manifest: write/read round-trip preserves declarative fields", {
  scenario <- ssd_define_scenario(
    ssddata::ccme_boron,
    nsim = 2L,
    seed = 42L,
    rescale = c(TRUE, FALSE),
    ci = TRUE,
    nboot = 100L
  )
  dir <- withr::local_tempdir()
  ssd_write_manifest(scenario, dir)
  manifest <- ssd_read_manifest(dir)

  expect_identical(manifest$seed, scenario$seed)
  expect_identical(manifest$datasets, scenario$datasets)
  expect_identical(manifest$min_pmix, scenario$fit$min_pmix)
  expect_identical(manifest$partition_by, scenario$partition_by)
  expect_identical(manifest$fit$dists, scenario$fit$dists)
})

test_that("manifest: round-trip keeps seed/nboot whole and knobs logical", {
  scenario <- ssd_define_scenario(
    ssddata::ccme_boron,
    nsim = 1L,
    seed = 7L,
    rescale = c(TRUE, FALSE),
    ci = TRUE,
    parametric = FALSE,
    nboot = 250L
  )
  dir <- withr::local_tempdir()
  ssd_write_manifest(scenario, dir)
  manifest <- ssd_read_manifest(dir)

  expect_identical(manifest$seed, 7L)
  expect_identical(manifest$hc$nboot, 250L)
  expect_identical(manifest$fit$rescale, c(TRUE, FALSE))
  expect_identical(manifest$hc$parametric, FALSE)
})

test_that("manifest: records complete session info plus the version subset", {
  scenario <- ssd_define_scenario(ssddata::ccme_boron, nsim = 1L, seed = 1L)
  dir <- withr::local_tempdir()
  ssd_write_manifest(scenario, dir)
  manifest <- ssd_read_manifest(dir)

  expect_identical(manifest$r_version, as.character(getRversion()))
  expect_identical(
    manifest$dqrng_version,
    as.character(utils::packageVersion("dqrng"))
  )
  expect_identical(
    manifest$ssdtools_version,
    as.character(utils::packageVersion("ssdtools"))
  )
  expect_named(manifest$session_info, c("platform", "packages"))
  expect_true("ssdtools" %in% names(manifest$session_info$packages))
})

test_that("manifest: read errors when no manifest exists", {
  expect_snapshot(error = TRUE, {
    ssd_read_manifest("no-such-results-dir")
  })
})

test_that("manifest: ssd_record_shard writes a per-shard sidecar", {
  dir <- withr::local_tempdir()
  shard_dir <- file.path(dir, "fit", "dataset=boron", "sim=1", "rescale=FALSE")
  key <- "fit/dataset=boron/sim=1/rescale=FALSE"
  sha <- strrep("a", 64L)
  ssd_record_shard(shard_dir, key, sha)

  sidecar <- file.path(shard_dir, "meta.json")
  expect_true(file.exists(sidecar))
  record <- jsonlite::read_json(sidecar, simplifyVector = FALSE)
  expect_identical(record$partition_key, key)
  expect_identical(record$sha256, sha)
})

test_that("manifest: concurrent shard records write distinct sidecars", {
  dir <- withr::local_tempdir()
  keys <- c(
    "fit/dataset=boron/sim=1/rescale=FALSE",
    "fit/dataset=boron/sim=2/rescale=FALSE"
  )
  for (key in keys) {
    ssd_record_shard(file.path(dir, key), key, rlang::hash(key))
  }
  sidecars <- list.files(
    dir,
    pattern = "^meta\\.json$",
    recursive = TRUE
  )
  expect_length(sidecars, 2L)
})

test_that("manifest: assembler hashes shards that have no sidecar", {
  scenario <- ssd_define_scenario(ssddata::ccme_boron, nsim = 1L, seed = 1L)
  dir <- withr::local_tempdir()
  ssd_write_manifest(scenario, dir)

  key <- "fit/dataset=boron/sim=1/rescale=FALSE"
  parquet <- file.path(dir, key, "part.parquet")
  dir.create(dirname(parquet), recursive = TRUE)
  writeLines("shard-bytes", parquet)

  manifest <- ssd_assemble_manifest(dir)
  expect_named(manifest$completed_shards, key)
  expect_identical(
    manifest$completed_shards[[key]]$sha256,
    ssd_file_sha256(parquet)
  )
})

test_that("manifest: assembler prefers a shard's recorded sidecar", {
  dir <- withr::local_tempdir()
  key <- "fit/dataset=boron/sim=1/rescale=FALSE"
  parquet <- file.path(dir, key, "part.parquet")
  dir.create(dirname(parquet), recursive = TRUE)
  writeLines("shard-bytes", parquet)
  ssd_record_shard(dirname(parquet), key, strrep("a", 64L))

  manifest <- ssd_assemble_manifest(dir)
  entry <- manifest$completed_shards[[key]]
  expect_identical(entry$sha256, strrep("a", 64L))
  expect_false(identical(entry$sha256, ssd_file_sha256(parquet)))
})

test_that("manifest: re-assembly after new shards appear unions them in", {
  dir <- withr::local_tempdir()
  first <- "fit/dataset=boron/sim=1/rescale=FALSE"
  parquet1 <- file.path(dir, first, "part.parquet")
  dir.create(dirname(parquet1), recursive = TRUE)
  writeLines("shard-one", parquet1)
  assembled <- ssd_assemble_manifest(dir)$completed_shards
  expect_named(assembled, first)
  sha_first <- assembled[[first]]$sha256

  # Expand: a second shard's Parquet appears, then we assemble again.
  second <- "fit/dataset=boron/sim=2/rescale=FALSE"
  parquet2 <- file.path(dir, second, "part.parquet")
  dir.create(dirname(parquet2), recursive = TRUE)
  writeLines("shard-two", parquet2)

  reassembled <- ssd_assemble_manifest(dir)$completed_shards
  expect_setequal(names(reassembled), c(first, second))
  expect_identical(reassembled[[first]]$sha256, sha_first)
})

test_that("manifest: a shard with no Parquet is absent from completed_shards", {
  dir <- withr::local_tempdir()
  present <- "fit/dataset=boron/sim=1/rescale=FALSE"
  parquet <- file.path(dir, present, "part.parquet")
  dir.create(dirname(parquet), recursive = TRUE)
  writeLines("shard-bytes", parquet)
  # An absent shard: a sidecar recorded but no Parquet written.
  absent <- "fit/dataset=boron/sim=2/rescale=FALSE"
  ssd_record_shard(file.path(dir, absent), absent, strrep("c", 64L))

  manifest <- ssd_assemble_manifest(dir)
  expect_named(manifest$completed_shards, present)
})
