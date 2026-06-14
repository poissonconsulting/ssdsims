# `skip_targets()`, `numeric_dataset()`, and `setup_targets_fixture()`-style
# helpers live in `helper.R` / test-task-shards.R conventions.

# ---- constructors (task 4.1) -----------------------------------------------

test_that("cloud-upload: ssd_upload_azure() is classed and carries no credentials", {
  upload <- ssd_upload_azure(
    url = "https://acct.blob.core.windows.net",
    container = "ssdsims-results"
  )
  expect_s3_class(upload, "ssdsims_upload_azure_blob")
  expect_s3_class(upload, "ssdsims_upload")
  expect_identical(upload$url, "https://acct.blob.core.windows.net")
  expect_identical(upload$container, "ssdsims-results")
  # only the destination is stored - no secrets, connections, or environments
  expect_setequal(
    names(upload),
    c("url", "container", "prefix", "domain", "account")
  )
  expect_null(upload$prefix)
  # the storage account name is derived from the url, not the environment
  expect_identical(upload$account, "acct")
  expect_identical(upload$domain, "blob.core.windows.net")
})

test_that("cloud-upload: the storage account is derived from the url (+ domain)", {
  expect_identical(
    ssd_upload_azure("https://myacct.blob.core.windows.net", "c")$account,
    "myacct"
  )
  # a sovereign/non-public cloud via `domain`
  expect_identical(
    ssd_upload_azure(
      "https://gov.blob.core.usgovcloudapi.net",
      "c",
      domain = "blob.core.usgovcloudapi.net"
    )$account,
    "gov"
  )
  # a url whose host does not end with `.<domain>` aborts (account ambiguous)
  expect_snapshot(error = TRUE, {
    ssd_upload_azure("https://acct.example.com", "c")
  })
})

test_that("cloud-upload: ssd_upload_azure() takes an optional subdirectory prefix", {
  upload <- ssd_upload_azure(
    url = "https://acct.blob.core.windows.net",
    container = "ssdsims-results",
    prefix = "/study-2026/run-3/"
  )
  # leading/trailing slashes are trimmed
  expect_identical(upload$prefix, "study-2026/run-3")
  # a slashes-only prefix collapses to no prefix
  expect_null(
    ssd_upload_azure(
      "https://acct.blob.core.windows.net",
      "c",
      prefix = "///"
    )$prefix
  )
})

test_that("cloud-upload: ssd_upload_azure() forces prefix to be passed by name", {
  expect_snapshot(error = TRUE, {
    ssd_upload_azure("https://acct", "c", "study-2026")
  })
  expect_snapshot(error = TRUE, {
    ssd_upload_azure("https://acct", "c", prefx = "study-2026")
  })
})

test_that("cloud-upload: a prefix routes the blob key and the read-back glob", {
  bare <- ssd_upload_azure("https://acct.blob.core.windows.net", "results")
  pre <- ssd_upload_azure(
    "https://acct.blob.core.windows.net",
    "results",
    prefix = "study-2026/run-3"
  )
  key <- "sample/dataset=boron/sim=1/part.parquet"
  expect_identical(azure_blob_dest(bare, key), key)
  expect_identical(
    azure_blob_dest(pre, key),
    "study-2026/run-3/sample/dataset=boron/sim=1/part.parquet"
  )
  expect_identical(azure_glob(bare, "hc"), "az://results/hc/**/part.parquet")
  expect_identical(
    azure_glob(pre, "hc"),
    "az://results/study-2026/run-3/hc/**/part.parquet"
  )
  expect_identical(
    azure_glob(pre, "summary"),
    "az://results/study-2026/run-3/summary.parquet"
  )
  expect_identical(
    azure_glob(pre, "summary_samples"),
    "az://results/study-2026/run-3/summary-samples.parquet"
  )
})

test_that("cloud-upload: ssd_upload_dryrun() is classed and empty", {
  upload <- ssd_upload_dryrun()
  expect_s3_class(upload, "ssdsims_upload_dryrun")
  expect_s3_class(upload, "ssdsims_upload")
  expect_length(upload, 0L)
})

test_that("cloud-upload: ssd_upload_azure() validates its destination", {
  expect_snapshot(error = TRUE, {
    ssd_upload_azure(url = "", container = "c")
  })
  expect_snapshot(error = TRUE, {
    ssd_upload_azure(url = "https://acct", container = 1L)
  })
  expect_snapshot(error = TRUE, {
    ssd_upload_azure(
      url = "https://acct.blob.core.windows.net",
      container = "c",
      prefix = 1L
    )
  })
})

# ---- ssd_test_upload() (task 4.2) ------------------------------------------

test_that("cloud-upload: ssd_test_upload() on a dry run is trivially OK", {
  expect_null(ssd_test_upload(ssd_upload_dryrun()))
})

test_that("cloud-upload: ssd_test_upload() Azure names the missing credential", {
  # Mock the Suggests check so the credential resolution (not the absent
  # `AzureStor`) is what runs (test-suite AGENTS: wrap, do not use `.package`).
  local_mocked_bindings(azure_check_installed = function() invisible(NULL))
  upload <- ssd_upload_azure("https://acct.blob.core.windows.net", "results")
  withr::local_envvar(
    SSDSIMS_AZURE_STORAGE_KEY = NA,
    SSDSIMS_AZURE_STORAGE_SAS = NA,
    SSDSIMS_AZURE_TENANT_ID = NA,
    SSDSIMS_AZURE_CLIENT_ID = NA,
    SSDSIMS_AZURE_CLIENT_SECRET = NA
  )
  expect_snapshot(error = TRUE, {
    ssd_test_upload(upload)
  })
})

test_that("cloud-upload: no secret set names the auth options", {
  withr::local_envvar(
    SSDSIMS_AZURE_STORAGE_KEY = NA,
    SSDSIMS_AZURE_STORAGE_SAS = NA,
    SSDSIMS_AZURE_TENANT_ID = NA,
    SSDSIMS_AZURE_CLIENT_ID = NA,
    SSDSIMS_AZURE_CLIENT_SECRET = NA
  )
  expect_snapshot(error = TRUE, {
    resolve_azure_credentials()
  })
})

test_that("cloud-upload: credential resolution honours key/SAS/principal precedence", {
  # the account name is NOT read from the environment (it comes from the url)
  withr::with_envvar(
    c(SSDSIMS_AZURE_STORAGE_KEY = "k"),
    expect_identical(resolve_azure_credentials()$mode, "key")
  )
  withr::with_envvar(
    c(SSDSIMS_AZURE_STORAGE_KEY = NA, SSDSIMS_AZURE_STORAGE_SAS = "s"),
    expect_identical(resolve_azure_credentials()$mode, "sas")
  )
  withr::with_envvar(
    c(
      SSDSIMS_AZURE_STORAGE_KEY = NA,
      SSDSIMS_AZURE_STORAGE_SAS = NA,
      SSDSIMS_AZURE_TENANT_ID = "t",
      SSDSIMS_AZURE_CLIENT_ID = "c",
      SSDSIMS_AZURE_CLIENT_SECRET = "x"
    ),
    expect_identical(resolve_azure_credentials()$mode, "service_principal")
  )
})

# ---- ssd_upload_shard() (task 4.3) -----------------------------------------

test_that("cloud-upload: dry-run ssd_upload_shard() records a skip and returns the path", {
  path <- tempfile(fileext = ".parquet")
  file.create(path)
  expect_message(
    out <- ssd_upload_shard(path, ssd_upload_dryrun()),
    class = "ssdsims_upload_skip"
  )
  expect_identical(out, path)
})

test_that("cloud-upload: dry-run ssd_upload_shard() ships a vector of paths", {
  paths <- c(tempfile(fileext = ".parquet"), tempfile(fileext = ".parquet"))
  file.create(paths)
  skips <- character(0L)
  out <- withCallingHandlers(
    ssd_upload_shard(paths, ssd_upload_dryrun()),
    ssdsims_upload_skip = function(cnd) {
      skips <<- c(skips, conditionMessage(cnd))
      invokeRestart("muffleMessage")
    }
  )
  # one skip per file, in order, and the input vector returned unchanged
  expect_identical(out, paths)
  expect_length(skips, 2L)
  expect_match(skips[[1L]], paths[[1L]], fixed = TRUE)
  expect_match(skips[[2L]], paths[[2L]], fixed = TRUE)
})

test_that("cloud-upload: ssd_upload_shard() validates path as a character vector", {
  expect_snapshot(error = TRUE, {
    ssd_upload_shard(character(0L), ssd_upload_dryrun())
  })
  expect_snapshot(error = TRUE, {
    ssd_upload_shard(1L, ssd_upload_dryrun())
  })
})

test_that("cloud-upload: Azure ssd_upload_shard() with absent credentials fails loud", {
  local_mocked_bindings(azure_check_installed = function() invisible(NULL))
  path <- tempfile(fileext = ".parquet")
  file.create(path)
  upload <- ssd_upload_azure("https://acct.blob.core.windows.net", "results")
  withr::local_envvar(
    SSDSIMS_AZURE_STORAGE_KEY = NA,
    SSDSIMS_AZURE_STORAGE_SAS = NA,
    SSDSIMS_AZURE_TENANT_ID = NA,
    SSDSIMS_AZURE_CLIENT_ID = NA,
    SSDSIMS_AZURE_CLIENT_SECRET = NA
  )
  expect_snapshot(error = TRUE, {
    ssd_upload_shard(path, upload)
  })
  # a vector of paths aborts the same way, before any file is shipped (the
  # credentials are resolved once per call, ahead of the loop)
  expect_error(
    ssd_upload_shard(c(path, path), upload),
    "Azure credentials are incomplete"
  )
})

test_that("cloud-upload: unknown upload object aborts on every generic", {
  expect_snapshot(error = TRUE, {
    ssd_test_upload(list())
  })
  expect_snapshot(error = TRUE, {
    ssd_upload_shard("x", list())
  })
  expect_snapshot(error = TRUE, {
    ssd_open_uploaded(list(), "hc")
  })
})

# ---- the blob key / glob / secret helpers ----------------------------------

test_that("cloud-upload: the blob key is the path below the layout root", {
  expect_identical(
    upload_blob_key(
      "results/layout=abc/sample/dataset=boron/sim=1/part.parquet"
    ),
    "sample/dataset=boron/sim=1/part.parquet"
  )
})

test_that("cloud-upload: ssd_open_uploaded() builds the expected Hive glob", {
  upload <- ssd_upload_azure("https://acct.blob.core.windows.net", "results")
  expect_identical(azure_glob(upload, "hc"), "az://results/hc/**/part.parquet")
  expect_identical(
    azure_glob(upload, "sample"),
    "az://results/sample/**/part.parquet"
  )
  expect_identical(
    azure_glob(upload, "summary"),
    "az://results/summary.parquet"
  )
  expect_identical(
    azure_glob(upload, "summary_samples"),
    "az://results/summary-samples.parquet"
  )
})

test_that("cloud-upload: the DuckDB secret remaps the front-end credentials", {
  # the account is supplied separately (derived from the url), not from the env
  expect_match(
    azure_duckdb_secret_sql(list(mode = "key", key = "k"), "acct"),
    "TYPE azure, CONNECTION_STRING 'AccountName=acct;AccountKey=k'",
    fixed = TRUE
  )
})

# ---- ssd_open_uploaded() (task 4.7) ----------------------------------------

test_that("cloud-upload: ssd_open_uploaded() on a dry run has nothing to read back", {
  expect_snapshot(error = TRUE, {
    ssd_open_uploaded(ssd_upload_dryrun(), "hc")
  })
})

# ---- ssd_summarise_uploaded() ----------------------------------------------

test_that("cloud-upload: ssd_summarise_uploaded() on a dry run has nothing to summarise", {
  expect_snapshot(error = TRUE, {
    ssd_summarise_uploaded(ssd_upload_dryrun())
  })
})

test_that("cloud-upload: ssd_summarise_uploaded() aborts for an unknown destination", {
  expect_snapshot(error = TRUE, {
    ssd_summarise_uploaded(list())
  })
})

test_that("cloud-upload: ssd_summarise_uploaded() validates step and drop_samples first", {
  # validation runs before any credential/network work, so these abort with no
  # `SSDSIMS_AZURE_*` set (the live Azure round-trip is a manual/lab step)
  upload <- ssd_upload_azure("https://acct.blob.core.windows.net", "results")
  expect_snapshot(error = TRUE, {
    ssd_summarise_uploaded(upload, step = "nope")
  })
  expect_snapshot(error = TRUE, {
    ssd_summarise_uploaded(upload, drop_samples = "yes")
  })
})

test_that("cloud-upload: the summary steps pass validation on the read path", {
  skip_if_not_installed("duckplyr")
  upload <- ssd_upload_azure("https://acct.blob.core.windows.net", "results")
  withr::local_envvar(
    SSDSIMS_AZURE_STORAGE_KEY = NA,
    SSDSIMS_AZURE_STORAGE_SAS = NA,
    SSDSIMS_AZURE_TENANT_ID = NA,
    SSDSIMS_AZURE_CLIENT_ID = NA,
    SSDSIMS_AZURE_CLIENT_SECRET = NA
  )
  # the steps are accepted: the failure is the absent credentials (the live
  # in-place read is a manual/lab step), not the step domain
  expect_error(
    ssd_open_uploaded(upload, step = "summary_samples"),
    "Azure credentials are incomplete"
  )
  expect_error(
    ssd_summarise_uploaded(
      upload,
      step = "summary_samples",
      drop_samples = FALSE
    ),
    "Azure credentials are incomplete"
  )
  expect_error(
    ssd_summarise_uploaded(upload, step = "summary"),
    "Azure credentials are incomplete"
  )
})

test_that("cloud-upload: the compact summary cannot pretend to carry samples", {
  upload <- ssd_upload_azure("https://acct.blob.core.windows.net", "results")
  # no credentials set: the abort precedes any credential/extension work, so
  # the message is the summary_samples pointer, not a credentials error
  withr::local_envvar(
    SSDSIMS_AZURE_STORAGE_KEY = NA,
    SSDSIMS_AZURE_STORAGE_SAS = NA,
    SSDSIMS_AZURE_TENANT_ID = NA,
    SSDSIMS_AZURE_CLIENT_ID = NA,
    SSDSIMS_AZURE_CLIENT_SECRET = NA
  )
  expect_snapshot(error = TRUE, {
    ssd_summarise_uploaded(upload, step = "summary", drop_samples = FALSE)
  })
})

# ---- the factory wiring (task 4.4) -----------------------------------------

all_target_names <- function(x) {
  if (inherits(x, "tar_target")) {
    return(x$settings$name)
  }
  if (is.list(x)) {
    return(unlist(lapply(x, all_target_names), use.names = FALSE))
  }
  character(0L)
}

test_that("cloud-upload: upload = NULL emits no upload_<step> targets", {
  skip_if_not_installed("targets")
  skip_if_not_installed("tarchetypes")
  scenario <- ssd_define_scenario(
    ssd_scenario_data(ssddata::ccme_boron),
    nsim = 2L,
    seed = 42L,
    nrow = 6L,
    dists = ssd_distset(lnorm = "lnorm")
  )
  names <- all_target_names(ssd_scenario_targets(scenario))
  expect_length(grep("^upload_", names), 0L)
})

test_that("cloud-upload: a dry-run upload pairs one upload target per shard", {
  skip_if_not_installed("targets")
  skip_if_not_installed("tarchetypes")
  scenario <- ssd_define_scenario(
    ssd_scenario_data(ssddata::ccme_boron),
    nsim = 2L,
    seed = 42L,
    nrow = 6L,
    dists = ssd_distset(lnorm = "lnorm")
  )
  tg <- ssd_scenario_targets(scenario, upload = ssd_upload_dryrun())
  names <- all_target_names(tg)
  for (step in c("sample", "fit", "hc")) {
    shards <- switch(
      step,
      sample = ssd_scenario_sample_shards(scenario),
      fit = ssd_scenario_fit_shards(scenario),
      hc = ssd_scenario_hc_shards(scenario)
    )
    expect_length(grep(paste0("^", step, "_step_"), names), nrow(shards))
    expect_length(grep(paste0("^upload_", step, "_"), names), nrow(shards))
  }
})

find_target <- function(x, name) {
  if (inherits(x, "tar_target")) {
    if (identical(x$settings$name, name)) {
      return(x)
    }
    return(NULL)
  }
  if (is.list(x)) {
    for (el in x) {
      hit <- find_target(el, name)
      if (!is.null(hit)) {
        return(hit)
      }
    }
  }
  NULL
}

test_that("cloud-upload: a non-NULL upload pairs the summary with one upload_summary", {
  skip_if_not_installed("targets")
  skip_if_not_installed("tarchetypes")
  scenario <- ssd_define_scenario(
    ssd_scenario_data(ssddata::ccme_boron),
    nsim = 2L,
    seed = 42L,
    nrow = 6L,
    dists = ssd_distset(lnorm = "lnorm")
  )
  # upload = NULL: no summary upload node
  expect_false(
    "upload_summary" %in% all_target_names(ssd_scenario_targets(scenario))
  )
  az <- ssd_upload_azure("https://acct.blob.core.windows.net", "results")
  for (upload in list(ssd_upload_dryrun(), az)) {
    tg <- ssd_scenario_targets(scenario, upload = upload)
    names <- all_target_names(tg)
    expect_length(which(names == "upload_summary"), 1L)
    target <- find_target(tg, "upload_summary")
    expect_identical(target$settings$format, "file")
    expect_identical(target$settings$error, "null")
  }
})

test_that("cloud-upload: the factory never runs the ssd_test_upload() probe", {
  skip_if_not_installed("targets")
  skip_if_not_installed("tarchetypes")
  scenario <- ssd_define_scenario(
    ssd_scenario_data(ssddata::ccme_boron),
    nsim = 2L,
    seed = 42L,
    nrow = 6L,
    dists = ssd_distset(lnorm = "lnorm")
  )
  # An Azure destination with no credentials present: if the factory probed, this
  # would abort. It must not - building the target list does no network I/O.
  az <- ssd_upload_azure("https://acct.blob.core.windows.net", "results")
  probed <- FALSE
  local_mocked_bindings(
    ssd_test_upload = function(upload) {
      probed <<- TRUE
      invisible(NULL)
    }
  )
  expect_no_error(ssd_scenario_targets(scenario, upload = az))
  expect_false(probed)
})

test_that("cloud-upload: root, upload, and cue must be passed by name", {
  scenario <- ssd_define_scenario(
    ssd_scenario_data(ssddata::ccme_boron),
    nsim = 1L,
    seed = 42L
  )
  expect_snapshot(error = TRUE, {
    ssd_scenario_targets(scenario, "results")
  })
  expect_snapshot(error = TRUE, {
    ssd_scenario_targets(scenario, uplaod = ssd_upload_dryrun())
  })
})

test_that("cloud-upload: a non-upload object aborts in the factory", {
  scenario <- ssd_define_scenario(
    ssd_scenario_data(ssddata::ccme_boron),
    nsim = 1L,
    seed = 42L
  )
  expect_snapshot(error = TRUE, {
    ssd_scenario_targets(scenario, upload = list())
  })
})

# ---- upload mode does not change the shard commands (task 4.5) -------------

test_that("cloud-upload: the step shard commands are identical across upload modes", {
  skip_if_not_installed("targets")
  skip_if_not_installed("tarchetypes")
  scenario <- ssd_define_scenario(
    ssd_scenario_data(ssddata::ccme_boron),
    nsim = 2L,
    seed = 42L,
    nrow = 6L,
    dists = ssd_distset(lnorm = "lnorm")
  )
  az <- ssd_upload_azure("https://acct.blob.core.windows.net", "results")
  # No probe to mock: the factory never runs `ssd_test_upload()`, so building the
  # target list does no network I/O even for an Azure destination without creds.
  step_command <- function(tg, step) {
    tg[[switch(step, sample = 1L, fit = 2L, hc = 3L)]][[paste0(
      step,
      "_step"
    )]][[
      1L
    ]]$command$expr
  }
  tg_null <- ssd_scenario_targets(scenario)
  tg_dry <- ssd_scenario_targets(scenario, upload = ssd_upload_dryrun())
  tg_az <- ssd_scenario_targets(scenario, upload = az)
  for (step in c("sample", "fit", "hc")) {
    expect_identical(step_command(tg_null, step), step_command(tg_dry, step))
    expect_identical(step_command(tg_null, step), step_command(tg_az, step))
  }
  # the summary fan-in's command is identical too: the upload mode adds the
  # `upload_summary` pairing but never alters what (or where) the summary writes,
  # so the summary file bytes are identical across the three modes
  summary_command <- function(tg) find_target(tg, "summary")$command$expr
  expect_identical(summary_command(tg_null), summary_command(tg_dry))
  expect_identical(summary_command(tg_null), summary_command(tg_az))
})

# ---- content-hash skip on re-drive (task 4.6) ------------------------------

test_that("cloud-upload: a re-driven dry-run re-uploads no unchanged shard", {
  skip_targets()
  dir <- withr::local_tempdir()
  saveRDS(numeric_dataset(), file.path(dir, "data.rds"))
  saveRDS(1L, file.path(dir, "nsim.rds"))
  file.copy(
    test_path("fixtures", "cloud-upload-dryrun-targets.R"),
    file.path(dir, "_targets.R")
  )
  withr::local_dir(dir)
  tar_make_local()
  # the no-op upload targets - the summary's included - ran the first time
  meta <- targets::tar_meta(fields = "error")
  upload_meta <- meta[grepl("^upload_", meta$name), ]
  expect_gt(nrow(upload_meta), 0L)
  expect_true("upload_summary" %in% upload_meta$name)
  expect_true(all(is.na(upload_meta$error)))

  # re-drive with nothing changed: no upload target re-runs
  tar_make_local()
  progress <- targets::tar_progress()
  reran <- progress$name[progress$progress %in% c("completed", "started")]
  expect_length(grep("^upload_", reran), 0L)
})

test_that("cloud-upload: extending nsim uploads only the new shard", {
  skip_targets()
  dir <- withr::local_tempdir()
  saveRDS(numeric_dataset(), file.path(dir, "data.rds"))
  saveRDS(1L, file.path(dir, "nsim.rds"))
  file.copy(
    test_path("fixtures", "cloud-upload-dryrun-targets.R"),
    file.path(dir, "_targets.R")
  )
  withr::local_dir(dir)
  tar_make_local()

  # grow nsim by one sim: only the new sim's shards (and their upload pair) run
  saveRDS(2L, "nsim.rds")
  tar_make_local()
  progress <- targets::tar_progress()
  completed <- progress$name[progress$progress == "completed"]
  new_uploads <- grep("^upload_", completed, value = TRUE)
  # the summary grew (its bytes changed), so it re-shipped
  expect_true("upload_summary" %in% new_uploads)
  # every re-run *shard* upload is for the newly added sim (sim=2), not sim=1
  new_shard_uploads <- setdiff(new_uploads, "upload_summary")
  expect_gt(length(new_shard_uploads), 0L)
  expect_true(all(grepl("_2(_|$)", new_shard_uploads)))
})

test_that("cloud-upload: the summary ships and re-ships only when its bytes change", {
  skip_targets()
  dir <- withr::local_tempdir()
  saveRDS(numeric_dataset(), file.path(dir, "data.rds"))
  saveRDS(1L, file.path(dir, "nsim.rds"))
  saveRDS(TRUE, file.path(dir, "samples.rds"))
  file.copy(
    test_path("fixtures", "cloud-upload-summary-targets.R"),
    file.path(dir, "_targets.R")
  )
  withr::local_dir(dir)
  tar_make_local()
  # with samples = TRUE the summary target emits both files and upload_summary
  # ships (dry-run: skip-records) the pair, returning the local paths unchanged
  uploaded <- targets::tar_read(upload_summary)
  expect_identical(
    basename(uploaded),
    c("summary.parquet", "summary-samples.parquet")
  )
  expect_true(all(file.exists(uploaded)))

  # re-drive with nothing changed: the summary is not re-shipped
  tar_make_local()
  progress <- targets::tar_progress()
  reran <- progress$name[progress$progress %in% c("completed", "started")]
  expect_false("upload_summary" %in% reran)
})
