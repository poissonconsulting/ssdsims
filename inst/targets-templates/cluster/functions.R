# Helper functions for the ssdsims cluster targets pipeline, sourced by
# `_targets.R` (the same way it sources `scenario.R`). Kept out of `_targets.R`
# so that file stays a readable pipeline definition — the controller block, the
# scenario, the factory call, and the probe wiring — rather than carrying the
# probe body and the gating glue inline.
#
# Two helpers:
#   * `ssdsims_cluster_probe()` — the connectivity + worker-prerequisite check
#     run as a target on a worker (TARGETS-DESIGN.md §4 ingredient B).
#   * `gate_targets()`          — wires the probe in as an upstream dependency of
#     the scenario's entry-step shards.

# CONNECTIVITY + WORKER-PREREQUISITE PROBE (TARGETS-DESIGN.md §4 ingredient B).
#
# Run as a target on a worker — i.e. inside one SLURM job — before any scenario
# shard, it verifies the worker can actually run ssdsims and aborts with an
# actionable message naming the failed check:
#   * R resolves at the expected version  (else: module-load / install-path),
#   * `library(ssdsims)` loads             (else: the ManyLinux binary path),
#   * the scratch `tempdir()` is writable  (else: storage / scratch config).
# A no-job-dispatched failure surfaces separately as a crew launch error
# (queue/account wiring). It returns a small witness (worker R version + node
# id). It is a self-contained function (it does NOT assume ssdsims is loadable —
# that is one of the things it checks), so it ships intact in the worker globals.
ssdsims_cluster_probe <- function(expected = NULL) {
  node <- Sys.info()[["nodename"]]
  r_version <- as.character(getRversion())
  # (1) R version resolves as expected (module-load / install-path).
  if (!is.null(expected) && !startsWith(r_version, expected)) {
    stop(
      "Cluster probe FAILED [R version]: worker R is ",
      r_version,
      " but expected ",
      expected,
      ".x. Fix the `module load R/...` line in the ",
      "controller's `script_lines` (or set `expected_r_version <- NULL`).",
      call. = FALSE
    )
  }
  # (2) ssdsims loads on the worker (the ManyLinux binary install path).
  if (!requireNamespace("ssdsims", quietly = TRUE)) {
    stop(
      "Cluster probe FAILED [ssdsims unavailable]: the worker on node '",
      node,
      "' cannot load `ssdsims`. Fix the worker install/module path (the ",
      "ManyLinux binary path; see this template's README, backend B) so the ",
      "dependency tree resolves without compiling.",
      call. = FALSE
    )
  }
  # (3) scratch / tempdir() is writable (storage config).
  probe_file <- file.path(tempdir(), "ssdsims-cluster-probe.txt")
  ok <- tryCatch(
    {
      writeLines(node, probe_file)
      file.exists(probe_file)
    },
    error = function(e) FALSE
  )
  if (!isTRUE(ok)) {
    stop(
      "Cluster probe FAILED [scratch not writable]: cannot write to tempdir() '",
      tempdir(),
      "' on node '",
      node,
      "'. Fix the scratch path (the ",
      "`export TMPDIR=...` line in `script_lines`).",
      call. = FALSE
    )
  }
  unlink(probe_file)
  # Witness: returned so downstream targets can depend on a green probe.
  list(node = node, r_version = r_version, time = Sys.time())
}

# Gate the scenario shards on the probe: prepend a reference to `probe` to every
# `sample`-step target's command (the `sample` step is the pipeline entry, so
# gating it makes the probe a transitive upstream of every fit/hc/summary
# target). `tar_make()` therefore builds the probe FIRST, and a probe failure
# stops the scenario shards from running — the cluster-wiring/prerequisite
# problem surfaces, not an obscure scenario error. The factory itself is
# untouched; this only adds an edge from `probe` into the shards it already
# minted. It walks the (nested) `tar_map()` list the factory returns and rebuilds
# each `tar_target` with `{ probe; <original command> }`.
gate_targets <- function(node, probe_name = "probe") {
  if (inherits(node, "tar_target")) {
    gated <- as.call(list(
      as.name("{"),
      as.name(probe_name),
      node$command$expr[[1]]
    ))
    return(targets::tar_target_raw(
      node$settings$name,
      gated,
      format = node$settings$format,
      error = node$settings$error,
      cue = node$cue
    ))
  }
  if (is.list(node)) {
    return(lapply(node, gate_targets, probe_name = probe_name))
  }
  node
}
