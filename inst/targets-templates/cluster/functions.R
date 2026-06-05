# Helper for the ssdsims cluster preflight, sourced by `preflight.R`.
#
# CONNECTIVITY + WORKER-PREREQUISITE PROBE (TARGETS-DESIGN.md section 4
# ingredient B). `preflight.R` runs this on a worker — inside one SLURM job —
# before the scenario pipeline, to verify the worker can actually run ssdsims.
# It aborts with an actionable message naming the failed check:
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
  # Witness: returned so the preflight can report which worker answered.
  list(node = node, r_version = r_version, time = Sys.time())
}
