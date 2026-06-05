# Cluster connectivity + worker-prerequisite PREFLIGHT (TARGETS-DESIGN.md
# section 4 ingredient B) — the crew labs' "login-node prerequisite checker".
#
# Run this BEFORE the scenario pipeline, to confirm your controller block
# (`controller.R`) matches the site and that a worker can actually run ssdsims
# — so a wiring/prerequisite problem surfaces here, not as an obscure failure
# part-way through the expensive scenario shards. `run.R` sources this for you
# before `tar_make()`; you can also run it on its own to confirm the mapping:
#   source("preflight.R")   # or, from a shell:  Rscript preflight.R
#
# It submits ONE probe task to a worker through the SAME controller `_targets.R`
# uses (i.e. one real SLURM job), reclaims it, and:
#   * on success prints a small witness (the worker's R version + node id);
#   * on failure aborts with an actionable message naming the failed check
#     (queue/account wiring, module/install path, or scratch storage).
# It is kept OUT of the main pipeline (`_targets.R`) so that stays a clean
# scenario-and-factory definition.

# Allow running from the project root, not just from this cluster directory.
if (dir.exists("inst/targets-templates/cluster")) {
  withr::local_dir(
    "inst/targets-templates/cluster",
    .local_envir = parent.frame(2)
  )
}

source("controller.R") # the one editable controller block (+ expected_r_version)

# CONNECTIVITY + WORKER-PREREQUISITE PROBE. Runs ON a worker (inside one SLURM
# job) and aborts with an actionable message naming the failed check:
#   * R resolves at the expected version  (else: module-load / install-path),
#   * `library(ssdsims)` loads             (else: the ManyLinux binary path),
#   * the scratch `tempdir()` is writable  (else: storage / scratch config).
# A no-job-dispatched failure surfaces separately as a crew launch error
# (queue/account wiring). It returns a small witness (worker R version + node
# id). It is self-contained (it does NOT assume ssdsims is loadable — that is
# one of the things it checks), so it ships intact in the worker globals.
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

message("Cluster preflight: submitting one probe job through the controller...")
controller$start()
on.exit(controller$terminate(), add = TRUE)

# Submit the probe to a worker (one SLURM job). The probe function and the
# expected R version travel as globals; the worker runs ssdsims_cluster_probe().
controller$push(
  command = ssdsims_cluster_probe(expected = expected_r_version),
  globals = list(
    ssdsims_cluster_probe = ssdsims_cluster_probe,
    expected_r_version = expected_r_version
  )
)
controller$wait()
out <- controller$pop()

if (is.null(out)) {
  stop(
    "Cluster preflight FAILED: no result returned from the worker. The job ",
    "was not dispatched or reclaimed — check the controller/queue/account ",
    "wiring in `controller.R`.",
    call. = FALSE
  )
}
if (!is.na(out$error)) {
  # The worker's stop() message (the actionable probe failure) is carried here.
  stop("Cluster preflight FAILED. ", out$error, call. = FALSE)
}

witness <- out$result[[1]]
message(
  "Cluster preflight OK: worker '",
  witness$node,
  "' running R ",
  witness$r_version,
  ", ssdsims loads, scratch writable."
)
