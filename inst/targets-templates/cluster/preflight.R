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
source("functions.R") # ssdsims_cluster_probe()

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
