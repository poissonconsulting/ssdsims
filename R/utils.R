sim_seq <- function(start_sim, nsim) {
  chk::chk_count(nsim)
  chk::chk_count(start_sim)
  chk::chk_gt(start_sim)

  seq(start_sim, start_sim + nsim - 1L)
}

# Package-local null-coalescing operator. Base R only gained `%||%` in 4.4.0,
# but the package supports R >= 4.1, so define our own (same semantics as
# base/rlang) rather than relying on the base operator being present.
`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}

# sha256 over a file's bytes - the shared hash the manifest's per-shard recorder
# and `completed_shards` assembler key on (TARGETS-DESIGN.md section 8.5). One
# hash function for both paths so a recorded sha and a re-hash agree byte for
# byte.
ssd_file_sha256 <- function(path) {
  digest::digest(file = path, algo = "sha256")
}
