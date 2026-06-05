# Self-contained benchmark for the `fit` step's blob-storage encoding
# (OpenSpec change `blob-storage-format`, task 1.3).
#
# Compares the lossless, string-column candidates for carrying a non-tabular
# `fitdists` object in the `fit` shard's Parquet `VARCHAR`:
#
#   (interim) ASCII serialise   - rawToChar(serialize(x, ascii = TRUE))
#   (a)       binary + base64   - base64(serialize(x, ascii = FALSE))
#   (d)       jsonlite JSON     - jsonlite::serializeJSON(x)
#   (+)       gzip + base64     - base64(memCompress(serialize(x), "gzip"))
#
# For each it reports:
#   * size_kb    - the in-R encoded string size (the value handed to duckplyr)
#   * disk_kb    - the on-disk size of a one-row `fit` shard Parquet carrying
#                  the blob (the *real* shard-size metric: Parquet applies its
#                  own column compression to the VARCHAR, so this is what the
#                  "blob layer dominates shard size" concern actually measures)
#   * encode_ms / decode_ms - mean encode/decode time over `reps`
#   * lossless   - whether `decode(encode(fit))` recovers the fit value-
#                  identically. A deserialised `fitdists` carries nil TMB
#                  pointers that `waldo` flags but `all.equal()` ignores
#                  (matching the oracle in tests/testthat/test-shard-runner.R),
#                  so `all.equal()` is the right lossless predicate here.
#
# Run from the package root with:
#   Rscript --no-environ openspec/changes/blob-storage-format/benchmark-blob-encoding.R

suppressMessages(devtools::load_all(quiet = TRUE))

# A representative `fitdists` built the same way the runner's oracle does, with
# several distributions so the blob is non-trivial.
scenario <- ssd_define_scenario(
  ssddata::ccme_boron,
  nsim = 1L,
  nrow = 6L,
  seed = 42L,
  dists = c("lnorm", "gamma", "llogis", "lgumbel")
)
fit <- ssd_run_scenario_baseline(scenario)$fit$fits[[1L]]

# ---- the candidate codecs --------------------------------------------------

enc_ascii <- function(x) {
  rawToChar(serialize(x, connection = NULL, ascii = TRUE))
}
dec_ascii <- function(s) unserialize(charToRaw(s))

enc_b64 <- function(x) {
  base64enc::base64encode(serialize(x, connection = NULL, ascii = FALSE))
}
dec_b64 <- function(s) unserialize(base64enc::base64decode(s))

# `serializeJSON()` returns a class-"json" character; strip the class so it is a
# plain VARCHAR value duckplyr can store (a bare classed vector is rejected).
enc_json <- function(x) as.character(jsonlite::serializeJSON(x))
dec_json <- function(s) jsonlite::unserializeJSON(s)

enc_gzb64 <- function(x) {
  base64enc::base64encode(memCompress(
    serialize(x, connection = NULL, ascii = FALSE),
    "gzip"
  ))
}
dec_gzb64 <- function(s) {
  unserialize(memDecompress(base64enc::base64decode(s), "gzip"))
}

codecs <- list(
  `interim ASCII VARCHAR` = list(enc = enc_ascii, dec = dec_ascii),
  `(a) binary + base64` = list(enc = enc_b64, dec = dec_b64),
  `(d) jsonlite JSON` = list(enc = enc_json, dec = dec_json),
  `(+) gzip + base64` = list(enc = enc_gzb64, dec = dec_gzb64)
)

# ---- size, on-disk size, time, and the byte-identity round-trip -----------

reps <- 50L

# `all.equal()` on a jsonlite round-trip recurses deep enough to overflow the
# node stack on a `fitdists`, so the lossless check is guarded - an error counts
# as not-lossless (the object did not round-trip cleanly).
is_lossless <- function(dec, s, fit) {
  tryCatch(isTRUE(all.equal(dec(s), fit)), error = function(e) FALSE)
}

disk_kb <- function(enc, fit) {
  tryCatch(
    {
      d <- tibble::tibble(fit_id = "f1", fit_blob = enc(fit))
      p <- tempfile(fileext = ".parquet")
      on.exit(unlink(p), add = TRUE)
      ssd_write_parquet(d, p)
      round(file.size(p) / 1024, 1)
    },
    error = function(e) NA_real_
  )
}

bench_one <- function(codec) {
  s <- codec$enc(fit)
  enc_t <- system.time(
    for (i in seq_len(reps)) {
      codec$enc(fit)
    }
  )[["elapsed"]]
  dec_t <- system.time(
    for (i in seq_len(reps)) {
      codec$dec(s)
    }
  )[["elapsed"]]
  data.frame(
    size_kb = round(nchar(s, type = "bytes") / 1024, 1),
    disk_kb = disk_kb(codec$enc, fit),
    encode_ms = round(1000 * enc_t / reps, 2),
    decode_ms = round(1000 * dec_t / reps, 2),
    lossless = is_lossless(codec$dec, s, fit)
  )
}

results <- do.call(
  rbind,
  lapply(names(codecs), function(nm) {
    cbind(encoding = nm, bench_one(codecs[[nm]]))
  })
)

cat("Representative fitdists dists:", paste(names(fit), collapse = ", "), "\n")
cat("In-memory object.size:", format(object.size(fit), units = "KB"), "\n\n")
print(results, row.names = FALSE)
