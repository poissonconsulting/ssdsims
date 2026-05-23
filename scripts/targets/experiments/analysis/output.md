# Output correctness — `ssdsims` targets experiments


- [Per-config row counts](#per-config-row-counts)
- [Cross-split agreement](#cross-split-agreement)
- [Samples list-column](#samples-list-column)
- [Summary](#summary)

The three split configs ran the same atomic fit grid; each wrote one
parquet file per `fit_dists_seed()` call to
`data/<config_id>/fit-*.parquet`. Branches differ in how many fits they
bundle, but the *combined* output should be identical across splits —
only the packaging differs.

## Per-config row counts

| split   | n_branches | n_atomic_fits | n_hc_per_fit | n_rows |
|:--------|-----------:|--------------:|-------------:|-------:|
| by_nrow |          3 |             6 |            4 |     24 |
| by_sim  |          2 |             6 |            4 |     24 |
| atomic  |          6 |             6 |            4 |     24 |

Each atomic fit task runs `n_hc_per_fit` HC calls. With
`est_method = "multi"`, every
`(nrow, sim, ci_method, nboot, proportion, dist)` tuple contributes one
row to the output:

| split   | n_rows | expected | delta |
|:--------|-------:|---------:|------:|
| by_nrow |     24 |       24 |     0 |
| by_sim  |     24 |       24 |     0 |
| atomic  |     24 |       24 |     0 |

`delta` may be non-zero if some distributions don’t yield rows for
particular configurations (the HC table includes one row per fitted
distribution × HC combo). It should be constant across splits.

If `delta` is zero everywhere, splitting is row-count-preserving.

## Cross-split agreement

For every `(sim, nrow, ci_method, nboot, proportion)` key the
`est`/`lcl`/`ucl` columns should match across splits — seeds are
deterministic, and the work per atomic unit doesn’t depend on how it’s
batched.

| metric | ref     | other  | max_abs_diff |
|:-------|:--------|:-------|-------------:|
| est    | by_nrow | by_sim |            0 |
| est    | by_nrow | atomic |            0 |
| lcl    | by_nrow | by_sim |            0 |
| lcl    | by_nrow | atomic |            0 |
| ucl    | by_nrow | by_sim |            0 |
| ucl    | by_nrow | atomic |            0 |

Zero (or floating-point-zero, e.g. `< 1e-10`) means the splits agree
exactly on the point estimate and CI bounds.

## Samples list-column

| split   | nboot | ci_method        |   n | uniq_lens |
|:--------|------:|:-----------------|----:|:----------|
| atomic  |     1 | multi_fixed      |   6 | 1         |
| atomic  |     1 | weighted_samples |   6 | 1         |
| atomic  |     5 | multi_fixed      |   6 | 5         |
| atomic  |     5 | weighted_samples |   6 | 5         |
| by_nrow |     1 | multi_fixed      |   6 | 1         |
| by_nrow |     1 | weighted_samples |   6 | 1         |
| by_nrow |     5 | multi_fixed      |   6 | 5         |
| by_nrow |     5 | weighted_samples |   6 | 5         |
| by_sim  |     1 | multi_fixed      |   6 | 1         |
| by_sim  |     1 | weighted_samples |   6 | 1         |
| by_sim  |     5 | multi_fixed      |   6 | 5         |
| by_sim  |     5 | weighted_samples |   6 | 5         |

`uniq_lens` is the set of sample-vector lengths observed at each
`(nboot, ci_method)`. The samples vector either equals `nboot` (most CI
methods) or `nboot × #dists` (multi methods); we expect this to be
identical across splits.

## Summary

If row counts match, split-vs-split estimate diff is floating-point
zero, and samples lengths are consistent, then split granularity is
**purely an infrastructure choice**: the answer doesn’t depend on how
the work was chopped up.
