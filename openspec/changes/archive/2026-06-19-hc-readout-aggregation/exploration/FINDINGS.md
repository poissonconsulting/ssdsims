# Exploration: vectorising the hc demand reduction

Reframe the `group_split()` + per-cell `for` loop in `design_hc_assembly()`
(`R/design-targets.R`) as a vectorised reduction, optimised for the
majority-size-one case, exploring `duckplyr` / `.prudence = "stingy"`.

POC: `poc-summarise-reduction.R` (equivalence vs the current implementation on
five design shapes + benchmark).

## What the reframe is

1. **Group sizes** by `hc_id`, then a **singleton fast-path**: a cell touched by
   one member (`n == 1`) needs no reduction — the row *is* the cell, its demand
   already final. Only the multi-member cells are reduced.
2. **One grouped `summarise(.by = hc_id)`** over the (rare) multi-member cells
   does the set-unions (`list(sort(unique(unlist(...))))` for `proportion`/
   `est_method`, `any()` for `ci`/`samples`); the constant axis/primer columns
   rejoin from the first row per group. This replaces the explicit per-cell loop.
3. **`ci` routing as a join**: each `ci = FALSE` cell finds its serving
   `ci = TRUE` cell via a join on `(fit_id, distset)` + `first(hc_id)`, replacing
   the current `O(n_false × n_true)` nested scan.

## Findings

**Equivalence.** Byte-for-byte identical computed cells, serving maps, and
readout filters vs the current `design_hc_assembly()` on: readout-only
(`proportion`), ci-mix (+routing), `est_method`+`proportion`, distset (uniform
path), a large ci-mix, and a multi-`nboot` routing tie-break.

**Speed.** Median wall time for the assembly (the part that re-runs on every
`_targets.R` source, i.e. every `tar_make`/worker):

| all_hc rows | current (split+loop) | new (dplyr count) | new (duckplyr count) |
|-------------|----------------------|-------------------|----------------------|
| 1,260       | 1.17s                | 0.54s             | 0.58s                |
| 6,300       | 5.85s                | 2.39s             | 2.42s                |
| 25,200      | 25.3s                | 9.8s              | 9.4s                 |

~2.5× faster overall. For **ci-mix** designs specifically, the dominant win is
the routing join (the current nested scan is `O(n_false × n_true)` — ~29M
comparisons at 25k rows); every `hc_id` there is a singleton, so the reduction
fast-path also skips the `summarise` entirely.

**duckplyr / `.prudence = "stingy"` is a wash here — challenge to the premise.**
The count engine (duckplyr-stingy vs plain dplyr) makes no measurable difference,
and duckplyr is slightly *slower* at small scale (transfer/startup overhead). The
reason is structural: `all_hc` carries list-columns (`primer`, `range_shape1/2`,
and the per-member demand vectors), which **cannot originate in DuckDB** — the
primer needs R-side hashing (`task_primer()`). So `all_hc` is always fully
materialised in R *before* any count, leaving DuckDB nothing to protect: the
"stingy" memory guarantee (don't pull DuckDB intermediates into R) is moot when
the input already lives in R. Only the scalar `hc_id` column could cross over,
and counting one in-memory character column is equally cheap in dplyr.

`dd$` (a DuckDB-function pronoun that *could* express `array_agg(DISTINCT ...)`
list-aggregation entirely in DuckDB) does **not** exist in the pinned
`duckplyr` 1.2.1, so a "duckplyr-only" reduction of the vector demand is not
reachable on this version regardless.

## Recommendation

Adopt the **singleton fast-path + single `summarise` reduction + join-based
routing**, using **plain `dplyr` for the group-size count**. It is the clear,
evidence-backed win (~2.5×, cleaner code, no behavioural change) without taking
on the `as_duckdb_tibble()` round-trip that buys nothing in this architecture.

If the `duckplyr`/stingy pattern is wanted for consistency with the rest of the
codebase regardless of the wash, the count can use
`as_duckdb_tibble(all_hc["hc_id"], prudence = "stingy")` (POC `count_engine =
"duckplyr"`) — identical results, marginally slower at small scale.
