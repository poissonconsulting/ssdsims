# Tasks: content-addressed-shards

> Prerequisite: `distset-hc-axis` (already makes `distset` an axis; `distset`
> enters the `draw` identity). The design layer that composes these
> content-addressed targets is the follow-up `scenario-combine-v2`.

## 1. Content-pure addressing on the single-scenario factory

- [ ] 1.1 In `R/task-shards.R` / `R/filepath.R`, derive each step's storage path
      from content: the `partition_by` layout cells plus, **at the step where it
      bites**, the scalar-setting discriminators (`nrow_max` at `sample`,
      `est_method` at the hc summary; `ci` as the superset projection). The
      discriminator is always present, never conditional on a comparison, and no
      scenario identity enters the path.
- [ ] 1.2 In `R/targets-runner.R`, name each `ssd_scenario_targets()` shard target
      by the same content function (cells + per-step discriminators), so the
      target name is a pure function of content with no scenario identity.
- [ ] 1.3 Confirm a single standalone scenario's per-task results are unchanged
      in **value** (byte-identical read-back), accepting the one-time **layout**
      change (settings now in the path).

## 2. Split `hc` into `draw` + `summarise`

- [ ] 2.1 In `R/hc-sims.R` / `R/shard-runner.R`, materialise the bootstrap sample
      set as a content-addressed `draw` shard keyed on
      `(fit identity, distset, nboot, ci_method, parametric)` — `distset` per
      `distset-hc-axis` (each pool bootstraps its own members), `est_method`
      excluded; the existing retained-samples persistence is this artifact. Gate
      it on `ci = TRUE`.
- [ ] 2.2 Add the RNG-free `summarise` step that reads the `draw` shard (and the
      `fit` shard for the analytic point estimate) and applies `est_method`/`ci`;
      address it with the `est_method` discriminator.
- [ ] 2.3 Wire the `draw → summarise` per-child edges into the factory's m:n
      fan-in; preserve the same-seed `est_method` identity property.

## 3. Tests

- [ ] 3.1 Addressing is a pure function of content: computing the target name and
      path for identical content via two scenario configs yields the **same**
      name and path; a setting that bites at a step appears in that step's
      address and **not** upstream; no scenario identity appears in any name or
      path.
- [ ] 3.2 hc split: one `draw` feeds many `est_method` summaries with no second
      bootstrap; the same-seed `est_method` identity holds.
- [ ] 3.3 Single-scenario byte-identity of values: per-task `sample`/`fit`/`hc`
      read-back values are unchanged from the pre-change pipeline (only the layout
      differs).

## 4. Documentation

- [ ] 4.1 Update `TARGETS-DESIGN.md` (§5/§6 addressing, §8 invalidation, the hc
      `draw`/`summarise` split) and `vignettes/sharded-pipeline.qmd`.
- [ ] 4.2 Roxygen for the content-pure factory and the `draw` artifact; regenerate
      `NAMESPACE`/`man/`.
- [ ] 4.3 `ROADMAP.md`: the chain `distset-hc-axis → content-addressed-shards →
      scenario-combine-v2`; note the foundation-for-follow-up relationship.
- [ ] 4.4 Format with `air`, run `devtools::check()`, `openspec validate
      --strict`.
