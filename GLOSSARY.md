# ssdsims Glossary

Terminology used throughout `ssdsims`.

## RNG terms

- **seed**: A scalar integer passed to `base::set.seed()` to initialize the
  RNG (e.g. `42`). Exposed via the `seed` argument of `ssd_sim_data()`,
  `ssd_fit_dists_sims()`, `ssd_hc_sims()` and `ssd_run_scenario()`, and
  consumed by `with_lecuyer_cmrg_seed()` / `local_lecuyer_cmrg_seed()`.
- **state**: A `.Random.seed`-style integer vector representing the full
  internal state of the RNG. For L'Ecuyer-CMRG this is a length-7 integer
  vector (see `?parallel::nextRNGStream`, indexed in
  `?parallel::RNGstreams`). States are produced by
  `parallel::nextRNGStream()` / `parallel::nextRNGSubStream()` and by
  `get_lecuyer_cmrg_stream_state()` / `get_lecuyer_cmrg_stream_states()`;
  they are consumed by `with_lecuyer_cmrg_state()` /
  `local_lecuyer_cmrg_state()`. A state cannot be passed to
  `base::set.seed()` because `set.seed()` only consumes a single integer.
- **stream**: An independent sequence of pseudo-random numbers within the
  L'Ecuyer-CMRG RNG, advanced via `parallel::nextRNGStream()`. Streams are
  designed to be statistically independent across distinct values of the
  `stream` argument.
- **sub-stream**: A subdivision of a stream, advanced via
  `parallel::nextRNGSubStream()`. Each simulation (`sim`) within a given
  `stream` uses a distinct sub-stream.

## Simulation terms

- **`sim`**: The index of a simulation replicate within a `stream`.
- **`nsim`**: The number of simulation replicates to perform.
- **`start_sim`**: The starting `sim` index; sub-streams are advanced to
  this position before data is generated.
- **`stream` (argument)**: The index of the L'Ecuyer-CMRG stream to use;
  distinct `stream` values give statistically independent sequences.
- **`nrow`**: The number of rows (species) in each simulated dataset.
- **`replace`**: Whether the resampling that generates simulated data is
  performed with replacement.

## SSD terms

- **SSD**: Species sensitivity distribution: a distribution of species-level
  toxicity endpoints used in ecological risk assessment.
- **`dists`**: The parametric distributions fit to the SSD data (e.g.
  `lnorm`, `gamma`, `llogis`); see `ssdtools::ssd_fit_dists()`.
- **`fits`**: A `fitdists` object holding one or more fitted distributions.
- **`hc`**: Hazard concentration: a quantile of the SSD at a given
  `proportion` (e.g. `hc5` is the 5% quantile); see `ssdtools::ssd_hc()`.
- **`proportion`**: The proportion of species affected at which the hazard
  concentration is computed.
- **`ci`**: Whether to compute confidence intervals on hazard
  concentrations.
- **`ci_method`**: The method used to compute confidence intervals (e.g.
  `multi_fixed`, `weighted_samples`).
- **`nboot`**: The number of bootstrap replicates used when computing
  confidence intervals.
