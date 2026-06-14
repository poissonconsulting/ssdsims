# scenario-definition: nrow_max must be a whole number

    Code
      ssd_define_scenario(ssd_scenario_data(ssddata::ccme_boron), nsim = 2L, seed = 1L,
      nrow_max = 10.5)
    Condition
      Error in `ssd_define_scenario()`:
      ! `nrow_max` must be a whole number (non-missing integer scalar or double equivalent).

# scenario-definition: nrow exceeding nrow_max errors, citing nrow_max

    Code
      ssd_define_scenario(ssd_scenario_data(ssddata::ccme_boron), nsim = 2L, seed = 1L,
      nrow = 50L, nrow_max = 20L)
    Condition
      Error in `ssd_define_scenario()`:
      ! `nrow` must be between 5 and `nrow_max` (20), not 50.

# scenario-definition: an all-infeasible replace = FALSE grid aborts

    Code
      ssd_define_scenario(ssd_scenario_data(ssddata::ccme_boron), nsim = 1L, seed = 1L,
      nrow = 50L, replace = FALSE)
    Condition
      Error in `ssd_define_scenario()`:
      ! No feasible `sample` task: with `replace = FALSE`, every `nrow` (50) exceeds every dataset's effective draw size (`min(nrow_max, nrow(data))`), so the scenario would produce nothing.

# scenario-definition: min_pmix rejects a bare function

    Code
      ssd_define_scenario(data, nsim = 2L, seed = 1L, min_pmix = ssdtools::ssd_min_pmix)
    Condition
      Error in `ssd_define_scenario()`:
      ! `min_pmix` must be an `ssd_pmix()` collection; wrap the function(s) with `ssd_pmix()` (a bare function, a plain list, or a character vector is not accepted).

# scenario-definition: min_pmix rejects a plain list

    Code
      ssd_define_scenario(data, nsim = 2L, seed = 1L, min_pmix = list(ssdtools::ssd_min_pmix))
    Condition
      Error in `ssd_define_scenario()`:
      ! `min_pmix` must be an `ssd_pmix()` collection; wrap the function(s) with `ssd_pmix()` (a bare function, a plain list, or a character vector is not accepted).

# scenario-definition: min_pmix rejects a character vector of names

    Code
      ssd_define_scenario(data, nsim = 2L, seed = 1L, min_pmix = "ssd_min_pmix")
    Condition
      Error in `ssd_define_scenario()`:
      ! `min_pmix` must be an `ssd_pmix()` collection; wrap the function(s) with `ssd_pmix()` (a bare function, a plain list, or a character vector is not accepted).

# scenario-definition: an indirectly-supplied list value still aborts cleanly

    Code
      ssd_define_scenario(data, nsim = 2L, seed = 1L, min_pmix = fns)
    Condition
      Error in `ssd_define_scenario()`:
      ! `min_pmix` must be an `ssd_pmix()` collection; wrap the function(s) with `ssd_pmix()` (a bare function, a plain list, or a character vector is not accepted).

# scenario-definition: partition_by rejects an unknown axis

    Code
      ssd_define_scenario(ssd_scenario_data(ssddata::ccme_boron), nsim = 2L, seed = 1L,
      partition_by = list(sample = c("dataset", "nboot"), fit = c("dataset", "sim"),
      hc = c("dataset", "sim")))
    Condition
      Error in `ssd_define_scenario()`:
      ! `partition_by$sample` names unknown axis '"nboot"'; valid axes for the `sample` step are '"dataset"', '"sim"' and '"replace"'.

# scenario-definition: partition_by rejects ci as an hc axis

    Code
      ssd_define_scenario(ssd_scenario_data(ssddata::ccme_boron), nsim = 2L, seed = 1L,
      partition_by = list(hc = c("dataset", "sim", "ci")))
    Condition
      Error in `ssd_define_scenario()`:
      ! `partition_by$hc` names unknown axis '"ci"'; valid axes for the `hc` step are '"dataset"', '"sim"', '"replace"', '"nrow"', '"rescale"', '"computable"', '"at_boundary_ok"', '"min_pmix"', ... and '"distset"'.

# scenario-definition: bundle rejects ci as an hc axis

    Code
      ssd_define_scenario(ssd_scenario_data(ssddata::ccme_boron), nsim = 2L, seed = 1L,
      bundle = list(hc = "ci"))
    Condition
      Error in `ssd_define_scenario()`:
      ! `bundle$hc` names unknown axis '"ci"'; valid axes for the `hc` step are '"dataset"', '"sim"', '"replace"', '"nrow"', '"rescale"', '"computable"', '"at_boundary_ok"', '"min_pmix"', ... and '"distset"'.

# scenario-definition: partition_by rejects nrow under the sample step

    Code
      ssd_define_scenario(ssd_scenario_data(ssddata::ccme_boron), nsim = 2L, seed = 1L,
      partition_by = list(sample = c("dataset", "sim", "nrow"), fit = c("dataset",
        "sim", "nrow"), hc = c("dataset", "sim")))
    Condition
      Error in `ssd_define_scenario()`:
      ! `partition_by$sample` must not include "nrow": the `sample` step is the shared draw and carries no `nrow` axis (every `nrow` truncates the same draw inside the `fit` step).

# scenario-definition: partition_by rejects duplicate or NA axis names

    Code
      ssd_define_scenario(ssd_scenario_data(ssddata::ccme_boron), nsim = 2L, seed = 1L,
      partition_by = list(sample = c("dataset", "dataset"), fit = c("dataset", "sim"),
      hc = c("dataset", "sim")))
    Condition
      Error in `ssd_define_scenario()`:
      ! `partition_by$sample` must not contain duplicate axis names.

---

    Code
      ssd_define_scenario(ssd_scenario_data(ssddata::ccme_boron), nsim = 2L, seed = 1L,
      partition_by = list(sample = c("dataset", NA_character_), fit = c("dataset",
        "sim"), hc = c("dataset", "sim")))
    Condition
      Error in `ssd_define_scenario()`:
      ! `partition_by$sample` must not contain missing values.

# scenario-definition: a step named in both partition_by and bundle errors

    Code
      ssd_define_scenario(ssd_scenario_data(ssddata::ccme_boron), nsim = 2L, seed = 1L,
      partition_by = list(fit = c("dataset", "sim")), bundle = list(fit = c(
        "computable")))
    Condition
      Error in `ssd_define_scenario()`:
      ! Step '"fit"' named in both `partition_by` and `bundle`; give each step its path axes (`partition_by`) or its inner axes (`bundle`), not both.

# scenario-definition: a bare data frame is rejected

    Code
      ssd_define_scenario(ssddata::ccme_boron, nsim = 2L, seed = 1L)
    Condition
      Error in `ssd_define_scenario()`:
      ! `data` must be an `ssd_scenario_data()` collection; assemble data frames (and `ssd_gen()` generator datasets) with `ssd_scenario_data()`.

# scenario-definition: a bare list is rejected

    Code
      ssd_define_scenario(list(boron = ssddata::ccme_boron, cadmium = ssddata::ccme_cadmium),
      nsim = 2L, seed = 1L)
    Condition
      Error in `ssd_define_scenario()`:
      ! `data` must be an `ssd_scenario_data()` collection; assemble data frames (and `ssd_gen()` generator datasets) with `ssd_scenario_data()`.

# scenario-definition: the dropped name= argument is rejected

    Code
      ssd_define_scenario(ssd_scenario_data(ssddata::ccme_boron), nsim = 2L, seed = 1L,
      name = "boron_data")
    Condition
      Error in `ssd_define_scenario()`:
      ! `...` must be unused.

# scenario-definition: ci = FALSE rejects an explicit nboot

    Code
      ssd_define_scenario(ssd_scenario_data(ssddata::ccme_boron), nsim = 2L, seed = 1L,
      ci = FALSE, nboot = 500)
    Condition
      Error in `ssd_define_scenario()`:
      ! Bootstrap-only knob ('nboot') cannot be set when `ci = FALSE`. Set `ci = TRUE` to enable bootstrap, or omit the knob.

# scenario-definition: ci = FALSE rejects ci_method and parametric

    Code
      ssd_define_scenario(ssd_scenario_data(ssddata::ccme_boron), nsim = 2L, seed = 1L,
      ci = FALSE, ci_method = "MACL")
    Condition
      Error in `ssd_define_scenario()`:
      ! Bootstrap-only knob ('ci_method') cannot be set when `ci = FALSE`. Set `ci = TRUE` to enable bootstrap, or omit the knob.

---

    Code
      ssd_define_scenario(ssd_scenario_data(ssddata::ccme_boron), nsim = 2L, seed = 1L,
      ci = FALSE, parametric = FALSE)
    Condition
      Error in `ssd_define_scenario()`:
      ! Bootstrap-only knob ('parametric') cannot be set when `ci = FALSE`. Set `ci = TRUE` to enable bootstrap, or omit the knob.

# scenario-definition: a vector ci is rejected

    Code
      ssd_define_scenario(ssd_scenario_data(ssddata::ccme_boron), nsim = 2L, seed = 1L,
      ci = c(FALSE, TRUE))
    Condition
      Error in `ssd_define_scenario()`:
      ! `ci` must be a flag (TRUE or FALSE).

# scenario-definition: seed is required

    Code
      ssd_define_scenario(ssd_scenario_data(ssddata::ccme_boron), nsim = 2L)
    Condition
      Error in `ssd_define_scenario()`:
      ! `seed` must be supplied (a scalar whole number); it is the scenario's RNG root.

# scenario-definition: nsim is required

    Code
      ssd_define_scenario(ssd_scenario_data(ssddata::ccme_boron), seed = 1L)
    Condition
      Error in `ssd_define_scenario()`:
      ! `nsim` must be supplied (a count of the number of simulations).

# scenario-definition: invalid seed errors

    Code
      ssd_define_scenario(ssd_scenario_data(ssddata::ccme_boron), nsim = 2L, seed = c(
        1L, 2L))
    Condition
      Error in `ssd_define_scenario()`:
      ! `seed` must be a whole number (non-missing integer scalar or double equivalent).

---

    Code
      ssd_define_scenario(ssd_scenario_data(ssddata::ccme_boron), nsim = 2L, seed = 1.5)
    Condition
      Error in `ssd_define_scenario()`:
      ! `seed` must be a whole number (non-missing integer scalar or double equivalent).

---

    Code
      ssd_define_scenario(ssd_scenario_data(ssddata::ccme_boron), nsim = 2L, seed = NULL)
    Condition
      Error in `ssd_define_scenario()`:
      ! `seed` must be a whole number (non-missing integer scalar or double equivalent).

# scenario-definition: out-of-range nrow errors

    Code
      ssd_define_scenario(ssd_scenario_data(ssddata::ccme_boron), nsim = 2L, seed = 1L,
      nrow = 4L)
    Condition
      Error in `ssd_define_scenario()`:
      ! `nrow` must be between 5 and `nrow_max` (1000), not 4.

---

    Code
      ssd_define_scenario(ssd_scenario_data(ssddata::ccme_boron), nsim = 2L, seed = 1L,
      nrow = 1001L)
    Condition
      Error in `ssd_define_scenario()`:
      ! `nrow` must be between 5 and `nrow_max` (1000), not 1001.

# scenario-definition: print is stable for a single dataset

    Code
      ssd_define_scenario(ssd_scenario_data(ssddata::ccme_boron), nsim = 100L, seed = 42L,
      nrow = c(5L, 10L))
    Output
      <ssdsims_scenario>
        seed:     42
        nsim:     100
        datasets: ccme_boron
        nrow:     5, 10
        replace:  TRUE
        nrow_max: 1000 (setting)
        fit grid:
          rescale: FALSE
          computable: FALSE
          at_boundary_ok: TRUE
          min_pmix: ssd_min_pmix
          range_shape1: {0.05, 20}
          range_shape2: {0.05, 20}
          dists: gamma, lgumbel, llogis, lnorm, lnorm_lnorm, weibull (setting)
        hc grid:
          est_method: multi (setting)
          proportion: 0.05 (setting)
          ci: FALSE (setting)
          nboot: 1000
          ci_method: weighted_samples
          parametric: TRUE
          samples: FALSE (setting)
        distsets:
          BCANZ: gamma, lgumbel, llogis, lnorm, lnorm_lnorm, weibull
        partition_by:
          sample: dataset, sim, replace
          fit: dataset, sim, nrow, rescale
          hc: dataset, sim
        bundle:
          sample: 
          fit: replace, computable, at_boundary_ok, min_pmix, range_shape1, range_shape2
          hc: replace, nrow, rescale, computable, at_boundary_ok, min_pmix, range_shape1, range_shape2, nboot, ci_method, parametric, distset

# scenario-definition: print is stable for multiple datasets and vector knobs

    Code
      ssd_define_scenario(ssd_scenario_data(boron = ssddata::ccme_boron, cadmium = ssddata::ccme_cadmium),
      nsim = 50L, seed = 1L, nrow = c(5L, 6L, 10L), rescale = c(FALSE, TRUE),
      computable = c(FALSE, TRUE), at_boundary_ok = c(TRUE, FALSE), range_shape1 = list(
        c(0.05, 20), c(0.1, 10)), est_method = c("multi", "geometric"), proportion = c(
        0.05, 0.1), ci = TRUE, nboot = c(100, 1000), ci_method = c("weighted_samples",
        "MACL"), parametric = c(TRUE, FALSE))
    Output
      <ssdsims_scenario>
        seed:     1
        nsim:     50
        datasets: boron, cadmium
        nrow:     5, 6, 10
        replace:  TRUE
        nrow_max: 1000 (setting)
        fit grid:
          rescale: FALSE, TRUE
          computable: FALSE, TRUE
          at_boundary_ok: TRUE, FALSE
          min_pmix: ssd_min_pmix
          range_shape1: {0.05, 20; 0.1, 10}
          range_shape2: {0.05, 20}
          dists: gamma, lgumbel, llogis, lnorm, lnorm_lnorm, weibull (setting)
        hc grid:
          est_method: multi, geometric (setting)
          proportion: 0.05, 0.1 (setting)
          ci: TRUE (setting)
          nboot: 100, 1000
          ci_method: weighted_samples, MACL
          parametric: TRUE, FALSE
          samples: FALSE (setting)
        distsets:
          BCANZ: gamma, lgumbel, llogis, lnorm, lnorm_lnorm, weibull
        partition_by:
          sample: dataset, sim, replace
          fit: dataset, sim, nrow, rescale
          hc: dataset, sim
        bundle:
          sample: 
          fit: replace, computable, at_boundary_ok, min_pmix, range_shape1, range_shape2
          hc: replace, nrow, rescale, computable, at_boundary_ok, min_pmix, range_shape1, range_shape2, nboot, ci_method, parametric, distset

# scenario-definition: print is stable for generator and mixed inputs

    Code
      ssd_define_scenario(ssd_scenario_data(ssd_gen(synth = ssdtools::ssd_rlnorm, .n = 30,
      .seed = 1L)), nsim = 10L, seed = 42L)
    Output
      <ssdsims_scenario>
        seed:     42
        nsim:     10
        datasets: synth
        nrow:     6
        replace:  TRUE
        nrow_max: 1000 (setting)
        fit grid:
          rescale: FALSE
          computable: FALSE
          at_boundary_ok: TRUE
          min_pmix: ssd_min_pmix
          range_shape1: {0.05, 20}
          range_shape2: {0.05, 20}
          dists: gamma, lgumbel, llogis, lnorm, lnorm_lnorm, weibull (setting)
        hc grid:
          est_method: multi (setting)
          proportion: 0.05 (setting)
          ci: FALSE (setting)
          nboot: 1000
          ci_method: weighted_samples
          parametric: TRUE
          samples: FALSE (setting)
        distsets:
          BCANZ: gamma, lgumbel, llogis, lnorm, lnorm_lnorm, weibull
        partition_by:
          sample: dataset, sim, replace
          fit: dataset, sim, nrow, rescale
          hc: dataset, sim
        bundle:
          sample: 
          fit: replace, computable, at_boundary_ok, min_pmix, range_shape1, range_shape2
          hc: replace, nrow, rescale, computable, at_boundary_ok, min_pmix, range_shape1, range_shape2, nboot, ci_method, parametric, distset

---

    Code
      ssd_define_scenario(ssd_scenario_data(boron = ssddata::ccme_boron, !!!ssd_gen(
        synth = ssdtools::ssd_rlnorm, .n = 30, .seed = 1L)), nsim = 10L, seed = 42L)
    Output
      <ssdsims_scenario>
        seed:     42
        nsim:     10
        datasets: boron, synth
        nrow:     6
        replace:  TRUE
        nrow_max: 1000 (setting)
        fit grid:
          rescale: FALSE
          computable: FALSE
          at_boundary_ok: TRUE
          min_pmix: ssd_min_pmix
          range_shape1: {0.05, 20}
          range_shape2: {0.05, 20}
          dists: gamma, lgumbel, llogis, lnorm, lnorm_lnorm, weibull (setting)
        hc grid:
          est_method: multi (setting)
          proportion: 0.05 (setting)
          ci: FALSE (setting)
          nboot: 1000
          ci_method: weighted_samples
          parametric: TRUE
          samples: FALSE (setting)
        distsets:
          BCANZ: gamma, lgumbel, llogis, lnorm, lnorm_lnorm, weibull
        partition_by:
          sample: dataset, sim, replace
          fit: dataset, sim, nrow, rescale
          hc: dataset, sim
        bundle:
          sample: 
          fit: replace, computable, at_boundary_ok, min_pmix, range_shape1, range_shape2
          hc: replace, nrow, rescale, computable, at_boundary_ok, min_pmix, range_shape1, range_shape2, nboot, ci_method, parametric, distset

# scenario-definition: samples must be a flag

    Code
      ssd_define_scenario(ssd_scenario_data(ssddata::ccme_boron), nsim = 1L, seed = 1L,
      samples = c(TRUE, FALSE))
    Condition
      Error in `ssd_define_scenario()`:
      ! `samples` must be a flag (TRUE or FALSE).

