# scenario-definition: ssd_data requires a Conc column

    Code
      ssd_data(d = data.frame(x = 1:5))
    Condition
      Error in `ssd_data()`:
      ! Dataset `d` must have a column named `Conc`.

# scenario-definition: ssd_data rejects a non-numeric Conc column

    Code
      ssd_data(d = data.frame(Conc = c("a", "b")))
    Condition
      Error in `ssd_data()`:
      ! Dataset `d` must have a numeric `Conc` column.

# scenario-definition: ssd_data needs a derivable or explicit name

    Code
      ssd_data(data.frame(Conc = 1:5))
    Condition
      Error in `ssd_data()`:
      ! Unable to derive a name for dataset 1; supply a name (e.g. `ssd_data(boron = ...)`).

# scenario-definition: ssd_data rejects duplicate names

    Code
      ssd_data(x = ssddata::ccme_boron, x = ssddata::ccme_cadmium)
    Condition
      Error in `ssd_data()`:
      ! Dataset names must be unique.

# scenario-definition: min_pmix rejects non-function list elements

    Code
      ssd_define_scenario(ssddata::ccme_boron, nsim = 2L, seed = 1L, min_pmix = list(
        1))
    Condition
      Error in `ssd_define_scenario()`:
      ! Each `min_pmix` function must take a single argument (the number of rows).

# scenario-definition: min_pmix rejects multi-argument functions

    Code
      ssd_define_scenario(ssddata::ccme_boron, nsim = 2L, seed = 1L, min_pmix = function(
        a, b) 0.05)
    Condition
      Error in `ssd_define_scenario()`:
      ! Each `min_pmix` function must take a single argument (the number of rows).

# scenario-definition: min_pmix rejects duplicate names

    Code
      ssd_define_scenario(ssddata::ccme_boron, nsim = 2L, seed = 1L, min_pmix = list(
        a = ssdtools::ssd_min_pmix, a = ssdtools::ssd_min_pmix))
    Condition
      Error in `ssd_define_scenario()`:
      ! `min_pmix` names must be unique.

# scenario-accessors: an unresolvable min_pmix name fails fast

    Code
      ssd_define_scenario(ssddata::ccme_boron, nsim = 2L, seed = 1L, min_pmix = "no_such_fun")
    Condition
      Error in `ssd_define_scenario()`:
      ! Unable to resolve `min_pmix` name "no_such_fun" to a single-argument function.

# scenario-accessors: a name resolving to a multi-arg function fails fast

    Code
      ssd_define_scenario(ssddata::ccme_boron, nsim = 2L, seed = 1L, min_pmix = "two_arg")
    Condition
      Error in `ssd_define_scenario()`:
      ! Unable to resolve `min_pmix` name "two_arg" to a single-argument function.

# scenario-definition: partition_by rejects an unknown axis

    Code
      ssd_define_scenario(ssddata::ccme_boron, nsim = 2L, seed = 1L, partition_by = list(
        sample = c("dataset", "nboot"), fit = c("dataset", "sim"), hc = c("dataset",
          "sim")))
    Condition
      Error in `ssd_define_scenario()`:
      ! `partition_by$sample` names unknown axis '"nboot"'; valid axes for the `sample` step are '"dataset"', '"sim"' and '"replace"'.

# scenario-definition: partition_by rejects ci as an hc axis

    Code
      ssd_define_scenario(ssddata::ccme_boron, nsim = 2L, seed = 1L, partition_by = list(
        hc = c("dataset", "sim", "ci")))
    Condition
      Error in `ssd_define_scenario()`:
      ! `partition_by$hc` names unknown axis '"ci"'; valid axes for the `hc` step are '"dataset"', '"sim"', '"replace"', '"nrow"', '"rescale"', '"computable"', '"at_boundary_ok"', '"min_pmix"', ... and '"parametric"'.

# scenario-definition: bundle rejects ci as an hc axis

    Code
      ssd_define_scenario(ssddata::ccme_boron, nsim = 2L, seed = 1L, bundle = list(
        hc = "ci"))
    Condition
      Error in `ssd_define_scenario()`:
      ! `bundle$hc` names unknown axis '"ci"'; valid axes for the `hc` step are '"dataset"', '"sim"', '"replace"', '"nrow"', '"rescale"', '"computable"', '"at_boundary_ok"', '"min_pmix"', ... and '"parametric"'.

# scenario-definition: partition_by rejects nrow under the sample step

    Code
      ssd_define_scenario(ssddata::ccme_boron, nsim = 2L, seed = 1L, partition_by = list(
        sample = c("dataset", "sim", "nrow"), fit = c("dataset", "sim", "nrow"), hc = c(
          "dataset", "sim")))
    Condition
      Error in `ssd_define_scenario()`:
      ! `partition_by$sample` must not include "nrow": the `sample` step is the shared draw and carries no `nrow` axis (every `nrow` truncates the same draw inside the `fit` step).

# scenario-definition: partition_by rejects duplicate or NA axis names

    Code
      ssd_define_scenario(ssddata::ccme_boron, nsim = 2L, seed = 1L, partition_by = list(
        sample = c("dataset", "dataset"), fit = c("dataset", "sim"), hc = c("dataset",
          "sim")))
    Condition
      Error in `ssd_define_scenario()`:
      ! `partition_by$sample` must not contain duplicate axis names.

---

    Code
      ssd_define_scenario(ssddata::ccme_boron, nsim = 2L, seed = 1L, partition_by = list(
        sample = c("dataset", NA_character_), fit = c("dataset", "sim"), hc = c(
          "dataset", "sim")))
    Condition
      Error in `ssd_define_scenario()`:
      ! `partition_by$sample` must not contain missing values.

# scenario-definition: a step named in both partition_by and bundle errors

    Code
      ssd_define_scenario(ssddata::ccme_boron, nsim = 2L, seed = 1L, partition_by = list(
        fit = c("dataset", "sim")), bundle = list(fit = c("computable")))
    Condition
      Error in `ssd_define_scenario()`:
      ! Step '"fit"' named in both `partition_by` and `bundle`; give each step its path axes (`partition_by`) or its inner axes (`bundle`), not both.

# scenario-definition: ssd_data() collection plus name= is an error

    Code
      ssd_define_scenario(ssd_data(boron = ssddata::ccme_boron), nsim = 2L, seed = 1L,
      name = "x")
    Condition
      Error in `ssd_define_scenario()`:
      ! `name` must not be supplied when `data` is an `ssd_data()` collection.

# scenario-definition: duplicate dataset names in a list error

    Code
      ssd_define_scenario(list(x = ssddata::ccme_boron, x = ssddata::ccme_cadmium),
      nsim = 2L, seed = 1L)
    Condition
      Error in `ssd_define_scenario()`:
      ! Dataset names must be unique.

# scenario-definition: named list plus name= is an error

    Code
      ssd_define_scenario(list(boron = ssddata::ccme_boron), nsim = 2L, seed = 1L,
      name = "x")
    Condition
      Error in `ssd_define_scenario()`:
      ! `name` must not be supplied with a named list of datasets.

# scenario-definition: data frame literal with no derivable name errors

    Code
      ssd_define_scenario(data.frame(Conc = 1:5), nsim = 2L, seed = 1L)
    Condition
      Error in `ssd_define_scenario()`:
      ! Unable to derive a dataset name from the data argument; supply an explicit `name=` or use `ssd_data()`.

# scenario-definition: bad data in a list aborts via ssd_data

    Code
      ssd_define_scenario(list(good = ssddata::ccme_boron, bad = 1:5), nsim = 2L,
      seed = 1L)
    Condition
      Error in `ssd_define_scenario()`:
      ! Dataset `bad` must be a data frame.

# scenario-definition: ci = FALSE rejects an explicit nboot

    Code
      ssd_define_scenario(ssddata::ccme_boron, nsim = 2L, seed = 1L, ci = FALSE,
      nboot = 500)
    Condition
      Error in `ssd_define_scenario()`:
      ! Bootstrap-only knob ('nboot') cannot be set when `ci = FALSE`. Set `ci = TRUE` to enable bootstrap, or omit the knob.

# scenario-definition: ci = FALSE rejects ci_method and parametric

    Code
      ssd_define_scenario(ssddata::ccme_boron, nsim = 2L, seed = 1L, ci = FALSE,
      ci_method = "MACL")
    Condition
      Error in `ssd_define_scenario()`:
      ! Bootstrap-only knob ('ci_method') cannot be set when `ci = FALSE`. Set `ci = TRUE` to enable bootstrap, or omit the knob.

---

    Code
      ssd_define_scenario(ssddata::ccme_boron, nsim = 2L, seed = 1L, ci = FALSE,
      parametric = FALSE)
    Condition
      Error in `ssd_define_scenario()`:
      ! Bootstrap-only knob ('parametric') cannot be set when `ci = FALSE`. Set `ci = TRUE` to enable bootstrap, or omit the knob.

# scenario-definition: a vector ci is rejected

    Code
      ssd_define_scenario(ssddata::ccme_boron, nsim = 2L, seed = 1L, ci = c(FALSE,
        TRUE))
    Condition
      Error in `ssd_define_scenario()`:
      ! `ci` must be a flag (TRUE or FALSE).

# scenario-definition: seed is required

    Code
      ssd_define_scenario(ssddata::ccme_boron, nsim = 2L)
    Condition
      Error in `ssd_define_scenario()`:
      ! `seed` must be supplied (a scalar whole number); it is the scenario's RNG root.

# scenario-definition: nsim is required

    Code
      ssd_define_scenario(ssddata::ccme_boron, seed = 1L)
    Condition
      Error in `ssd_define_scenario()`:
      ! `nsim` must be supplied (a count of the number of simulations).

# scenario-definition: invalid seed errors

    Code
      ssd_define_scenario(ssddata::ccme_boron, nsim = 2L, seed = c(1L, 2L))
    Condition
      Error in `ssd_define_scenario()`:
      ! `seed` must be a whole number (non-missing integer scalar or double equivalent).

---

    Code
      ssd_define_scenario(ssddata::ccme_boron, nsim = 2L, seed = 1.5)
    Condition
      Error in `ssd_define_scenario()`:
      ! `seed` must be a whole number (non-missing integer scalar or double equivalent).

---

    Code
      ssd_define_scenario(ssddata::ccme_boron, nsim = 2L, seed = NULL)
    Condition
      Error in `ssd_define_scenario()`:
      ! `seed` must be a whole number (non-missing integer scalar or double equivalent).

# scenario-definition: out-of-range nrow errors

    Code
      ssd_define_scenario(ssddata::ccme_boron, nsim = 2L, seed = 1L, nrow = 4L)
    Condition
      Error in `ssd_define_scenario()`:
      ! `nrow` must be between 5 and 1000, not 4.

---

    Code
      ssd_define_scenario(ssddata::ccme_boron, nsim = 2L, seed = 1L, nrow = 1001L)
    Condition
      Error in `ssd_define_scenario()`:
      ! `nrow` must be between 5 and 1000, not 1001.

# scenario-definition: print is stable for a single dataset

    Code
      ssd_define_scenario(ssddata::ccme_boron, nsim = 100L, seed = 42L, nrow = c(5L,
        10L))
    Output
      <ssdsims_scenario>
        seed:     42
        nsim:     100
        datasets: ccme_boron
        nrow:     5, 10
        replace:  FALSE
        fit grid:
          rescale: FALSE
          computable: FALSE
          at_boundary_ok: TRUE
          min_pmix: ssd_min_pmix
          range_shape1: {0.05, 20}
          range_shape2: {0.05, 20}
          dists: gamma, lgumbel, llogis, lnorm, lnorm_lnorm, weibull (setting)
        hc grid:
          ci: FALSE (setting)
          nboot: 1000
          ci_method: weighted_samples
          parametric: TRUE
          est_method: multi (setting)
          proportion: 0.05 (setting)
          samples: FALSE (setting)
        partition_by:
          sample: dataset, sim, replace
          fit: dataset, sim, nrow, rescale
          hc: dataset, sim
        bundle:
          sample: 
          fit: replace, computable, at_boundary_ok, min_pmix, range_shape1, range_shape2
          hc: replace, nrow, rescale, computable, at_boundary_ok, min_pmix, range_shape1, range_shape2, nboot, ci_method, parametric

# scenario-definition: print is stable for multiple datasets and vector knobs

    Code
      ssd_define_scenario(list(boron = ssddata::ccme_boron, cadmium = ssddata::ccme_cadmium),
      nsim = 50L, seed = 1L, nrow = c(5L, 6L, 10L), rescale = c(FALSE, TRUE),
      computable = c(FALSE, TRUE), at_boundary_ok = c(TRUE, FALSE), range_shape1 = list(
        c(0.05, 20), c(0.1, 10)), proportion = c(0.05, 0.1), ci = TRUE, nboot = c(100,
        1000), est_method = c("multi", "geometric"), ci_method = c("weighted_samples",
        "MACL"), parametric = c(TRUE, FALSE))
    Output
      <ssdsims_scenario>
        seed:     1
        nsim:     50
        datasets: boron, cadmium
        nrow:     5, 6, 10
        replace:  FALSE
        fit grid:
          rescale: FALSE, TRUE
          computable: FALSE, TRUE
          at_boundary_ok: TRUE, FALSE
          min_pmix: ssd_min_pmix
          range_shape1: {0.05, 20; 0.1, 10}
          range_shape2: {0.05, 20}
          dists: gamma, lgumbel, llogis, lnorm, lnorm_lnorm, weibull (setting)
        hc grid:
          ci: TRUE (setting)
          nboot: 100, 1000
          ci_method: weighted_samples, MACL
          parametric: TRUE, FALSE
          est_method: multi, geometric (setting)
          proportion: 0.05, 0.1 (setting)
          samples: FALSE (setting)
        partition_by:
          sample: dataset, sim, replace
          fit: dataset, sim, nrow, rescale
          hc: dataset, sim
        bundle:
          sample: 
          fit: replace, computable, at_boundary_ok, min_pmix, range_shape1, range_shape2
          hc: replace, nrow, rescale, computable, at_boundary_ok, min_pmix, range_shape1, range_shape2, nboot, ci_method, parametric

# scenario-definition: samples must be a flag

    Code
      ssd_define_scenario(ssddata::ccme_boron, nsim = 1L, seed = 1L, samples = c(TRUE,
        FALSE))
    Condition
      Error in `ssd_define_scenario()`:
      ! `samples` must be a flag (TRUE or FALSE).

