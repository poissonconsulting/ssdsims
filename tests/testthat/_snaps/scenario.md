# scenario: print is stable for a single dataset

    Code
      s <- ssd_define_scenario(ssddata::ccme_boron, nsim = 100L, nrow = c(5L, 10L),
      seed = 42L)
      print(s)
    Output
      <ssdsims_scenario>
        seed:     42
        datasets: ccme_boron
        nsim:     100
        nrow:     5, 10
        fit grid:
          dists: gamma, lgumbel, llogis, lnorm, lnorm_lnorm, weibull
          rescale: FALSE
          computable: FALSE
          at_boundary_ok: TRUE
          min_pmix: ssd_min_pmix
          range_shape1: {0.05, 20}
          range_shape2: {0.05, 20}
        hc grid:
          proportion: 0.05
          ci: FALSE
          nboot: 1000
          est_method: multi
          ci_method: weighted_samples
          parametric: TRUE

# scenario: print is stable for multiple datasets and vector knobs

    Code
      s <- ssd_define_scenario(list(boron = ssddata::ccme_boron, cadmium = ssddata::ccme_cadmium),
      nsim = 50L, nrow = c(5L, 6L, 10L), seed = 1L, rescale = c(FALSE, TRUE),
      computable = c(FALSE, TRUE), at_boundary_ok = c(TRUE, FALSE), range_shape1 = list(
        c(0.05, 20), c(0.1, 10)), proportion = c(0.05, 0.1), ci = c(FALSE, TRUE),
      nboot = c(100, 1000), est_method = c("multi", "geometric"), ci_method = c(
        "weighted_samples", "MACL"), parametric = c(TRUE, FALSE))
      print(s)
    Output
      <ssdsims_scenario>
        seed:     1
        datasets: boron, cadmium
        nsim:     50
        nrow:     5, 6, 10
        fit grid:
          dists: gamma, lgumbel, llogis, lnorm, lnorm_lnorm, weibull
          rescale: FALSE, TRUE
          computable: FALSE, TRUE
          at_boundary_ok: TRUE, FALSE
          min_pmix: ssd_min_pmix
          range_shape1: {0.05, 20; 0.1, 10}
          range_shape2: {0.05, 20}
        hc grid:
          proportion: 0.05, 0.1
          ci: FALSE, TRUE
          nboot: 100, 1000
          est_method: multi, geometric
          ci_method: weighted_samples, MACL
          parametric: TRUE, FALSE

