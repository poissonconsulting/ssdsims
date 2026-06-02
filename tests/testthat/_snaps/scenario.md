# scenario: print is stable for a single dataset

    Code
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
          min_pmix: {<fn>}
          range_shape1: {0.05, 20}
          range_shape2: {0.05, 20}
        hc grid:
          proportion: 0.05
          ci: FALSE
          nboot: 1000
          est_method: multi
          ci_method: weighted_samples
          parametric: TRUE

# scenario: print is stable for multiple datasets

    Code
      print(s)
    Output
      <ssdsims_scenario>
        seed:     1
        datasets: boron, cadmium
        nsim:     50
        nrow:     6
        fit grid:
          dists: gamma, lgumbel, llogis, lnorm, lnorm_lnorm, weibull
          rescale: FALSE
          computable: FALSE
          at_boundary_ok: TRUE
          min_pmix: {<fn>}
          range_shape1: {0.05, 20}
          range_shape2: {0.05, 20}
        hc grid:
          proportion: 0.05
          ci: FALSE, TRUE
          nboot: 100, 1000
          est_method: multi
          ci_method: weighted_samples
          parametric: TRUE

