# print works for data.frame source

    Code
      print(scenario)
    Output
      <ssd_scenario>
      Source:    data.frame [28 x 5]
      Jobs:      4
      Simulation:
        nsim:      2
        start_sim: 1
        stream:    1
        seed:      NULL
      Fit:
        dists: [gamma, lgumbel, llogis, lnorm, lnorm_lnorm, weibull]
        rescale: FALSE
        computable: FALSE
        at_boundary_ok: TRUE
        min_pmix: [<function>]
        range_shape1: [[0.05, 20]]
        range_shape2: [[0.05, 20]]
      HC:
        proportion: 0.05
        ci: FALSE
        nboot: 1000
        est_method: multi
        ci_method: weighted_samples
        parametric: TRUE

# print works for function source

    Code
      print(scenario)
    Output
      <ssd_scenario>
      Source:    function
      Jobs:      2
      Simulation:
        nsim:      2
        start_sim: 1
        stream:    1
        seed:      NULL
      Fit:
        dists: [gamma, lgumbel, llogis, lnorm, lnorm_lnorm, weibull]
        rescale: FALSE
        computable: FALSE
        at_boundary_ok: TRUE
        min_pmix: [<function>]
        range_shape1: [[0.05, 20]]
        range_shape2: [[0.05, 20]]
      HC:
        proportion: 0.05
        ci: FALSE
        nboot: 1000
        est_method: multi
        ci_method: weighted_samples
        parametric: TRUE

# print works for character source

    Code
      print(scenario)
    Output
      <ssd_scenario>
      Source:    function
      Jobs:      2
      Simulation:
        nsim:      2
        start_sim: 1
        stream:    1
        seed:      NULL
      Fit:
        dists: [gamma, lgumbel, llogis, lnorm, lnorm_lnorm, weibull]
        rescale: FALSE
        computable: FALSE
        at_boundary_ok: TRUE
        min_pmix: [<function>]
        range_shape1: [[0.05, 20]]
        range_shape2: [[0.05, 20]]
      HC:
        proportion: 0.05
        ci: FALSE
        nboot: 1000
        est_method: multi
        ci_method: weighted_samples
        parametric: TRUE

# print works for tmbfit source

    Code
      print(scenario)
    Output
      <ssd_scenario>
      Source:    tmbfit (gamma)
      Jobs:      2
      Simulation:
        nsim:      2
        start_sim: 1
        stream:    1
        seed:      NULL
      Fit:
        dists: [gamma, lgumbel, llogis, lnorm, lnorm_lnorm, weibull]
        rescale: FALSE
        computable: FALSE
        at_boundary_ok: TRUE
        min_pmix: [<function>]
        range_shape1: [[0.05, 20]]
        range_shape2: [[0.05, 20]]
      HC:
        proportion: 0.05
        ci: FALSE
        nboot: 1000
        est_method: multi
        ci_method: weighted_samples
        parametric: TRUE

# print works for fitdists source

    Code
      print(scenario)
    Output
      <ssd_scenario>
      Source:    fitdists: gamma, lgumbel, llogis, lnorm, lnorm_lnorm, weibull
      Jobs:      4
      Simulation:
        nsim:      2
        start_sim: 1
        stream:    1
        seed:      NULL
      Fit:
        dists: [gamma, lgumbel, llogis, lnorm, lnorm_lnorm, weibull]
        rescale: FALSE
        computable: FALSE
        at_boundary_ok: TRUE
        min_pmix: [<function>]
        range_shape1: [[0.05, 20]]
        range_shape2: [[0.05, 20]]
      HC:
        proportion: 0.05
        ci: FALSE
        nboot: 1000
        est_method: multi
        ci_method: weighted_samples
        parametric: TRUE

# print reflects custom fit and hc params

    Code
      print(scenario)
    Output
      <ssd_scenario>
      Source:    function
      Jobs:      6
      Simulation:
        nsim:      3
        start_sim: 7
        stream:    2
        seed:      42
      Fit:
        dists: [gamma, lgumbel, llogis, lnorm, lnorm_lnorm, weibull]
        rescale: FALSE
        computable: FALSE
        at_boundary_ok: TRUE
        min_pmix: [<function>]
        range_shape1: [[0.05, 20]]
        range_shape2: [[0.05, 20]]
      HC:
        proportion: [0.05, 0.1]
        ci: TRUE
        nboot: 500
        est_method: multi
        ci_method: weighted_samples
        parametric: TRUE

