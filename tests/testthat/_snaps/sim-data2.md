# print.ssdsims_scenario data.frame

    Code
      scenario
    Output
      <ssdsims_scenario>
      * Generator: data.frame
          source:  data.frame [28 x 5]
          nrow:    6
          replace: FALSE
      * Sim: nsim=2, stream=1, start_sim=1, seed=1
      * Fit:
          dists:          gamma, lgumbel, llogis, lnorm, lnorm_lnorm, weibull
          rescale:        FALSE
          computable:     FALSE
          at_boundary_ok: TRUE
          min_pmix:       <list of 1>
          range_shape1:   <list of 1>
          range_shape2:   <list of 1>
      * HC:
          proportion: 0.05
          ci:         FALSE
          nboot:      1000
          est_method: multi
          ci_method:  weighted_samples
          parametric: TRUE

# print.ssdsims_scenario data.frame vectorized

    Code
      scenario
    Output
      <ssdsims_scenario>
      * Generator: data.frame
          source:  data.frame [28 x 5]
          nrow:     5, 10
          replace:  TRUE, FALSE
      * Sim: nsim=5, stream=1, start_sim=1, seed=42
      * Fit:
          dists:          gamma, lgumbel, llogis, lnorm, lnorm_lnorm, weibull
          rescale:        FALSE
          computable:     FALSE
          at_boundary_ok: TRUE
          min_pmix:       <list of 1>
          range_shape1:   <list of 1>
          range_shape2:   <list of 1>
      * HC:
          proportion: 0.05
          ci:         FALSE
          nboot:      1000
          est_method: multi
          ci_method:  weighted_samples
          parametric: TRUE

# print.ssdsims_scenario function

    Code
      scenario
    Output
      <ssdsims_scenario>
      * Generator: function
          nrow: 6
          args: <empty>
      * Sim: nsim=2, stream=1, start_sim=1, seed=1
      * Fit:
          dists:          gamma, lgumbel, llogis, lnorm, lnorm_lnorm, weibull
          rescale:        FALSE
          computable:     FALSE
          at_boundary_ok: TRUE
          min_pmix:       <list of 1>
          range_shape1:   <list of 1>
          range_shape2:   <list of 1>
      * HC:
          proportion: 0.05
          ci:         FALSE
          nboot:      1000
          est_method: multi
          ci_method:  weighted_samples
          parametric: TRUE

# print.ssdsims_scenario character

    Code
      scenario
    Output
      <ssdsims_scenario>
      * Generator: function
          nrow: 6
          args: <empty>
      * Sim: nsim=2, stream=1, start_sim=1, seed=1
      * Fit:
          dists:          gamma, lgumbel, llogis, lnorm, lnorm_lnorm, weibull
          rescale:        FALSE
          computable:     FALSE
          at_boundary_ok: TRUE
          min_pmix:       <list of 1>
          range_shape1:   <list of 1>
          range_shape2:   <list of 1>
      * HC:
          proportion: 0.05
          ci:         FALSE
          nboot:      1000
          est_method: multi
          ci_method:  weighted_samples
          parametric: TRUE

# print.ssdsims_scenario tmbfit

    Code
      scenario
    Output
      <ssdsims_scenario>
      * Generator: function
          nrow: 6
          args: meanlog=2.561646, sdlog=1.241541
      * Sim: nsim=2, stream=1, start_sim=1, seed=1
      * Fit:
          dists:          gamma, lgumbel, llogis, lnorm, lnorm_lnorm, weibull
          rescale:        FALSE
          computable:     FALSE
          at_boundary_ok: TRUE
          min_pmix:       <list of 1>
          range_shape1:   <list of 1>
          range_shape2:   <list of 1>
      * HC:
          proportion: 0.05
          ci:         FALSE
          nboot:      1000
          est_method: multi
          ci_method:  weighted_samples
          parametric: TRUE

# print.ssdsims_scenario fitdists

    Code
      scenario
    Output
      <ssdsims_scenario>
      * Generator: fitdists
          source:   fitdists [lnorm, gamma]
          nrow:     6
          dist_sim: lnorm, top
      * Sim: nsim=2, stream=1, start_sim=1, seed=1
      * Fit:
          dists:          gamma, lgumbel, llogis, lnorm, lnorm_lnorm, weibull
          rescale:        FALSE
          computable:     FALSE
          at_boundary_ok: TRUE
          min_pmix:       <list of 1>
          range_shape1:   <list of 1>
          range_shape2:   <list of 1>
      * HC:
          proportion: 0.05
          ci:         FALSE
          nboot:      1000
          est_method: multi
          ci_method:  weighted_samples
          parametric: TRUE

# print.ssdsims_scenario with custom fit and hc args

    Code
      scenario
    Output
      <ssdsims_scenario>
      * Generator: data.frame
          source:  data.frame [28 x 5]
          nrow:    6
          replace: FALSE
      * Sim: nsim=2, stream=1, start_sim=1, seed=1
      * Fit:
          dists:          gamma, lgumbel, llogis, lnorm, lnorm_lnorm, weibull
          rescale:         TRUE, FALSE
          computable:     FALSE
          at_boundary_ok: TRUE
          min_pmix:       <list of 1>
          range_shape1:   <list of 1>
          range_shape2:   <list of 1>
      * HC:
          proportion: 0.05, 0.10
          ci:         TRUE
          nboot:      100, 200
          est_method: multi
          ci_method:  weighted_samples
          parametric: TRUE

# print.ssdsims_scenario with extras

    Code
      scenario
    Output
      <ssdsims_scenario>
      * Generator: data.frame
          source:  data.frame [28 x 5]
          nrow:    6
          replace: FALSE
      * Sim: nsim=2, stream=1, start_sim=1, seed=1
      * Fit:
          dists:          gamma, lgumbel, llogis, lnorm, lnorm_lnorm, weibull
          rescale:        FALSE
          computable:     FALSE
          at_boundary_ok: TRUE
          min_pmix:       <list of 1>
          range_shape1:   <list of 1>
          range_shape2:   <list of 1>
      * HC:
          proportion: 0.05
          ci:         FALSE
          nboot:      1000
          est_method: multi
          ci_method:  weighted_samples
          parametric: TRUE
      * Extras:
          fit: left
          hc:  samples

