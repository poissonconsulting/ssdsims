# cost-analysis: analysis and comparison print is informative

    Code
      print(analysis)
    Output
      <ssdsims_cost_analysis>  (observed, serial-equivalent; measured)
        total compute:  1.0 hours
        longest task:   2.0 min
        breakdown (ci_method x nboot, by total cost):
          weighted_samples       nboot     50     2 tasks  40.0 min
          weighted_samples       nboot     10     2 tasks  21.0 min
        host(s):        Test CPU @ 1.0GHz
      

---

    Code
      print(comparison)
    Output
      <ssdsims_cost_comparison>  (predicted vs observed)
        total compute:  predicted 1.3 s | observed 1.0 hours | obs/pred 1.05x
        longest task:   predicted 0.4 s | observed 2.0 min | obs/pred 0.91x
      

