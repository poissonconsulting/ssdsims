# design-cost: design analysis and comparison print is informative

    Code
      print(analysis)
    Output
      <ssdsims_cost_analysis>  (design observed, serial-equivalent; 2 members)
        total compute:  2.0 hours  (per-member accounting; shared cells counted per member)
        longest task:   2.0 min
        breakdown (scenario x ci_method x nboot, by total cost):
          coarse       weighted_samples       nboot     50     2 tasks  40.0 min
          coarse       weighted_samples       nboot     10     2 tasks  21.0 min
          dense        weighted_samples       nboot     50     2 tasks  1.0 hours
        host(s):        Test CPU @ 1.0GHz
      

---

    Code
      print(comparison)
    Output
      <ssdsims_cost_comparison>  (predicted vs observed)
        total compute:  predicted 1.9 hours | observed 2.0 hours | obs/pred 1.04x
        longest task:   predicted 2.2 min | observed 2.0 min | obs/pred 0.92x
      

