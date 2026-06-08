# cost-estimation: calibration print renders coefficients and caveat

    Code
      test_cost_calibration()
    Output
      <ssdsims_cost_calibration>
        per-ci_method cost model  time = (base + slope * max(nboot, n0)) * nrow_factor
          weighted_samples   base   0.60s     5.70 ms/boot  n0   0
          GMACL              base   1.00s    18.90 ms/boot  n0  25
          MACL               base   1.00s    19.60 ms/boot  n0  25
          arithmetic_samples base   1.00s    21.00 ms/boot  n0  30
          geometric_samples  base   1.00s    21.60 ms/boot  n0  25
          multi_fixed        base   0.10s    52.60 ms/boot  n0   0
          multi_free         base   1.00s    55.00 ms/boot  n0  25
        nrow_factor:   5:0.30  10:1.00  20:1.00  50:0.80
        fixed_addend:  0.05s (sample + fit per task)
        provenance:
          cpu:       Test CPU @ 1.00GHz
          R:         R version 4.5.3 (2026-03-11)
          ssdtools:  2.6.0.9002
          date:      2026-06-07
          sweep:     nboot {20, 50, 100, 200} x nrow {5, 10, 20, 50} x 7 ci_methods
        Ballpark only: coefficients are architecture-specific - recalibrate with
        ssd_calibrate_cost() on the target machine for a trustworthy estimate.
      

# cost-estimation: estimate print is stable

    Code
      ssd_estimate_cost(scenario, test_cost_calibration())
    Output
      <ssdsims_cost_estimate>  (ballpark, serial)
        total compute:  44.7 min
        longest task:   9.2 min
        breakdown (ci_method x nboot, by total cost):
          multi_free         nboot  10000     4 tasks  36.7 min
          weighted_samples   nboot  10000     4 tasks  3.8 min
          multi_free         nboot   1000     4 tasks  3.7 min
          weighted_samples   nboot   1000     4 tasks  25.4 s
        calibration:    Test CPU @ 1.00GHz | R 4.5.3 | ssdtools 2.6.0.9002 | 2026-06-07
        Ballpark only - recalibrate with ssd_calibrate_cost() on the target machine.
      

# cost-estimation: input validation errors

    Code
      ssd_calibrate_cost(nrow = 10L)
    Condition
      Error in `ssd_calibrate_cost()`:
      ! `nrow` must have a length between 2 and Inf not 1.

---

    Code
      ssd_calibrate_cost(nboot = -1L)
    Condition
      Error in `ssd_calibrate_cost()`:
      ! `nboot` must be greater than 0, not -1.

