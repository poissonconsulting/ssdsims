# cost-analysis-design: design breakdown renders per-scenario and totals

    Code
      cat(format_design_breakdown(bd, total = as.difftime(3660, units = "secs"),
      longest = as.difftime(3600, units = "secs")), sep = "\n")
    Output
        design observed (serial-equivalent):
        total compute:  1.0 hours
        longest task:   1.0 hours
        breakdown (scenario x ci_method x nboot, by total cost):
          a            weighted_samples   nboot    100     1 tasks  1.0 min
          b            weighted_arithmetic_samples nboot    200     2 tasks  1.0 hours

