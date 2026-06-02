# task-lists: printing a task table is informative

    Code
      ssd_scenario_data_tasks(ssd_define_scenario(ssddata::ccme_boron, nsim = 2L,
      seed = 42L))
    Output
      <ssdsims_tasks: data>
        axes:  dataset, sim, replace
        tasks: 2
      # A tibble: 2 x 4
        dataset      sim replace nrow     
      * <chr>      <int> <lgl>   <list>   
      1 ccme_boron     1 FALSE   <int [1]>
      2 ccme_boron     2 FALSE   <int [1]>

---

    Code
      ssd_scenario_fit_tasks(ssd_define_scenario(ssddata::ccme_boron, nsim = 1L,
      seed = 42L, rescale = c(FALSE, TRUE)))
    Output
      <ssdsims_tasks: fit>
        axes:  dataset, sim, replace, rescale, computable, at_boundary_ok, min_pmix, range_shape1, range_shape2
        tasks: 2
      # A tibble: 2 x 10
        dataset      sim replace nrow      rescale computable at_boundary_ok min_pmix 
      * <chr>      <int> <lgl>   <list>    <lgl>   <lgl>      <lgl>          <chr>    
      1 ccme_boron     1 FALSE   <int [1]> FALSE   FALSE      TRUE           ssd_min_~
      2 ccme_boron     1 FALSE   <int [1]> TRUE    FALSE      TRUE           ssd_min_~
      # i 2 more variables: range_shape1 <list>, range_shape2 <list>

---

    Code
      ssd_scenario_hc_tasks(ssd_define_scenario(ssddata::ccme_boron, nsim = 1L, seed = 42L,
      ci = c(FALSE, TRUE), nboot = c(10L, 100L)))
    Output
      <ssdsims_tasks: hc>
        axes:  dataset, sim, replace, rescale, computable, at_boundary_ok, min_pmix, range_shape1, range_shape2, ci, nboot, est_method, ci_method, parametric
        tasks: 3
      # A tibble: 3 x 15
        dataset      sim replace nrow      rescale computable at_boundary_ok min_pmix 
      * <chr>      <int> <lgl>   <list>    <lgl>   <lgl>      <lgl>          <chr>    
      1 ccme_boron     1 FALSE   <int [1]> FALSE   FALSE      TRUE           ssd_min_~
      2 ccme_boron     1 FALSE   <int [1]> FALSE   FALSE      TRUE           ssd_min_~
      3 ccme_boron     1 FALSE   <int [1]> FALSE   FALSE      TRUE           ssd_min_~
      # i 7 more variables: range_shape1 <list>, range_shape2 <list>, ci <lgl>,
      #   nboot <int>, est_method <chr>, ci_method <chr>, parametric <lgl>

# task-lists: runner errors on missing dataset

    Code
      ssd_run_scenario_baseline(scenario, ssd_data(boron = ssddata::ccme_boron))
    Condition
      Error in `ssd_run_scenario_baseline()`:
      ! `data` is missing dataset: 'cadmium'.

# task-lists: task-table column contracts are pinned

    Code
      names(ssd_scenario_data_tasks(scenario))
    Output
      [1] "dataset" "sim"     "replace" "nrow"   
    Code
      names(ssd_scenario_fit_tasks(scenario))
    Output
       [1] "dataset"        "sim"            "replace"        "nrow"          
       [5] "rescale"        "computable"     "at_boundary_ok" "min_pmix"      
       [9] "range_shape1"   "range_shape2"  
    Code
      names(ssd_scenario_hc_tasks(scenario))
    Output
       [1] "dataset"        "sim"            "replace"        "nrow"          
       [5] "rescale"        "computable"     "at_boundary_ok" "min_pmix"      
       [9] "range_shape1"   "range_shape2"   "ci"             "nboot"         
      [13] "est_method"     "ci_method"      "parametric"    

