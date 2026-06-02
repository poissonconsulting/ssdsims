# task-lists: printing a task table is informative

    Code
      ssd_scenario_sample_tasks(ssd_define_scenario(ssddata::ccme_boron, nsim = 2L,
      seed = 42L))
    Output
      <ssdsims_tasks: sample>
        axes:  dataset, sim, replace
        tasks: 2
      # A tibble: 2 x 5
        dataset      sim replace n_max sample_id                             
        <chr>      <int> <lgl>   <int> <chr>                                 
      1 ccme_boron     1 FALSE       6 dataset=ccme_boron/sim=1/replace=FALSE
      2 ccme_boron     2 FALSE       6 dataset=ccme_boron/sim=2/replace=FALSE

---

    Code
      ssd_scenario_data_tasks(ssd_define_scenario(ssddata::ccme_boron, nsim = 1L,
      nrow = c(5L, 10L), seed = 42L))
    Output
      <ssdsims_tasks: data>
        axes:  dataset, sim, replace, nrow
        tasks: 2
      # A tibble: 2 x 6
        dataset      sim replace  nrow data_id                               sample_id
        <chr>      <int> <lgl>   <int> <chr>                                 <chr>    
      1 ccme_boron     1 FALSE       5 dataset=ccme_boron/sim=1/replace=FAL~ dataset=~
      2 ccme_boron     1 FALSE      10 dataset=ccme_boron/sim=1/replace=FAL~ dataset=~

---

    Code
      ssd_scenario_fit_tasks(ssd_define_scenario(ssddata::ccme_boron, nsim = 1L,
      seed = 42L, rescale = c(FALSE, TRUE)))
    Output
      <ssdsims_tasks: fit>
        axes:  dataset, sim, replace, nrow, rescale, computable, at_boundary_ok, min_pmix, range_shape1, range_shape2
        tasks: 2
      # A tibble: 2 x 12
        dataset      sim replace  nrow rescale computable at_boundary_ok min_pmix    
        <chr>      <int> <lgl>   <int> <lgl>   <lgl>      <lgl>          <chr>       
      1 ccme_boron     1 FALSE       6 FALSE   FALSE      TRUE           ssd_min_pmix
      2 ccme_boron     1 FALSE       6 TRUE    FALSE      TRUE           ssd_min_pmix
      # i 4 more variables: range_shape1 <list>, range_shape2 <list>, fit_id <chr>,
      #   data_id <chr>

---

    Code
      ssd_scenario_hc_tasks(ssd_define_scenario(ssddata::ccme_boron, nsim = 1L, seed = 42L,
      ci = c(FALSE, TRUE), nboot = c(10L, 100L)))
    Output
      <ssdsims_tasks: hc>
        axes:  dataset, sim, replace, nrow, rescale, computable, at_boundary_ok, min_pmix, range_shape1, range_shape2, ci, nboot, est_method, ci_method, parametric
        tasks: 3
      # A tibble: 3 x 17
        dataset      sim replace  nrow rescale computable at_boundary_ok min_pmix    
        <chr>      <int> <lgl>   <int> <lgl>   <lgl>      <lgl>          <chr>       
      1 ccme_boron     1 FALSE       6 FALSE   FALSE      TRUE           ssd_min_pmix
      2 ccme_boron     1 FALSE       6 FALSE   FALSE      TRUE           ssd_min_pmix
      3 ccme_boron     1 FALSE       6 FALSE   FALSE      TRUE           ssd_min_pmix
      # i 9 more variables: range_shape1 <list>, range_shape2 <list>, ci <lgl>,
      #   nboot <int>, est_method <chr>, ci_method <chr>, parametric <lgl>,
      #   hc_id <chr>, fit_id <chr>

# task-lists: printing a task set reports per-step counts

    Code
      ssd_scenario_tasks(ssd_define_scenario(ssddata::ccme_boron, nsim = 2L, nrow = c(
        5L, 10L), seed = 42L, rescale = c(FALSE, TRUE), ci = c(FALSE, TRUE), nboot = c(
        10L, 100L)))
    Output
      <ssdsims_task_set>
        sample tasks: 2
        data   tasks: 4
        fit    tasks: 8
        hc     tasks: 24

# task-lists: runner errors on missing dataset

    Code
      ssd_run_scenario_baseline(scenario, ssd_data(boron = ssddata::ccme_boron))
    Condition
      Error in `ssd_run_scenario_baseline()`:
      ! `data` is missing dataset: 'cadmium'.

# task-lists: task-table column contracts are pinned

    Code
      names(ssd_scenario_sample_tasks(scenario))
    Output
      [1] "dataset"   "sim"       "replace"   "n_max"     "sample_id"
    Code
      names(ssd_scenario_data_tasks(scenario))
    Output
      [1] "dataset"   "sim"       "replace"   "nrow"      "data_id"   "sample_id"
    Code
      names(ssd_scenario_fit_tasks(scenario))
    Output
       [1] "dataset"        "sim"            "replace"        "nrow"          
       [5] "rescale"        "computable"     "at_boundary_ok" "min_pmix"      
       [9] "range_shape1"   "range_shape2"   "fit_id"         "data_id"       
    Code
      names(ssd_scenario_hc_tasks(scenario))
    Output
       [1] "dataset"        "sim"            "replace"        "nrow"          
       [5] "rescale"        "computable"     "at_boundary_ok" "min_pmix"      
       [9] "range_shape1"   "range_shape2"   "ci"             "nboot"         
      [13] "est_method"     "ci_method"      "parametric"     "hc_id"         
      [17] "fit_id"        

