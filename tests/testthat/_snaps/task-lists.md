# task-lists: printing a task table is informative

    Code
      ssd_scenario_sample_tasks(ssd_define_scenario(ssddata::ccme_boron, nsim = 2L,
      seed = 42L))
    Output
      <ssdsims_tasks: sample>
        axes:  dataset, sim, replace
        tasks: 2
      # A tibble: 2 x 5
        sample_id                              dataset      sim replace n_max
      * <chr>                                  <chr>      <int> <lgl>   <int>
      1 dataset=ccme_boron/sim=1/replace=FALSE ccme_boron     1 FALSE       6
      2 dataset=ccme_boron/sim=2/replace=FALSE ccme_boron     2 FALSE       6

---

    Code
      ssd_scenario_data_tasks(ssd_define_scenario(ssddata::ccme_boron, nsim = 1L,
      nrow = c(5L, 10L), seed = 42L))
    Output
      <ssdsims_tasks: data>
        axes:  dataset, sim, replace, nrow
        tasks: 2
      # A tibble: 2 x 6
        data_id                                  sample_id dataset   sim replace  nrow
      * <chr>                                    <chr>     <chr>   <int> <lgl>   <int>
      1 dataset=ccme_boron/sim=1/replace=FALSE/~ dataset=~ ccme_b~     1 FALSE       5
      2 dataset=ccme_boron/sim=1/replace=FALSE/~ dataset=~ ccme_b~     1 FALSE      10

---

    Code
      ssd_scenario_fit_tasks(ssd_define_scenario(ssddata::ccme_boron, nsim = 1L,
      seed = 42L, rescale = c(FALSE, TRUE)))
    Output
      <ssdsims_tasks: fit>
        axes:  dataset, sim, replace, nrow, rescale, computable, at_boundary_ok, min_pmix, range_shape1, range_shape2
        tasks: 2
      # A tibble: 2 x 12
        fit_id   data_id dataset   sim replace  nrow rescale computable at_boundary_ok
      * <chr>    <chr>   <chr>   <int> <lgl>   <int> <lgl>   <lgl>      <lgl>         
      1 dataset~ datase~ ccme_b~     1 FALSE       6 FALSE   FALSE      TRUE          
      2 dataset~ datase~ ccme_b~     1 FALSE       6 TRUE    FALSE      TRUE          
      # i 3 more variables: min_pmix <chr>, range_shape1 <list>, range_shape2 <list>

---

    Code
      ssd_scenario_hc_tasks(ssd_define_scenario(ssddata::ccme_boron, nsim = 1L, seed = 42L,
      ci = c(FALSE, TRUE), nboot = c(10L, 100L)))
    Output
      <ssdsims_tasks: hc>
        axes:  dataset, sim, replace, nrow, rescale, computable, at_boundary_ok, min_pmix, range_shape1, range_shape2, ci, nboot, est_method, ci_method, parametric
        tasks: 3
      # A tibble: 3 x 17
        hc_id     fit_id dataset   sim replace  nrow rescale computable at_boundary_ok
      * <chr>     <chr>  <chr>   <int> <lgl>   <int> <lgl>   <lgl>      <lgl>         
      1 dataset=~ datas~ ccme_b~     1 FALSE       6 FALSE   FALSE      TRUE          
      2 dataset=~ datas~ ccme_b~     1 FALSE       6 FALSE   FALSE      TRUE          
      3 dataset=~ datas~ ccme_b~     1 FALSE       6 FALSE   FALSE      TRUE          
      # i 8 more variables: min_pmix <chr>, range_shape1 <list>, range_shape2 <list>,
      #   ci <lgl>, nboot <int>, est_method <chr>, ci_method <chr>, parametric <lgl>

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
      [1] "sample_id" "dataset"   "sim"       "replace"   "n_max"    
    Code
      names(ssd_scenario_data_tasks(scenario))
    Output
      [1] "data_id"   "sample_id" "dataset"   "sim"       "replace"   "nrow"     
    Code
      names(ssd_scenario_fit_tasks(scenario))
    Output
       [1] "fit_id"         "data_id"        "dataset"        "sim"           
       [5] "replace"        "nrow"           "rescale"        "computable"    
       [9] "at_boundary_ok" "min_pmix"       "range_shape1"   "range_shape2"  
    Code
      names(ssd_scenario_hc_tasks(scenario))
    Output
       [1] "hc_id"          "fit_id"         "dataset"        "sim"           
       [5] "replace"        "nrow"           "rescale"        "computable"    
       [9] "at_boundary_ok" "min_pmix"       "range_shape1"   "range_shape2"  
      [13] "ci"             "nboot"          "est_method"     "ci_method"     
      [17] "parametric"    

