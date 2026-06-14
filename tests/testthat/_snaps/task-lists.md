# task-lists: printing a task table is informative

    Code
      ssd_scenario_sample_tasks(ssd_define_scenario(ssd_scenario_data(ssddata::ccme_boron),
      nsim = 2L, seed = 42L))
    Output
      <ssdsims_tasks: sample>
        axes:  dataset, sim, replace
        tasks: 2
      # A tibble: 2 x 4
        dataset      sim replace sample_id                            
        <chr>      <int> <lgl>   <chr>                                
      1 ccme_boron     1 TRUE    dataset=ccme_boron/sim=1/replace=TRUE
      2 ccme_boron     2 TRUE    dataset=ccme_boron/sim=2/replace=TRUE

---

    Code
      ssd_scenario_fit_tasks(ssd_define_scenario(ssd_scenario_data(ssddata::ccme_boron),
      nsim = 1L, seed = 42L, nrow = c(5L, 10L), rescale = c(FALSE, TRUE)))
    Output
      <ssdsims_tasks: fit>
        axes:  dataset, sim, replace, nrow, rescale, computable, at_boundary_ok, min_pmix, range_shape1, range_shape2
        tasks: 4
      # A tibble: 4 x 12
        dataset      sim replace  nrow rescale computable at_boundary_ok min_pmix    
        <chr>      <int> <lgl>   <int> <lgl>   <lgl>      <lgl>          <chr>       
      1 ccme_boron     1 TRUE        5 FALSE   FALSE      TRUE           ssd_min_pmix
      2 ccme_boron     1 TRUE        5 TRUE    FALSE      TRUE           ssd_min_pmix
      3 ccme_boron     1 TRUE       10 FALSE   FALSE      TRUE           ssd_min_pmix
      4 ccme_boron     1 TRUE       10 TRUE    FALSE      TRUE           ssd_min_pmix
      # i 4 more variables: range_shape1 <list>, range_shape2 <list>, fit_id <chr>,
      #   sample_id <chr>

---

    Code
      ssd_scenario_hc_tasks(ssd_define_scenario(ssd_scenario_data(ssddata::ccme_boron),
      nsim = 1L, seed = 42L, ci = TRUE, nboot = c(10L, 100L)))
    Output
      <ssdsims_tasks: hc>
        axes:  dataset, sim, replace, nrow, rescale, computable, at_boundary_ok, min_pmix, range_shape1, range_shape2, nboot, ci_method, parametric, distset
        tasks: 2
      # A tibble: 2 x 16
        dataset      sim replace  nrow rescale computable at_boundary_ok min_pmix    
        <chr>      <int> <lgl>   <int> <lgl>   <lgl>      <lgl>          <chr>       
      1 ccme_boron     1 TRUE        6 FALSE   FALSE      TRUE           ssd_min_pmix
      2 ccme_boron     1 TRUE        6 FALSE   FALSE      TRUE           ssd_min_pmix
      # i 8 more variables: range_shape1 <list>, range_shape2 <list>, nboot <int>,
      #   ci_method <chr>, parametric <lgl>, distset <chr>, hc_id <chr>, fit_id <chr>

# task-lists: printing a task set reports per-step counts

    Code
      ssd_scenario_tasks(ssd_define_scenario(ssd_scenario_data(ssddata::ccme_boron),
      nsim = 2L, seed = 42L, nrow = c(5L, 10L), rescale = c(FALSE, TRUE), ci = TRUE,
      nboot = c(10L, 100L)))
    Output
      <ssdsims_task_set>
        sample tasks: 2
        fit    tasks: 8
        hc     tasks: 16

# task-lists: task-table column contracts are pinned

    Code
      names(ssd_scenario_sample_tasks(scenario))
    Output
      [1] "dataset"   "sim"       "replace"   "sample_id"
    Code
      names(ssd_scenario_fit_tasks(scenario))
    Output
       [1] "dataset"        "sim"            "replace"        "nrow"          
       [5] "rescale"        "computable"     "at_boundary_ok" "min_pmix"      
       [9] "range_shape1"   "range_shape2"   "fit_id"         "sample_id"     
    Code
      names(ssd_scenario_hc_tasks(scenario))
    Output
       [1] "dataset"        "sim"            "replace"        "nrow"          
       [5] "rescale"        "computable"     "at_boundary_ok" "min_pmix"      
       [9] "range_shape1"   "range_shape2"   "nboot"          "ci_method"     
      [13] "parametric"     "distset"        "hc_id"          "fit_id"        

