# scenario-accessors: scenario_dataset errors on an unknown name

    Code
      scenario_dataset(s, "nope")
    Condition
      Error:
      ! Unknown dataset "nope"; scenario datasets are 'ccme_boron'.

# scenario-accessors: scenario_min_pmix errors on an unknown name

    Code
      scenario_min_pmix(s, "nope")
    Condition
      Error:
      ! Unknown `min_pmix` name "nope"; scenario `min_pmix` names are 'ssd_min_pmix'.

