# scenario-definition: ssd_scenario_data requires a Conc column

    Code
      ssd_scenario_data(d = data.frame(x = 1:5))
    Condition
      Error in `ssd_scenario_data()`:
      ! Dataset `d` must have a column named `Conc`.

# scenario-definition: ssd_scenario_data rejects a non-numeric Conc column

    Code
      ssd_scenario_data(d = data.frame(Conc = c("a", "b")))
    Condition
      Error in `ssd_scenario_data()`:
      ! Dataset `d` must have a numeric `Conc` column.

# scenario-definition: ssd_scenario_data needs a derivable or explicit name

    Code
      ssd_scenario_data(data.frame(Conc = 1:5))
    Condition
      Error in `ssd_scenario_data()`:
      ! Unable to derive a name for dataset 1; supply a name (e.g. `ssd_scenario_data(boron = ...)`).

# scenario-definition: ssd_scenario_data rejects duplicate names

    Code
      ssd_scenario_data(x = ssddata::ccme_boron, x = ssddata::ccme_cadmium)
    Condition
      Error in `ssd_scenario_data()`:
      ! Dataset names must be unique.

# scenario-definition: a duplicate name across mixed inputs aborts

    Code
      ssd_scenario_data(synth = ssddata::ccme_boron, ssd_gen(synth = ssdtools::ssd_rlnorm,
      .n = 30, .seed = 1L))
    Condition
      Error in `ssd_scenario_data()`:
      ! Dataset names must be unique.

# scenario-definition: a named ssd_gen() argument is rejected

    Code
      ssd_scenario_data(g = ssd_gen(synth = ssdtools::ssd_rlnorm, .n = 30, .seed = 1L))
    Condition
      Error in `ssd_scenario_data()`:
      ! An `ssd_gen()` collection must be supplied unnamed; its members carry their own names.

