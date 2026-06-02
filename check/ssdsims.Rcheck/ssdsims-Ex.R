pkgname <- "ssdsims"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
base::assign(".ExTimings", "ssdsims-Ex.timings", pos = 'CheckExEnv')
base::cat(
  "name\tuser\tsystem\telapsed\n",
  file = base::get(".ExTimings", pos = 'CheckExEnv')
)
base::assign(
  ".format_ptime",
  function(x) {
    if (!is.na(x[4L])) {
      x[1L] <- x[1L] + x[4L]
    }
    if (!is.na(x[5L])) {
      x[2L] <- x[2L] + x[5L]
    }
    options(OutDec = '.')
    format(x[1L:3L], digits = 7L)
  },
  pos = 'CheckExEnv'
)

### * </HEADER>
library('ssdsims')

base::assign(".oldSearch", base::search(), pos = 'CheckExEnv')
base::assign(".old_wd", base::getwd(), pos = 'CheckExEnv')
cleanEx()
nameEx("local_dqrng_backend")
### * local_dqrng_backend

flush(stderr())
flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: local_dqrng_backend
### Title: Local dqrng pcg64 Backend
### Aliases: local_dqrng_backend

### ** Examples

local_dqrng_backend()
dqrng::dqset.seed(42, stream = c(1L, 2L))
runif(3)


base::assign(
  ".dptime",
  (proc.time() - get(".ptime", pos = "CheckExEnv")),
  pos = "CheckExEnv"
)
base::cat(
  "local_dqrng_backend",
  base::get(".format_ptime", pos = 'CheckExEnv')(get(
    ".dptime",
    pos = "CheckExEnv"
  )),
  "\n",
  file = base::get(".ExTimings", pos = 'CheckExEnv'),
  append = TRUE,
  sep = "\t"
)
cleanEx()
nameEx("local_lecuyer_cmrg_seed")
### * local_lecuyer_cmrg_seed

flush(stderr())
flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: local_lecuyer_cmrg_seed
### Title: Local L'Ecuyer-CMRG Seed
### Aliases: local_lecuyer_cmrg_seed

### ** Examples

local_lecuyer_cmrg_seed(42)
runif(3)


base::assign(
  ".dptime",
  (proc.time() - get(".ptime", pos = "CheckExEnv")),
  pos = "CheckExEnv"
)
base::cat(
  "local_lecuyer_cmrg_seed",
  base::get(".format_ptime", pos = 'CheckExEnv')(get(
    ".dptime",
    pos = "CheckExEnv"
  )),
  "\n",
  file = base::get(".ExTimings", pos = 'CheckExEnv'),
  append = TRUE,
  sep = "\t"
)
cleanEx()
nameEx("local_lecuyer_cmrg_state")
### * local_lecuyer_cmrg_state

flush(stderr())
flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: local_lecuyer_cmrg_state
### Title: Local L'Ecuyer-CMRG State
### Aliases: local_lecuyer_cmrg_state

### ** Examples

state <- with_lecuyer_cmrg_seed(42, parallel::nextRNGStream(.Random.seed))
local_lecuyer_cmrg_state(state)
runif(3)


base::assign(
  ".dptime",
  (proc.time() - get(".ptime", pos = "CheckExEnv")),
  pos = "CheckExEnv"
)
base::cat(
  "local_lecuyer_cmrg_state",
  base::get(".format_ptime", pos = 'CheckExEnv')(get(
    ".dptime",
    pos = "CheckExEnv"
  )),
  "\n",
  file = base::get(".ExTimings", pos = 'CheckExEnv'),
  append = TRUE,
  sep = "\t"
)
cleanEx()
nameEx("ssd_data")
### * ssd_data

flush(stderr())
flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: ssd_data
### Title: Assemble and Validate Datasets for a Simulation Scenario
### Aliases: ssd_data

### ** Examples

ssd_data(ssddata::ccme_boron)
ssd_data(boron = ssddata::ccme_boron, cadmium = ssddata::ccme_cadmium)


base::assign(
  ".dptime",
  (proc.time() - get(".ptime", pos = "CheckExEnv")),
  pos = "CheckExEnv"
)
base::cat(
  "ssd_data",
  base::get(".format_ptime", pos = 'CheckExEnv')(get(
    ".dptime",
    pos = "CheckExEnv"
  )),
  "\n",
  file = base::get(".ExTimings", pos = 'CheckExEnv'),
  append = TRUE,
  sep = "\t"
)
cleanEx()
nameEx("ssd_define_scenario")
### * ssd_define_scenario

flush(stderr())
flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: ssd_define_scenario
### Title: Define a Simulation Scenario
### Aliases: ssd_define_scenario

### ** Examples

ssd_define_scenario(
  ssddata::ccme_boron,
  nsim = 100L,
  nrow = c(5L, 10L),
  seed = 42L
)


base::assign(
  ".dptime",
  (proc.time() - get(".ptime", pos = "CheckExEnv")),
  pos = "CheckExEnv"
)
base::cat(
  "ssd_define_scenario",
  base::get(".format_ptime", pos = 'CheckExEnv')(get(
    ".dptime",
    pos = "CheckExEnv"
  )),
  "\n",
  file = base::get(".ExTimings", pos = 'CheckExEnv'),
  append = TRUE,
  sep = "\t"
)
cleanEx()
nameEx("ssd_run_scenario")
### * ssd_run_scenario

flush(stderr())
flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: ssd_run_scenario
### Title: Run Scenario
### Aliases: ssd_run_scenario ssd_run_scenario.data.frame
###   ssd_run_scenario.fitdists ssd_run_scenario.tmbfit
###   ssd_run_scenario.character ssd_run_scenario.function

### ** Examples

ssd_run_scenario(ssddata::ccme_boron, nsim = 2)

fit <- ssdtools::ssd_fit_dists(ssddata::ccme_boron)
ssd_run_scenario(fit, dist_sim = c("lnorm", "top"), nsim = 3)

fit <- ssdtools::ssd_fit_dists(ssddata::ccme_boron)
ssd_run_scenario(fit[[1]], nsim = 3)

ssd_run_scenario("rlnorm", nsim = 3)

ssd_run_scenario(ssdtools::ssd_rlnorm, nsim = 3)


base::assign(
  ".dptime",
  (proc.time() - get(".ptime", pos = "CheckExEnv")),
  pos = "CheckExEnv"
)
base::cat(
  "ssd_run_scenario",
  base::get(".format_ptime", pos = 'CheckExEnv')(get(
    ".dptime",
    pos = "CheckExEnv"
  )),
  "\n",
  file = base::get(".ExTimings", pos = 'CheckExEnv'),
  append = TRUE,
  sep = "\t"
)
cleanEx()
nameEx("ssd_run_scenario_baseline")
### * ssd_run_scenario_baseline

flush(stderr())
flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: ssd_run_scenario_baseline
### Title: Run a Scenario with the Baseline Loop Runner
### Aliases: ssd_run_scenario_baseline

### ** Examples

scenario <- ssd_define_scenario(
  ssddata::ccme_boron,
  nsim = 1L,
  nrow = 6L,
  seed = 42L,
  dists = "lnorm"
)
withr::with_seed(42L, {
  out <- ssd_run_scenario_baseline(scenario)
})
out$hc


base::assign(
  ".dptime",
  (proc.time() - get(".ptime", pos = "CheckExEnv")),
  pos = "CheckExEnv"
)
base::cat(
  "ssd_run_scenario_baseline",
  base::get(".format_ptime", pos = 'CheckExEnv')(get(
    ".dptime",
    pos = "CheckExEnv"
  )),
  "\n",
  file = base::get(".ExTimings", pos = 'CheckExEnv'),
  append = TRUE,
  sep = "\t"
)
cleanEx()
nameEx("ssd_scenario_data_tasks")
### * ssd_scenario_data_tasks

flush(stderr())
flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: ssd_scenario_data_tasks
### Title: Derive the data Task Table from a Scenario
### Aliases: ssd_scenario_data_tasks

### ** Examples

scenario <- ssd_define_scenario(
  ssddata::ccme_boron,
  nsim = 2L,
  nrow = c(5L, 10L),
  seed = 42L
)
ssd_scenario_data_tasks(scenario)


base::assign(
  ".dptime",
  (proc.time() - get(".ptime", pos = "CheckExEnv")),
  pos = "CheckExEnv"
)
base::cat(
  "ssd_scenario_data_tasks",
  base::get(".format_ptime", pos = 'CheckExEnv')(get(
    ".dptime",
    pos = "CheckExEnv"
  )),
  "\n",
  file = base::get(".ExTimings", pos = 'CheckExEnv'),
  append = TRUE,
  sep = "\t"
)
cleanEx()
nameEx("ssd_scenario_fit_tasks")
### * ssd_scenario_fit_tasks

flush(stderr())
flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: ssd_scenario_fit_tasks
### Title: Derive the fit Task Table from a Scenario
### Aliases: ssd_scenario_fit_tasks

### ** Examples

scenario <- ssd_define_scenario(
  ssddata::ccme_boron,
  nsim = 3L,
  seed = 42L,
  rescale = c(FALSE, TRUE)
)
ssd_scenario_fit_tasks(scenario)


base::assign(
  ".dptime",
  (proc.time() - get(".ptime", pos = "CheckExEnv")),
  pos = "CheckExEnv"
)
base::cat(
  "ssd_scenario_fit_tasks",
  base::get(".format_ptime", pos = 'CheckExEnv')(get(
    ".dptime",
    pos = "CheckExEnv"
  )),
  "\n",
  file = base::get(".ExTimings", pos = 'CheckExEnv'),
  append = TRUE,
  sep = "\t"
)
cleanEx()
nameEx("ssd_scenario_hc_tasks")
### * ssd_scenario_hc_tasks

flush(stderr())
flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: ssd_scenario_hc_tasks
### Title: Derive the hc Task Table from a Scenario
### Aliases: ssd_scenario_hc_tasks

### ** Examples

scenario <- ssd_define_scenario(
  ssddata::ccme_boron,
  nsim = 2L,
  seed = 42L,
  ci = c(FALSE, TRUE),
  nboot = c(10L, 100L)
)
ssd_scenario_hc_tasks(scenario)


base::assign(
  ".dptime",
  (proc.time() - get(".ptime", pos = "CheckExEnv")),
  pos = "CheckExEnv"
)
base::cat(
  "ssd_scenario_hc_tasks",
  base::get(".format_ptime", pos = 'CheckExEnv')(get(
    ".dptime",
    pos = "CheckExEnv"
  )),
  "\n",
  file = base::get(".ExTimings", pos = 'CheckExEnv'),
  append = TRUE,
  sep = "\t"
)
cleanEx()
nameEx("ssd_scenario_sample_tasks")
### * ssd_scenario_sample_tasks

flush(stderr())
flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: ssd_scenario_sample_tasks
### Title: Derive the sample Task Table from a Scenario
### Aliases: ssd_scenario_sample_tasks

### ** Examples

scenario <- ssd_define_scenario(ssddata::ccme_boron, nsim = 3L, seed = 42L)
ssd_scenario_sample_tasks(scenario)


base::assign(
  ".dptime",
  (proc.time() - get(".ptime", pos = "CheckExEnv")),
  pos = "CheckExEnv"
)
base::cat(
  "ssd_scenario_sample_tasks",
  base::get(".format_ptime", pos = 'CheckExEnv')(get(
    ".dptime",
    pos = "CheckExEnv"
  )),
  "\n",
  file = base::get(".ExTimings", pos = 'CheckExEnv'),
  append = TRUE,
  sep = "\t"
)
cleanEx()
nameEx("ssd_scenario_tasks")
### * ssd_scenario_tasks

flush(stderr())
flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: ssd_scenario_tasks
### Title: Expand a Scenario into all Four Task Tables
### Aliases: ssd_scenario_tasks

### ** Examples

scenario <- ssd_define_scenario(ssddata::ccme_boron, nsim = 3L, seed = 42L)
tasks <- ssd_scenario_tasks(scenario)
tasks
tasks$hc


base::assign(
  ".dptime",
  (proc.time() - get(".ptime", pos = "CheckExEnv")),
  pos = "CheckExEnv"
)
base::cat(
  "ssd_scenario_tasks",
  base::get(".format_ptime", pos = 'CheckExEnv')(get(
    ".dptime",
    pos = "CheckExEnv"
  )),
  "\n",
  file = base::get(".ExTimings", pos = 'CheckExEnv'),
  append = TRUE,
  sep = "\t"
)
cleanEx()
nameEx("ssd_sim_data")
### * ssd_sim_data

flush(stderr())
flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: ssd_sim_data
### Title: Generate Data for Simulations
### Aliases: ssd_sim_data ssd_sim_data.data.frame ssd_sim_data.fitdists
###   ssd_sim_data.tmbfit ssd_sim_data.character ssd_sim_data.function

### ** Examples

ssd_sim_data(ssddata::ccme_boron, nrow = 5, nsim = 3)

fit <- ssdtools::ssd_fit_dists(ssddata::ccme_boron)
ssd_sim_data(fit, nrow = 5, nsim = 3)

fit <- ssdtools::ssd_fit_dists(ssddata::ccme_boron)
ssd_sim_data(fit[[1]], nrow = 5, nsim = 3)

ssd_sim_data("rnorm", nrow = 5, nsim = 3)

ssd_sim_data(ssdtools::ssd_rlnorm, nrow = 5, nsim = 3)


base::assign(
  ".dptime",
  (proc.time() - get(".ptime", pos = "CheckExEnv")),
  pos = "CheckExEnv"
)
base::cat(
  "ssd_sim_data",
  base::get(".format_ptime", pos = 'CheckExEnv')(get(
    ".dptime",
    pos = "CheckExEnv"
  )),
  "\n",
  file = base::get(".ExTimings", pos = 'CheckExEnv'),
  append = TRUE,
  sep = "\t"
)
cleanEx()
nameEx("with_lecuyer_cmrg_seed")
### * with_lecuyer_cmrg_seed

flush(stderr())
flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: with_lecuyer_cmrg_seed
### Title: With L'Ecuyer-CMRG Seed
### Aliases: with_lecuyer_cmrg_seed

### ** Examples

with_lecuyer_cmrg_seed(42, {
  runif(3)
})


base::assign(
  ".dptime",
  (proc.time() - get(".ptime", pos = "CheckExEnv")),
  pos = "CheckExEnv"
)
base::cat(
  "with_lecuyer_cmrg_seed",
  base::get(".format_ptime", pos = 'CheckExEnv')(get(
    ".dptime",
    pos = "CheckExEnv"
  )),
  "\n",
  file = base::get(".ExTimings", pos = 'CheckExEnv'),
  append = TRUE,
  sep = "\t"
)
cleanEx()
nameEx("with_lecuyer_cmrg_state")
### * with_lecuyer_cmrg_state

flush(stderr())
flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: with_lecuyer_cmrg_state
### Title: With L'Ecuyer-CMRG State
### Aliases: with_lecuyer_cmrg_state

### ** Examples

state <- with_lecuyer_cmrg_seed(42, parallel::nextRNGStream(.Random.seed))
with_lecuyer_cmrg_state(state, runif(3))


base::assign(
  ".dptime",
  (proc.time() - get(".ptime", pos = "CheckExEnv")),
  pos = "CheckExEnv"
)
base::cat(
  "with_lecuyer_cmrg_state",
  base::get(".format_ptime", pos = 'CheckExEnv')(get(
    ".dptime",
    pos = "CheckExEnv"
  )),
  "\n",
  file = base::get(".ExTimings", pos = 'CheckExEnv'),
  append = TRUE,
  sep = "\t"
)
### * <FOOTER>
###
cleanEx()
options(digits = 7L)
base::cat(
  "Time elapsed: ",
  proc.time() - base::get("ptime", pos = 'CheckExEnv'),
  "\n"
)
grDevices::dev.off()
###
### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "\\(> \\)?### [*]+" ***
### End: ***
quit('no')
