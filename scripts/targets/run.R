## Run the pipeline from this directory:
##   setwd("scripts/targets")
##   source("run.R")
##
## Or from anywhere:
##   targets::tar_make(script = "scripts/targets/_targets.R",
##                     store  = "scripts/targets/_targets")

targets::tar_make()

## Inspect what was built:
##   targets::tar_manifest()
##   targets::tar_visnetwork()
##   targets::tar_read(basic_hcs)
##   targets::tar_read(scenario1_parquet)
