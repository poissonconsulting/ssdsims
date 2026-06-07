# The scenario for the small targets example, shared by `_targets.R` (the
# targets pipeline driven by run.R) and `run-serial.R` (the single-core shard
# runner). Both source this file, so they run the *same* study and their
# results can be compared. Edit to taste.
library(ssdsims)
# ssdsims uses dqrng for reproducible per-task seeding but never loads it
# itself (it is a Suggested dependency). Load it here so the shard runners --
# in this process and in every targets worker that sources this file -- can
# activate the dqrng backend; without it a run aborts with a reminder.
library(dqrng)

scenario <- ssd_define_scenario(
  ssddata::ccme_boron,
  nsim = 2L,
  seed = 42L,
  nrow = c(5L, 10L),
  rescale = c(FALSE, TRUE)
)
