# Whole-scenario pipeline

A single target writes one Parquet file for the entire scenario.
Useful as a baseline / control case to compare against the branched
variants. Any knob change re-runs the whole pipeline.

Files written: `results/whole.parquet`.
