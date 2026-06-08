# Read a Per-Scenario Manifest

Reads `<dir>/manifest.json` back into an R list, round-tripping the
declarative fields and session-info block written by
[`ssd_write_manifest()`](https://poissonconsulting.github.io/ssdsims/reference/ssd_write_manifest.md)
(and the `completed_shards` tail added by
[`ssd_assemble_manifest()`](https://poissonconsulting.github.io/ssdsims/reference/ssd_assemble_manifest.md))
without lossy coercion: `seed`/`nboot` are restored as whole numbers and
logical knobs (`rescale`, `parametric`, ...) as logicals.

## Usage

``` r
ssd_read_manifest(dir)
```

## Arguments

- dir:

  The results directory to write `manifest.json` into (created if it
  does not exist).

## Value

The manifest as an R list, with the scalar/vector knob types of the
written scenario restored.

## See also

[`ssd_write_manifest()`](https://poissonconsulting.github.io/ssdsims/reference/ssd_write_manifest.md).
