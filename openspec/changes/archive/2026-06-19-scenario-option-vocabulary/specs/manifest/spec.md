# manifest Delta

## MODIFIED Requirements

### Requirement: Read a manifest back
The package SHALL provide `ssd_read_manifest(dir)` that reads `<dir>/manifest.json` into an R list, round-tripping the declarative fields and session info written by `ssd_write_manifest()` without lossy coercion of integer or logical scenario options.

#### Scenario: Round-trip preserves manifest contents
- **WHEN** a manifest is written for a scenario and then read back
- **THEN** the read fields SHALL equal the written values, with `seed`/`nboot` preserved as whole numbers and logical scenario options preserved as logicals
