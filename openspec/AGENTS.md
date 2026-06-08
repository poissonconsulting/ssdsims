# OpenSpec working guide (subdirectory)

This guide governs work **inside `openspec/`** — proposing, applying, syncing,
and archiving changes. It exists because the OpenSpec slash-commands/skills
(`/opsx:*`, `openspec-*`) are **generic and vendor-maintained**: they do not
know this project's conventions, so the project-specific obligations below are
easy to miss when a skill drives the workflow end-to-end. Read this before
running any OpenSpec skill; it sits alongside the root `AGENTS.md` (do not
treat the skill's own steps as the complete checklist).

## Layout

- `changes/<name>/` — active changes: `proposal.md`, `design.md`,
  `specs/<capability>/spec.md`, `tasks.md`, and an optional `exploration/`
  (see below).
- `changes/archive/YYYY-MM-DD-<name>/` — archived changes (moved here by
  `/opsx:archive`).
- `specs/<capability>/spec.md` — the main specs; delta specs are synced in here
  on/before archive.
- `config.yaml` — OpenSpec project config.

## Explorations live with their change

A change's proof-of-work / investigation scripts (the evidence behind a design
decision) belong in that change's own `changes/<name>/exploration/` directory —
**never** at the repo top level. This keeps them travelling with the change
(and into `changes/archive/` on archive) and out of the shipped package: the
root `^openspec$` entry in `.Rbuildignore` already excludes every change's
`exploration/` from `R CMD build`/`check`, so **do not** add a separate
top-level `exploration` ignore. Reference these scripts from the change's
artifacts as `exploration/<script>.R` (relative to the change); from shipped
source name the owning change (e.g. *"the `est-method-setting` change's
`exploration/...`"*) so the path survives archiving. Existing examples:
`cost-estimation/exploration/`, `task-rng-postcheck/exploration/`,
`est-method-setting/exploration/`.

## Conventions for spec prose

- UK/AU (Commonwealth) English (`-ise`/`-isation`, `-our`) — see the root
  `AGENTS.md` *Orthography* rule; code identifiers and base-R/API names keep
  their canonical spelling.
- Escape function/object/file names in backticks.
- `openspec validate --strict` should pass for any spec you touch.
