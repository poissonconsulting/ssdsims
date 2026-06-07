# OpenSpec working guide (subdirectory)

This guide governs work **inside `openspec/`** — proposing, applying, syncing,
and archiving changes. It exists because the OpenSpec slash-commands/skills
(`/opsx:*`, `openspec-*`) are **generic and vendor-maintained**: they do not
know this project's conventions, so the project-specific obligations below are
easy to miss when a skill drives the workflow end-to-end. Read this before
running any OpenSpec skill; it sits alongside the root `AGENTS.md` (do not
treat the skill's own steps as the complete checklist).

## The roadmap update is part of every change-lifecycle action — NOT optional

The project roadmap spans **two files** and you MUST keep both current:
[`ROADMAP.md`](../ROADMAP.md) carries the forward-looking backlog and shipped
log (the `initiative`-template `Now` / `Next` / `Later` / `Bluesky` / `Done`
prose), and `TARGETS-DESIGN.md` §12 carries the **dependency DAG** (a Mermaid
graph colouring every node by status) plus the `### Archived` prose for landed
steps. **Every** time you create, apply, sync, or archive a change — whether via
a skill (`/opsx:propose`, `/opsx:ff`, `/opsx:apply`, `/opsx:sync`,
`/opsx:archive`, and their `openspec-*` skill equivalents) or by hand — you MUST
reflect it across these two files **in the same change-set**, not as a
follow-up. The OpenSpec skills will *not* do this for you and will *not* remind
you.

Node status colours (Mermaid `classDef`s applied by `class` lines at the foot
of the graph):

| Colour | Class | Meaning |
|--------|-------|---------|
| 🟩 green | `archived` | change archived |
| 🟨 yellow | `done` | implemented, not yet archived |
| 🟥 red | `proposed` | artifacts exist, not implemented |
| 🟦 blue | `ready` | no artifacts yet, but every prerequisite has landed — ready to propose |
| ⬜ unfilled | `open` | roadmap only, still blocked by an un-landed prerequisite |

### Per-action checklist

- **Propose** (`/opsx:propose`, `/opsx:ff`): add a `ROADMAP.md` line under
  `## Now`/`## Next`/`## Later` (priority emoji + `[change]` identifier). For a
  **DAG step** also add a Mermaid node + edges classed `proposed` (red) in §12;
  an **off-DAG** independent tidy-up / new capability (no DAG prerequisites or
  dependants) is `ROADMAP.md`-only — it gets **no** §12 node and no §12 prose
  until it archives.
- **Apply / implement** (`/opsx:apply`): when implementation lands, move the §12
  node `proposed → done` (red → yellow) and update the `ROADMAP.md` line's
  in-flight emoji (`🛠️` / `🚀`).
- **Archive** (`/opsx:archive`): move the §12 node `done → archived` (yellow →
  green) **and move its declaration into the `archived_box` subgraph** so the
  box always holds exactly the `archived`-class nodes; add the change's
  `### Archived` prose bullet in §12 (in implementation order). Move its
  `ROADMAP.md` line to `## Done` (`✅ YYYY-MM-DD [change] [🔗](archive-path) — …`).
  Then **re-check dependants**: any `open` node whose every prerequisite is now
  archived moves `open → ready` (blue). Update the §12 status snapshot to match
  the graph so the two never contradict each other.
- **Sync** (`/opsx:sync`): syncing the delta into `openspec/specs/<cap>/spec.md`
  is the spec half; it does not by itself touch the roadmap, but if you sync as
  part of archiving, do the archive roadmap update above.

The authoritative, full statement of the colour/box rule is **here** plus the
legend at the foot of the `TARGETS-DESIGN.md` §12 graph itself. The root
`AGENTS.md` only points here; keep this file and the §12 legend in step.

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
