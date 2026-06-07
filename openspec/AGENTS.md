# OpenSpec working guide (subdirectory)

This guide governs work **inside `openspec/`** — proposing, applying, syncing,
and archiving changes. It exists because the OpenSpec slash-commands/skills
(`/opsx:*`, `openspec-*`) are **generic and vendor-maintained**: they do not
know this project's conventions, so the project-specific obligations below are
easy to miss when a skill drives the workflow end-to-end. Read this before
running any OpenSpec skill; it sits alongside the root `AGENTS.md` (do not
treat the skill's own steps as the complete checklist).

## The roadmap update is part of every change-lifecycle action — NOT optional

`TARGETS-DESIGN.md` §12 is the project roadmap (a Mermaid DAG plus prose).
**Every** time you create, apply, sync, or archive a change — whether via a
skill (`/opsx:propose`, `/opsx:ff`, `/opsx:apply`, `/opsx:sync`,
`/opsx:archive`, and their `openspec-*` skill equivalents) or by hand — you
MUST reflect it in §12 **in the same change-set**, not as a follow-up. The
OpenSpec skills will *not* do this for you and will *not* remind you.

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

- **Propose** (`/opsx:propose`, `/opsx:ff`): add a Mermaid node + edges classed
  `proposed` (red) for a DAG step, **or** an off-DAG prose bullet for an
  independent tidy-up / new capability with no DAG prerequisites or dependants.
- **Apply / implement** (`/opsx:apply`): when implementation lands, move the
  node `proposed → done` (red → yellow).
- **Archive** (`/opsx:archive`): move the node `done → archived` (yellow →
  green) **and move its declaration into the `archived_box` subgraph** so the
  box always holds exactly the `archived`-class nodes. Then **re-check
  dependants**: any `open` node whose every prerequisite is now archived moves
  `open → ready` (blue). Update the §12 prose snapshot/addendum to match the
  graph so the two never contradict each other.
- **Sync** (`/opsx:sync`): syncing the delta into `openspec/specs/<cap>/spec.md`
  is the spec half; it does not by itself touch the roadmap, but if you sync as
  part of archiving, do the archive roadmap update above.

The authoritative, full statement of the colour/box rule is in the root
`AGENTS.md` → *Architectural Notes → The targets redesign*. This file is the
reminder that it applies to **every** lifecycle action; that file is the spec.

## Layout

- `changes/<name>/` — active changes: `proposal.md`, `design.md`,
  `specs/<capability>/spec.md`, `tasks.md`.
- `changes/archive/YYYY-MM-DD-<name>/` — archived changes (moved here by
  `/opsx:archive`).
- `specs/<capability>/spec.md` — the main specs; delta specs are synced in here
  on/before archive.
- `config.yaml` — OpenSpec project config.

## Conventions for spec prose

- UK/AU (Commonwealth) English (`-ise`/`-isation`, `-our`) — see the root
  `AGENTS.md` *Orthography* rule; code identifiers and base-R/API names keep
  their canonical spelling.
- Escape function/object/file names in backticks.
- `openspec validate --strict` should pass for any spec you touch.
