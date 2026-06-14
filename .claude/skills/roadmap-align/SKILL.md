---
name: roadmap-align
description: Align ROADMAP.md with the OpenSpec change backlog — correct in-flight emoji, move in-flight changes to Now, move archived changes to Done (newest first), and report anything it can't classify. Use after archiving changes, after ticking tasks, or whenever the roadmap may have drifted from openspec/changes/.
license: MIT
compatibility: Requires python3. Reads openspec/changes/ directly; the openspec CLI is not required.
---

Align `ROADMAP.md` with the current state of the OpenSpec backlog.

**This skill is the *only* sanctioned way to make automated edits to `ROADMAP.md`.** The roadmap is otherwise a hand-edited document. Do **not** edit it directly from other workflows (archiving, applying tasks, etc.): the merge-conflict surface is high and the file is curated by humans. When some *other* task would "add something to the roadmap", don't — instead hand the user a copy-pasteable one-line Markdown fragment (one bullet per change, no surrounding structure) for them to paste where they want it. See the host repo's `AGENTS.md` / `CLAUDE.md` for that convention.

This skill leans on a small **deterministic script** (`roadmap_align.py`, alongside this file) that does everything it can recognise and **talks loudly** about everything it can't. Most invocations are fully handled by the script; only a few residual cases need your judgement.

## What the script does (deterministic)

Given `openspec/changes/` (active changes + the `archive/` log) and `ROADMAP.md`, it makes **only emoji changes and line movements** — never prose edits:

1. **In-flight emoji alignment.** For every actionable line carrying a `[change-name]` that maps to an active change, set the in-flight marker to match task progress: `⏳` (no tasks done), `🛠️` (some done), `🚀` (all done). Every line backed by an OpenSpec change carries a marker — including `⏳` for a change that has a proposal but no ticked tasks yet. Lines with **no** `[change-name]` (plain prose bullets) stay unmarked.
2. **In-flight → `## Now`.** Any active change whose work has started (≥1 task done) is moved into `## Now`.
3. **Archived → `## Done`.** Any line whose `[change-name]` now lives under `openspec/changes/archive/<date>-<name>/` is reformatted into a Done entry — `- ✅ <date> [<name>] [🔗](…) — <prose>` — and moved to `## Done`, preserving its prose.
4. **Sort `## Done`** newest-first (last done first), as one contiguous block.

It leaves every other `##` section (legend, Milestones, Dependencies, …), all comments, and all prose untouched. Re-running it on an aligned roadmap is a no-op.

## What needs you (the "loud" cases)

The script prints `WARN` / `ACTION NEEDED` lines and exits non-zero (3) when it finds:

- **`WARN: [id] … is neither an active change nor archived`** — a bulleted `[id]` with no matching change directory. Usually a deliberate placeholder (a not-yet-created change, or a non-OpenSpec idea). Leave it unless the user says otherwise; if it's a typo of a real change, fix the line by hand.
- **`ACTION NEEDED: active change [id] is not on the roadmap`** — an active change with no bullet anywhere. Add a one-line bullet with a priority emoji and a short description (the script can't invent prose). Put it in `## Now`/`## Next`/`## Later` per its readiness.

Two more editorial calls the script deliberately does **not** make for you (the `## Done` log is a *curated, trimmed* view, not a full mirror of `archive/`):

- **Freshly archived changes that were never on the board.** If you just archived a change that had no roadmap bullet, add its Done entry yourself (date + `🔗` + a one-line summary from its proposal). The script only *moves* bullets that already existed.
- **Trimming.** Keep `## Done` to a readable recent window; older entries live in `archive/`.

## Steps

1. **Locate the repo root** (where `ROADMAP.md` and `openspec/` live) and `cd` there.
2. **Dry run** to preview and surface warnings:

   ```bash
   python3 .claude/skills/roadmap-align/roadmap_align.py
   ```

   Read the deterministic-change list and the diff. Investigate every `WARN` / `ACTION NEEDED`.
3. **Resolve the loud cases** by hand (add missing-change bullets, add Done entries for freshly-archived off-board changes, fix typos). Re-run the dry run until only expected, deliberate warnings remain.
4. **Apply:**

   ```bash
   python3 .claude/skills/roadmap-align/roadmap_align.py --write
   ```

5. **Verify idempotency:** run the dry run once more; it should report "No emoji/movement changes needed" with only the deliberate warnings left.
6. **Show the user the diff** and the residual warnings. Don't open a PR or push unless asked.

## Flags

- `--roadmap PATH` — roadmap file (default `ROADMAP.md`).
- `--changes-dir PATH` — OpenSpec changes dir (default `openspec/changes`).
- `--write` — apply in place (default is a dry-run diff).

## Guardrails

- **Only this skill edits the roadmap programmatically.** Elsewhere, emit a copy-pasteable fragment instead.
- The script never edits prose, never reorders `## Next`/`## Later`/`## Bluesky` beyond the in-flight→Now move, and never invents bullets. If a desired change isn't "an emoji change or a movement", do it by hand.
- A fresh template or an initiative with no `openspec/changes/` is a clean no-op — the script says so and exits 0.
- Respect the host repo's roadmap legend; this skill assumes the Now/Next/Later/Bluesky/Done schema with the priority + in-flight emoji legend documented in `ROADMAP.md`.
