#!/usr/bin/env python3
"""Align ROADMAP.md with the OpenSpec change backlog.

Deterministic roadmap maintenance. Reads the OpenSpec change directories
directly (no `openspec` CLI dependency) and rewrites the actionable lines of
ROADMAP.md so that:

  * every active change's in-flight marker matches its task progress
    (no tasks done -> design, some done -> implementation, all done -> pending);
  * every in-flight change (work has started) sits in `## Now`;
  * every change whose directory has moved under `openspec/changes/archive/`
    is moved to `## Done` (most recent first);
  * `## Done` stays sorted newest-first.

Anything the script cannot confidently classify is reported as a loud
`WARN` / `ACTION NEEDED` line and left untouched for a human (or an LLM
driving the wrapping skill) to resolve. It only ever changes priority/
in-flight emoji and moves whole lines between sections — never the prose.

Usage:
    roadmap_align.py [--write] [--roadmap PATH] [--changes-dir PATH]

Without --write it prints the proposed diff and the report, changing nothing.
Exit status: 0 when nothing needs human attention, 3 when there are warnings.
"""

from __future__ import annotations

import argparse
import datetime as _dt
import difflib
import os
import re
import sys

# Priority emoji (most carry a U+FE0F variation selector) and the mutually
# exclusive in-flight markers, kept in sync with the roadmap legend.
PRIORITY = ["‼️", "❗️", "😀", "🐢", "📚", "💡", "✅"]
FLIGHT = ["⏳", "🛠️", "🚀"]

_PRIO_ALT = "|".join(re.escape(p) for p in PRIORITY)
_FLIGHT_ALT = "|".join(re.escape(f) for f in FLIGHT)

# An actionable backlog line: "- <prio>[<flight>] [<id>] <prose>".
# The id and prose are both optional (Bluesky lines may omit the id).
ACTION_RE = re.compile(
    r"^- (?P<prio>" + _PRIO_ALT + r")"
    r"(?P<flight>" + _FLIGHT_ALT + r")?"
    r"(?: \[(?P<id>[^\]]+)\])?"
    r"(?P<rest>(?: .*)?)$"
)

# A Done line: "- ✅ YYYY-MM-DD [<id>] [🔗](<link>) — <prose>".
DONE_RE = re.compile(
    r"^- ✅ (?P<date>\d{4}-\d{2}-\d{2}) \[(?P<id>[^\]]+)\] "
    r"\[🔗\]\((?P<link>[^)]+)\)(?: — (?P<prose>.*))?$"
)

ARCHIVE_DIR_RE = re.compile(r"^(?P<date>\d{4}-\d{2}-\d{2})-(?P<id>.+)$")

# Sections whose bullet lists this tool maintains. Other `##` sections
# (legend, Milestones, Dependencies, ...) are passed through verbatim.
MANAGED = ("Now", "Next", "Later", "Bluesky", "Done")


class Change:
    __slots__ = ("name", "done", "total")

    def __init__(self, name: str, done: int, total: int) -> None:
        self.name = name
        self.done = done
        self.total = total

    @property
    def started(self) -> bool:
        return self.done > 0

    @property
    def marker(self) -> str:
        if self.total > 0 and self.done >= self.total:
            return "🚀"
        if self.done > 0:
            return "🛠️"
        return "⏳"


def count_tasks(tasks_path: str) -> tuple[int, int]:
    done = total = 0
    with open(tasks_path, encoding="utf-8") as handle:
        for line in handle:
            stripped = line.lstrip()
            if stripped.startswith("- [x]") or stripped.startswith("- [X]"):
                done += 1
                total += 1
            elif stripped.startswith("- [ ]"):
                total += 1
    return done, total


def scan_active(changes_dir: str) -> dict[str, Change]:
    active: dict[str, Change] = {}
    for entry in sorted(os.listdir(changes_dir)):
        if entry in ("archive", "draft"):
            continue
        path = os.path.join(changes_dir, entry)
        if not os.path.isdir(path):
            continue
        if not (
            os.path.exists(os.path.join(path, "proposal.md"))
            or os.path.exists(os.path.join(path, "tasks.md"))
        ):
            continue
        tasks_path = os.path.join(path, "tasks.md")
        done, total = count_tasks(tasks_path) if os.path.exists(tasks_path) else (0, 0)
        active[entry] = Change(entry, done, total)
    return active


def scan_archived(changes_dir: str) -> dict[str, str]:
    """Map archived change name -> archive date (YYYY-MM-DD)."""
    archive_dir = os.path.join(changes_dir, "archive")
    archived: dict[str, str] = {}
    if not os.path.isdir(archive_dir):
        return archived
    for entry in sorted(os.listdir(archive_dir)):
        match = ARCHIVE_DIR_RE.match(entry)
        if match and os.path.isdir(os.path.join(archive_dir, entry)):
            # Latest date wins if a name was archived more than once.
            archived[match.group("id")] = match.group("date")
    return archived


class Section:
    def __init__(self, name: str | None, header: str | None) -> None:
        self.name = name  # None for the preamble before the first heading.
        self.header = header
        self.lines: list[str] = []


def split_sections(text: str) -> list[Section]:
    sections = [Section(None, None)]
    for line in text.split("\n"):
        heading = re.match(r"^## (.+?)\s*$", line)
        if heading:
            sections.append(Section(heading.group(1).strip(), line))
        else:
            sections[-1].lines.append(line)
    return sections


def render(sections: list[Section]) -> str:
    out: list[str] = []
    for section in sections:
        if section.header is not None:
            out.append(section.header)
        out.extend(section.lines)
    return "\n".join(out)


def done_link(changes_dir: str, date: str, name: str) -> str:
    # Keep the repo-relative form used in the Done log.
    base = changes_dir.rstrip("/")
    return f"{base}/archive/{date}-{name}/"


def make_done_line(changes_dir: str, date: str, name: str, prose: str) -> str:
    link = done_link(changes_dir, date, name)
    line = f"- ✅ {date} [{name}] [🔗]({link})"
    if prose:
        line += f" — {prose}"
    return line


def align(
    sections: list[Section],
    active: dict[str, Change],
    archived: dict[str, str],
    changes_dir: str,
    report: list[str],
    warnings: list[str],
) -> None:
    by_name = {s.name: s for s in sections if s.name}
    now = by_name.get("Now")
    done = by_name.get("Done")

    seen_ids: set[str] = set()
    move_to_now: list[str] = []
    move_to_done: list[str] = []

    for section in sections:
        if section.name not in MANAGED:
            continue
        kept: list[str] = []
        for line in section.lines:
            match = ACTION_RE.match(line)
            if not match or match.group("prio") == "✅":
                # Not an actionable backlog bullet (comment, blank, prose,
                # Done entry, table) — pass through untouched.
                if section.name == "Done":
                    done_match = DONE_RE.match(line)
                    if done_match:
                        seen_ids.add(done_match.group("id"))
                kept.append(line)
                continue

            cid = match.group("id")
            if cid is None:
                kept.append(line)  # e.g. a Bluesky line with no change id.
                continue
            seen_ids.add(cid)
            rest = (match.group("rest") or "").strip()

            if cid in archived:
                date = archived[cid]
                move_to_done.append(make_done_line(changes_dir, date, cid, rest))
                report.append(f"moved to Done: [{cid}] (archived {date}) from ## {section.name}")
                continue

            change = active.get(cid)
            if change is None:
                warnings.append(
                    f"WARN: [{cid}] in ## {section.name} is neither an active change "
                    f"nor archived under {changes_dir}/archive/ — left in place."
                )
                kept.append(line)
                continue

            # Align the in-flight marker to task progress. Any line backed by an
            # OpenSpec change carries a marker — including ⏳ for a not-yet-started
            # change. (Lines with no `[change-name]`, handled above, stay
            # unmarked.)
            have = match.group("flight")
            new_flight = change.marker
            new_line = f"- {match.group('prio')}{new_flight}"
            if cid:
                new_line += f" [{cid}]"
            new_line += match.group("rest") or ""
            if (have or "") != new_flight:
                report.append(
                    f"emoji: [{cid}] {have or '∅'} -> {new_flight or '∅'} "
                    f"({change.done}/{change.total})"
                )

            # In-flight work belongs in ## Now.
            if change.started and section.name != "Now":
                move_to_now.append(new_line)
                report.append(
                    f"moved to Now: [{cid}] in-flight ({change.done}/{change.total}) "
                    f"from ## {section.name}"
                )
            else:
                kept.append(new_line)
        section.lines = kept

    if move_to_now and now is not None:
        insert_bullets(now, move_to_now)
    if move_to_done and done is not None:
        done.lines.extend(move_to_done)

    if done is not None:
        sort_done(done)

    # Loud reporting for the one thing the deterministic pass cannot fix on its
    # own: an active change with no bullet anywhere needs prose only a human/LLM
    # can write. (The `## Done` log is a deliberately trimmed curation, not a
    # complete mirror of `archive/`, so a missing Done entry is NOT flagged —
    # this pass only *moves* lines that are already on the board into Done.)
    for name in sorted(active):
        if name not in seen_ids:
            warnings.append(
                f"ACTION NEEDED: active change [{name}] is not on the roadmap. "
                f"Add a one-line bullet (priority emoji + prose) to ## Now/## Next/## Later."
            )


def insert_bullets(section: Section, bullets: list[str]) -> None:
    """Append bullets after the last existing bullet line in the section."""
    last_bullet = -1
    for idx, line in enumerate(section.lines):
        if line.startswith("- "):
            last_bullet = idx
    if last_bullet == -1:
        # No real bullet yet (e.g. a `- …` placeholder or empty section):
        # drop them after the last non-blank line.
        last_content = -1
        for idx, line in enumerate(section.lines):
            if line.strip():
                last_content = idx
        last_bullet = last_content
    section.lines[last_bullet + 1 : last_bullet + 1] = bullets


def sort_done(section: Section) -> None:
    """Collapse the `- ✅` bullets into one contiguous newest-first block.

    Prose/comment lines before the first bullet are preserved; any blank or
    other lines interleaved with or trailing the bullets are pushed below the
    sorted block so the Done log reads as a single clean list.
    """
    bullets = [line for line in section.lines if line.startswith("- ✅ ")]
    if len(bullets) < 2:
        return

    def key(line: str) -> str:
        match = DONE_RE.match(line)
        return match.group("date") if match else "0000-00-00"

    bullets.sort(key=key, reverse=True)
    first = next(i for i, line in enumerate(section.lines) if line.startswith("- ✅ "))
    head = section.lines[:first]
    tail = [line for line in section.lines[first:] if not line.startswith("- ✅ ")]
    section.lines = head + bullets + tail


def main(argv: list[str] | None = None) -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--roadmap", default="ROADMAP.md")
    parser.add_argument("--changes-dir", default="openspec/changes")
    parser.add_argument("--write", action="store_true", help="apply changes in place")
    args = parser.parse_args(argv)

    if not os.path.isfile(args.roadmap):
        print(f"error: roadmap not found: {args.roadmap}", file=sys.stderr)
        return 2
    if not os.path.isdir(args.changes_dir):
        print(
            f"info: no OpenSpec changes directory at {args.changes_dir}; "
            "nothing to align.",
            file=sys.stderr,
        )
        return 0

    active = scan_active(args.changes_dir)
    archived = scan_archived(args.changes_dir)
    if not active and not archived:
        print("info: no active or archived changes found; nothing to align.")
        return 0

    original = open(args.roadmap, encoding="utf-8").read()
    sections = split_sections(original)

    report: list[str] = []
    warnings: list[str] = []
    align(sections, active, archived, args.changes_dir, report, warnings)

    updated = render(sections)

    print(f"== roadmap-align: {len(active)} active, {len(archived)} archived changes ==")
    if report:
        print(f"\nDeterministic changes ({len(report)}):")
        for line in report:
            print(f"  - {line}")
    else:
        print("\nNo emoji/movement changes needed.")
    if warnings:
        print(f"\n{'!' * 60}\nNeeds attention ({len(warnings)}):")
        for line in warnings:
            print(f"  {line}")
        print("!" * 60)

    if updated == original:
        print("\nROADMAP.md already aligned (no automatic edits).")
    elif args.write:
        with open(args.roadmap, "w", encoding="utf-8") as handle:
            handle.write(updated)
        print(f"\nWrote {args.roadmap}.")
    else:
        print("\n--- proposed diff (dry run; pass --write to apply) ---")
        diff = difflib.unified_diff(
            original.splitlines(keepends=True),
            updated.splitlines(keepends=True),
            fromfile="ROADMAP.md",
            tofile="ROADMAP.md (aligned)",
        )
        sys.stdout.writelines(diff)

    return 3 if warnings else 0


if __name__ == "__main__":
    raise SystemExit(main())
