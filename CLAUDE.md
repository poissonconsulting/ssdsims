Always follow [AGENTS.md](AGENTS.md) in the main directory and the subdirectories.

## Pull Requests

PR titles **must** be valid Conventional Commits — `<type>: <summary>` or
`<type>(<scope>): <summary>`, where `type` is one of `feat`, `fix`, `docs`,
`refactor`, `test`, `chore`, etc. (e.g.
`feat: add scenario-scoped dqrng pcg64 RNG backend`,
`docs(openspec): propose registry, manifest, and task-tables changes`). Use the
imperative mood and keep the summary concise.

This is load-bearing: **PRs are squash-merged**, so the PR title becomes the
single commit on `main`, and that commit is what `fledge` reads to build
`NEWS.md`. Individual commits on the branch are squashed away — their messages
are not consumed by `fledge`, so a clean history helps review but the title is
what ships.

See the *Pull Requests* section of [AGENTS.md](AGENTS.md) for the extended PR
instructions (updating the title/description, backticking names, referencing
issues, and review-thread etiquette).
