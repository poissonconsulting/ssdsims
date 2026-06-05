See [AGENTS.md](AGENTS.md) for the development guide and conventions (and
[tests/testthat/AGENTS.md](tests/testthat/AGENTS.md) for test-suite conventions).

Before opening **or updating** a PR, apply the **Pull Requests** conventions in
AGENTS.md: the **title must be a Conventional Commit** (`<type>(<scope>):
<summary>`) — PRs are squash-merged, so the title becomes the single `main`
commit and the `fledge` `NEWS.md` entry — and the description must capture the
change's *current* state, not a revision/process log.
