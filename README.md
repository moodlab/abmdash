# abmdash: An R package for creating an automatically updating dashboard

`abmdash` is an R package that builds a **private** Quarto dashboard from REDCap, Google Sheets, Google Calendar, and ABS data. It is rendered via Docker + Quarto, encrypted with staticrypt, and deployed to GitHub Pages every day (cron) and on every push to `main`. There is **no running server** — the output is static HTML pushed to the repo.

## Learn the repo

- [`docs/okf/index.md`](docs/okf/index.md) — the agent/human map: module docs, deploy pipeline, conventions. Read this first.
- [`docs/okf/modules/`](docs/okf/modules/index.md) — module concept docs for the R/ modules (week12_tracking.R is covered via the run_initial_function doc; a full OKF regen is a known follow-up)
- [`.opencode/skills/abmdash-guide/`](.opencode/skills/abmdash-guide/SKILL.md) — repo skill: learn-repo walkthrough + debug workflows.

## Troubleshooting

The vignettes are written for non-engineers and follow an "if you see X, here's the cause and the fix" pattern. They live in `vignettes/*.md` — plain Markdown, rendered directly by GitHub:

- [FAQ: runtime errors and their fixes](vignettes/faq.md) — most common errors with exact error text, cause, and fix
- [REDCap data access](vignettes/redcap-troubleshooting.md) — missing token, `""` vs `NA` bug class
- [Google Sheets and Calendar access](vignettes/google-troubleshooting.md) — service-account JSON quoting and sharing
- [ABS portal login and downloads](vignettes/abs-troubleshooting.md) — credentials, `Not authenticated`, connection quirks
- [Local Docker build](vignettes/docker-troubleshooting.md) — make targets, `docs/` clobber warning, image issues
- [Daily CI build](vignettes/ci-troubleshooting.md) — the two jobs, secrets, and the four failure modes

## Behavior-lock test suite

`tests/testthat/` is the executable spec: tests pin current behavior (e.g. [`test-redcap-behavior-lock.R`](tests/testthat/test-redcap-behavior-lock.R), [`test-snapshot-lock.R`](tests/testthat/test-snapshot-lock.R)). Harness utilities in [`helper-harness.R`](tests/testthat/helper-harness.R) provide `with_fixed_clock()` (pins time/TZ) and `local_isolated_env()` (unsets API credentials). [`RECORDING.md`](RECORDING.md) documents the fixture recording + redaction workflow (fixtures live in `tests/testthat/fixtures/`).

Run tests:

```sh
make test                 # or: Rscript -e 'devtools::test()'
make test-trad            # trad-compliance tests only
```

## Build & run

- Restore renv (`renv::restore()`) BEFORE running `make test` on a fresh clone.
- **Makefile targets** — `test`, `test-trad`, `docker-build`, `docker-test-auth`, `docker-render` (full local render, same as CI), `lint`, `serve` (serves `docs/` on :8000).
- [`build-dashboard.sh`](build-dashboard.sh) — local deploy. **WARNING: it runs `rm -rf docs` — back up anything under `docs/okf/` you want to keep first.**
- [`load-env.sh`](load-env.sh) — sources `.Renviron` (API credentials) for local runs.
- **renv** — package versions are locked by `renv.lock`; run `renv::restore()` after cloning.
- **Codegraph index** — `.codegraph/` is a machine-local symbol index for coding agents (gitignored, never committed). Run `codegraph init` from the repo root after structural changes to `R/`, or when agent navigation seems stale.

## Contributing

1. **Clone** — `git clone git@github.com:moodlab/abmdash.git`
2. **Create an issue** describing what you'll change (prefix `feat:`, `bug:`, or `maint:`). To request someone else do the work, describe current vs desired behavior and assign the issue.
3. **Branch** — `git switch -c <your-github-username>/<short-description>` (e.g. `mcmullarkey/add-pre-commit`). `main` is protected; feature branches are how changes get reviewed.
4. **Change + commit** — include `Closes #<issue-number>` in a commit message so the issue auto-closes on merge.
5. **Push + PR** — `git push origin <branch>`, then open a pull request from the URL GitHub prints.

**Pre-commit hooks** run styler/lintr on every commit (CI enforces the same checks). Install once from R: `install.packages("precommit")` then `precommit::use_precommit()`. If a hook fails the fixes are usually automatic — re-stage (`git add .`) and re-commit; otherwise read the hook output and file an issue if stuck.

Deployment is automatic: GitHub Actions rebuilds the dashboard on push to `main` and on the daily cron — you only push code. Thanks for contributing!
