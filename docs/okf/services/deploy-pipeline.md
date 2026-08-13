---
type: Service
title: Dashboard Deploy Pipeline
description: Dockerfile 2-stage image + GitHub Actions daily dashboard build (quarto render + staticrypt) + local build script, including the documented docs/ clobber risk.
resource: Dockerfile, .github/workflows/build-dashboard.yml, build-dashboard.sh
tags: [docker, github-actions, quarto, staticrypt, deploy]
timestamp: 2026-08-13
pure: false
---

# Responsibility

What this service does: builds the R + Quarto dashboard into a static site in
`docs/` (optionally password-encrypted with staticrypt), every day at 06:00 UTC
via GitHub Actions, on push to `main`, or manually. It also supports the same
build locally via `build-dashboard.sh`.

What it does NOT do: it does not run the tests, does not deploy elsewhere
(GitHub Pages is not used — `docs/` is committed directly), and does not
modify `docs/okf/` (the knowledge bundle lives in `docs/` too — see the
clobber risk below).

# Interface

Entry points:

- `Dockerfile` — the image consumed by both CI and local runs
- `.github/workflows/build-dashboard.yml` — CI orchestration
- `build-dashboard.sh` — local orchestration (Docker build + run)

# Dependencies

- Modules: all `R/` modules via [modules index](../modules/index.md) — the dashboard renders their outputs
- Docker base: `rocker/r-ver:4.5.1`
- Tooling: Node.js 18 + `staticrypt` (global npm), Quarto CLI v1.4.550 (`/usr/local/bin/quarto`)
- Secrets: `STATICRYPT_PASSWORD`, `REDCAP_API_TOKEN`, `GOOGLE_SERVICE_ACCOUNT_JSON`, `ABS_USERNAME`, `ABS_PASSWORD`

# Invariants

- **Dockerfile is 2-stage.** Stage 1 (`base`, `rocker/r-ver:4.5.1`) installs system deps, copies `renv.lock` + `renv/activate.R`, and runs `renv::restore()`. Stage 2 (runtime, same base) additionally installs Node 18 + staticrypt and the Quarto CLI, then copies the renv library from stage 1 (`COPY --from=base /project .`) and the app files. The env vars `REDCAP_API_TOKEN`/`GOOGLE_SERVICE_ACCOUNT_JSON`/`STATICRYPT_PASSWORD` are defaulted empty in the image.
- **CI is 2 jobs.** `build-image` (buildx → pushes `ghcr.io/moodlab/abmdash` with branch/sha/latest tags, GHA cache) and `build-dashboard` (`needs: build-image`; checks out the repo, `docker run`s the pushed image to render `inst/dashboard/index.qmd` via quarto into the mounted `docs/` volume, staticrypt-encrypts `*.html`, then commits `git add -f docs/` with a 🤖 message and pushes). Artifacts uploaded via `actions/upload-artifact`.
- **Triggers:** `schedule` cron `'0 6 * * *'` (daily 6 AM UTC) + `workflow_dispatch` + `push` to `main`. `concurrency` group `dashboard-build`, `cancel-in-progress: false`.
- **The docker-run write surface is narrow:** the R snippet writes only `index.html` and `site_libs/` into `/project/docs` (workflow L108-111); the staticrypt loop touches only `*.html` in the docs root (L114-134). So `docs/okf/` is left byte-identical, and the `git add -f docs/` commit step produces no diff for it.
- **LATENT CLOBBER RISK (documented only, NOT fixed):** `build-dashboard.sh` L53 runs `rm -rf docs` before `mkdir -p docs`. Executed locally, this DELETES `docs/okf/` (and `docs/evidence/`) before the Docker render rebuilds the dir — the knowledge bundle would be destroyed on a local run. The CI workflow does NOT do this (it mounts the repo's existing `docs/` and writes over it), so CI is safe. Do not run `build-dashboard.sh` locally without first backing up `docs/okf/`; the fix (e.g. `rm -rf docs/*` or removing the line) is intentionally out of scope for the docs/okf AC.

# Examples

```bash
# CI (safe): push to main, workflow_dispatch, or cron 06:00 UTC
# Local (DANGEROUS for docs/okf): ./build-dashboard.sh   # rm -rf docs at L53
```
