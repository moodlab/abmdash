# Troubleshooting the local Docker build

## What this guide is for

The dashboard is an R + Quarto site. It is built inside a Docker image
(`Dockerfile`) and rendered to static HTML in the `docs/` folder. The same
image and render steps run locally and in CI, so a local error usually means
the daily build will fail too — this guide helps you find it before CI does.

The image is **2-stage**: stage 1 (`rocker/r-ver:4.5.1`) installs system
packages and restores the R library from `renv.lock`; stage 2 adds Node.js 18
with the `staticrypt` password tool and the Quarto CLI (v1.4.550), then copies
the ready-made library from stage 1.

## Read this first: `build-dashboard.sh` clobbers `docs/`

`make docker-render` runs `build-dashboard.sh`, and that script starts by
deleting the whole output directory:

```
# build-dashboard.sh, near the top:
rm -rf docs
mkdir -p docs
```

Because `docs/okf/` (the knowledge bundle) also lives inside `docs/`, a local
`make docker-render` **deletes it before rendering anything**. CI is safe —
the workflow mounts the repo's existing `docs/` and writes over it instead of
deleting it — but your laptop is not.

**Before any local `make docker-render`, back up the bundle:**

```
cp -r docs/okf /tmp/okf-backup
make docker-render          # WARNING: this just did rm -rf docs
cp -r /tmp/okf-backup docs/okf   # restore what the script deleted
```

If you already ran it and `docs/okf/` is gone, restore it from git instead:

```
git checkout -- docs/okf
```

Do not attempt to "fix" the script yourself — the clobber line is a known,
documented risk (see `../docs/okf/services/deploy-pipeline.md`) and removing
it is intentionally out of scope.

## The build targets at a glance

All seven maintained targets:

| Target | What it does |
|---|---|
| `make test` | Runs the full test suite |
| `make test-trad` | Runs only the trad-compliance tests |
| `make docker-build` | Builds the Docker image locally (same build as CI) |
| `make docker-render` | Full local render — runs `build-dashboard.sh` (see clobber warning above) |
| `make docker-test-auth` | Builds the image, then logs into ABS inside the container to verify credentials |
| `make lint` | Runs R package checks (`devtools::check()`) |
| `make serve` | Serves the rendered `docs/` locally at `http://localhost:8000` |

## Common errors and fixes

**If you see** → **Cause** → **Fix**:

| If you see | Cause | Fix |
|---|---|---|
| `no space left on device` / Docker build dies mid-layer | The buildx cache in `/tmp/docker-cache-abmdash` (or the Docker disk) is full | Free disk space and clear the cache: `rm -rf /tmp/docker-cache-abmdash` |
| `renv::restore()` fails while building the image | A package in `renv.lock` can no longer be installed from CRAN, or the lock drifted from what the code needs | Do not hand-edit `renv.lock`. Recreate it properly with `renv::snapshot()` if a dependency changed, and keep the lock committed |
| `❌ ERROR: Dashboard was not created` from `build-dashboard.sh` | The Docker render step failed — usually a missing or wrong environment variable (the script prints `✓`/`✗` per credential before building) | Re-run with `set -x` or read the printed credential checks; see the REDCap / Google / ABS vignettes for the variable-specific fix |
| `staticrypt: command not found` | Node.js 18 or the global `staticrypt` npm install is missing from the image | Rebuild from scratch (`make docker-build` with `--no-cache` via `docker buildx build --no-cache`), or reinstall Node 18 in stage 2 |
| Package `abmdash` not found / `Error in library(abmdash)` | The image is stale or the local build skipped the package install step | The render script always reinstalls the package from source; force a clean rebuild if the image predates your R code changes |

## Inspecting the image: `debug-docker.sh`

If the build succeeds but rendering misbehaves, `debug-docker.sh` opens a shell
inside the built image and prints the renv library state, `.Rprofile`, and
`RENV` variables — everything you need to tell whether the image contains what
the render expects:

```
make docker-build      # ensure the image exists first
bash debug-docker.sh
```

## Still stuck?

- Read the full pipeline notes in `../docs/okf/services/deploy-pipeline.md`
  (image layout, CI parity, and the clobber risk).
- If the failure is specific to one data source, start with that source's
  vignette (REDCap, Google, or ABS) — the local render loads all of them, and
  one bad credential fails the whole render.
