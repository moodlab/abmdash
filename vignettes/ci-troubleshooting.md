# Troubleshooting the daily CI build

## What this guide is for

Every day at 06:00 UTC a GitHub Actions workflow (`.github/workflows/
build-dashboard.yml`) rebuilds the dashboard and commits the result straight
into the repo's `docs/` folder. When that workflow goes red, this guide tells
you which job failed and what to do about it.

**There is no separate deployment step.** The workflow commits the rendered
`docs/` directory directly — the repo itself is the deployed site. There are
no Pages settings to check and no deployment branch to push to.

## The two jobs

The workflow has exactly two jobs:

| Job | What it does |
|---|---|
| `build-image` | Builds the Docker image with `buildx` and pushes it to `ghcr.io/moodlab/abmdash` with `branch`, `sha`, and `latest` tags |
| `build-dashboard` | Waits for `build-image` (`needs: build-image`), then `docker run`s the pushed image: renders `inst/dashboard/index.qmd` with Quarto, encrypts the HTML with `staticrypt`, and commits the result with `git add -f docs/` → `git commit` → `git pull --rebase` → `git push` |

The workflow runs on three triggers: the `0 6 * * *` cron schedule, manual
runs (`workflow_dispatch`), and pushes to `main`. A `concurrency` group
(`dashboard-build`, `cancel-in-progress: false`) ensures builds never cancel
each other — a manual run and the cron can queue, but never interrupt.

## The five secrets

The workflow reads five repository secrets, all set in
**Settings → Secrets and variables → Actions**:

| Secret | Used for |
|---|---|
| `STATICRYPT_PASSWORD` | Encrypting the rendered HTML (password gate for readers) |
| `REDCAP_API_TOKEN` | REDCap data access |
| `GOOGLE_SERVICE_ACCOUNT_JSON` | Google Sheets and Calendar access |
| `ABS_USERNAME` | ABS portal login |
| `ABS_PASSWORD` | ABS portal login |

A missing or wrong secret shows up only in the `build-dashboard` job, and
usually as the exact error text described in the matching troubleshooting
vignette (REDCap, Google, or ABS).

## Four failure modes and their fixes

**If you see** → **Cause** → **Fix**:

| If you see | Cause | Fix |
|---|---|---|
| `build-image` job red: the "Build and push Docker image" step exits nonzero | `renv::restore()` or the base-image pull failed — network, lock drift, or a Docker registry hiccup | Open the failed run (Actions → the red run) and read the step log. For a transient pull failure, re-run the workflow (Actions → Re-run jobs). For a lock problem, see the Docker vignette's `renv::restore()` row |
| `build-dashboard` job red: the `docker run` step shows a token/sheet/login error | One of the five secrets is missing, empty, or rotated | Compare the error text with the REDCap / Google / ABS vignettes, then update the secret in Settings and re-run the workflow |
| `build-dashboard` job red: the "Commit and push updated dashboard" step fails on `git pull --rebase` | Someone pushed to `main` while the workflow was rendering, and the rebase hit a conflict in `docs/` | Re-run the workflow. If it fails again, look at what changed in `docs/` on `main` and reconcile the conflicting HTML |
| Workflow green, but the dashboard on the repo shows stale or blank numbers | The last successful run produced no visual change (dashboard re-rendered identically), or you are looking at an old cached page — a genuinely unreachable data source usually turns the run **red**, so check the job log first | Check the artifacts: every run uploads `docs/` as an artifact (`dashboard-<run number>`, kept 30 days) with the exact HTML that was committed. Compare artifact timestamps with the data you expected |

## Reading the run log

Every workflow run is visible under **Actions**. To confirm which job you are
looking at, the run page lists both jobs side by side. Red means failed; a
failed `build-dashboard` job still shows you the full console output of every
step, including the R render and `staticrypt` output — the error text in the
log is what the REDCap / Google / ABS vignettes match against.

## Still stuck?

- Read the full pipeline notes in `../docs/okf/services/deploy-pipeline.md` —
  it documents the 2-job layout, the triggers, and why `docs/okf/` survives
  the CI render.
- A failure that also reproduces locally is usually a credential or image
  problem: see the Docker troubleshooting vignette first, then the data-source
  vignette that matches the error text.
