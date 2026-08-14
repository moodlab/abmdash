# Frequently Asked Questions: runtime errors and their fixes

## What this guide is for

This page collects the most common runtime errors in this project and gives
each one the same treatment: **If you see** (the exact error text) → **Cause**
(why it happens) → **Fix** (the steps to take in *this* repo).

Every error string quoted below appears verbatim somewhere in the repository
source (`R/`, `Makefile`, `Dockerfile`, `build-dashboard.sh`, the GitHub
Actions workflow) — if you search the code for it, you will find it. Every fix
step names a command that already exists in this repo; there is no generic
internet advice here.

All code blocks are shown for reference only and are **never executed** while
you read this page (plain Markdown, so GitHub renders it without running
anything).

The dashboard reads from REDCap, Google Sheets/Calendar, and the ABS portal,
then renders with Quarto and encrypts with staticrypt. If you are not sure
which of the sections below applies to you, start with the module map in
[`../docs/okf/index.md`](../docs/okf/index.md), then come back here for the
fix.

## 1. REDCap empty-string bug: "row names contain missing values"

**If you see** (raised by `data.frame()`):

```
row names contain missing values
```

**Cause.** REDCap sends an empty string (`""`) — not `NA` — for any field a
participant never filled in. R treats `""` as *text*. Converting it with
`as.numeric("")` silently produces `NA`, and an `NA` used as a row index
inserts an all-`NA` row. When `data.frame()` then tries to build a table, it
refuses and raises *"row names contain missing values"*.

**Fix.** The fix is a code pattern, not a setting. Parse each numeric field
**once** and guard on the *parsed* value, never on the raw string:

```
# BAD: guard on the RAW string, then convert -- NA leaks into the row index
# raw[!is.na(raw) & as.numeric(raw) >= 17]

# GOOD: parse once, then guard on the PARSED value
score <- suppressWarnings(as.numeric(raw))
eligible <- score[!is.na(score) & score >= 17]
```

Two rules prevent this whole class of bug:

1. **Parse once** with `suppressWarnings(as.numeric(...))`, then test the new
   vector. Never write `!is.na(x) & as.numeric(x) >= N`.
2. **Keep values out of names.** When using `sapply()`, pass
   `USE.NAMES = FALSE` so a value vector never becomes the row names of the
   result.

This bug class affected **both** functions that filter REDCap records by PHQ-8
score: `get_eligible_participants()` and `get_weekly_screening_stats()`
(both in `R/redcap_api.R`). It shipped once already (issue #38) and was fixed
in **PR #39**. If you still see the error, the raw-string guard has been
re-introduced somewhere — the module notes in
[`../docs/okf/modules/redcap_api.md`](../docs/okf/modules/redcap_api.md)
document the `""` invariant and the parse-once rule in detail.

## 2. Missing REDCap token

**If you see:**

```
REDCAP_API_TOKEN environment variable is not set or is empty
```

**Cause.** The dashboard reads the token from the `REDCAP_API_TOKEN`
environment variable (`R/redcap_api.R`). It is missing or blank.

**Fix.**

1. In the file `.Renviron` at the **repo root**, add one line:
   `REDCAP_API_TOKEN=<your-token>` (no quotes needed).
2. From the repo root, load it: `source ./load-env.sh`.
3. Restart R, then confirm it stuck:
   `Sys.getenv("REDCAP_API_TOKEN")`.

If the error appears in the daily CI build instead, the token is set as a
repository secret, not in `.Renviron` — see the CI section (entry 7) below.

## 3. Google service account problems

**If you see:**

```
GOOGLE_SERVICE_ACCOUNT_JSON environment variable is not set or is empty
```

**Cause.** The dashboard authenticates to Google Sheets and Google Calendar
with a service-account JSON document stored in the `GOOGLE_SERVICE_ACCOUNT_JSON`
environment variable (`R/gsheet_api.R`, `R/gcal_api.R`). The variable is
missing or empty.

**Fix.** Put the **entire JSON document on one line**, wrapped in double
quotes, in `.Renviron` at the repo root, e.g.:

```
GOOGLE_SERVICE_ACCOUNT_JSON="{"type":"service_account","project_id":"...","client_email":"...","private_key":"-----BEGIN PRIVATE KEY-----\n...\n-----END PRIVATE KEY-----\n"}"
```

Then `source ./load-env.sh` and restart R. The module strips the outer quotes
and un-escapes inner ones itself, so do **not** try to un-escape `\"` or `\n`
by hand — pasting the JSON exactly as the Google Cloud console shows it works.

**If you see instead:**

```
Failed to parse GOOGLE_SERVICE_ACCOUNT_JSON:
```

**Cause.** The variable is set, but the JSON document itself is broken — most
often because the paste lost characters, or the JSON spans multiple lines
instead of one.

**Fix.** Re-copy the full JSON from Google Cloud Console → the service
account's *Keys* tab, replace the value in `.Renviron` with a single quoted
line, then `source ./load-env.sh` and restart R. If you are re-using a
service-account key, see the sharing notes in
[`../docs/okf/modules/gsheet_api.md`](../docs/okf/modules/gsheet_api.md) and
[`../docs/okf/modules/gcal_api.md`](../docs/okf/modules/gcal_api.md).

## 4. ABS portal login and downloads

**If you see:**

```
ABS_USERNAME environment variable is not set
```

or

```
ABS_PASSWORD environment variable is not set
```

**Cause.** The ABS login needs both `ABS_USERNAME` and `ABS_PASSWORD`
(`R/abs_login.R`); one of them is missing.

**Fix.** Add both lines to `.Renviron` at the repo root, then
`source ./load-env.sh` and restart R.

**If you see:**

```
Login failed: no redirect in response.
```

**Cause.** The credentials were rejected or the session is stale — the portal
did not redirect back into the site after login.

**Fix.** Confirm the password was not rotated, then create a fresh session:

```
session <- abs_login()
```

**If you see:**

```
Not authenticated. Session may have expired. Please login again.
```

**Cause.** The session expired mid-run — the tests page redirected back to the
login page.

**Fix.** Re-login and re-download in one session (the download needs the
session object from the login):

```
session <- abs_login()
data <- download_abs_csv(session)
```

The maintained end-to-end login check is `make docker-test-auth`: it builds
the Docker image, logs in inside the container, and prints `Login OK`. Run it
when you suspect the credentials rather than the code. Full details on the
redirect detection and environment hygiene live in
[`../docs/okf/modules/abs_login.md`](../docs/okf/modules/abs_login.md).

## 5. Missing R packages (for example httr2)

**If you see:**

```
httr2 package is required. Please install it with: install.packages('httr2')
```

**Cause.** The project's packages have not been restored. All dependencies are
pinned in `renv.lock` and installed by `renv::restore()` (the Dockerfile runs
it during the image build). A fresh clone or a cleared cache has no packages.

**Fix.** From the repo root, restore the pinned packages:

```
# From the repo root, in a terminal:
Rscript -e 'renv::restore()'
```

or rebuild the Docker image, which runs the same restore step:
`make docker-build`. Do **not** hand-install packages one by one — that
bypasses the lockfile. `renv::snapshot()` is never a recovery command — it only *updates* the lockfile; see
entry 9.

## 6. Missing enrollment targets file

**If you see:**

```
Could not find enrollment_targets.csv file
```

**Cause.** `get_enrollment_targets()` in `R/run_initial_function.R` looks for
the bundled `inst/extdata/enrollment_targets.csv` (plus a couple of fallback
locations) and cannot find it — the package was installed before the file was
added, or the install is stale.

**Fix.** Reinstall the local package from source. The dashboard itself does
this inside the container (see `install.packages('/project', repos = NULL,
type = 'source', dependencies = FALSE)` in `build-dashboard.sh`); locally, the
simplest path is:

1. `make docker-build` — rebuild the image with the current source.
2. `make docker-render` — re-render the dashboard.

The file is present in the repo (`inst/extdata/enrollment_targets.csv`), so a
fresh install always ships it. Details in
[`../docs/okf/modules/run_initial_function.md`](../docs/okf/modules/run_initial_function.md).

## 7. Docker build or daily CI deploy fails

**If you see** (local build, from `build-dashboard.sh`):

```
ERROR: Dashboard was not created
```

**Cause.** The render step failed earlier in the same script — almost always a
missing environment variable (entries 2–4) or a broken renv library.

**Fix.**

1. `make docker-build` — build (or rebuild) the local image.
2. `bash debug-docker.sh` — inspect what is inside the image: the renv
   library, `.Rprofile`, and the `RENV_*` environment variables.
3. `make docker-render` — re-run the full render with the fixed image.

> **WARNING:** `bash build-dashboard.sh` (and therefore `make docker-render`) runs `rm -rf docs` before rendering — it deletes the **entire** `docs/` directory, including the OKF knowledge bundle in `docs/okf/` (`build-dashboard.sh`). Make sure anything you need from `docs/` is committed to git (or re-generable) before you run it.

**If you see** a failing run in the daily CI build: open the **Actions** tab
of the repo, click the failing "Build Dashboard Daily" run, and expand the
failing step. The `build-image` job logs in to GitHub Container Registry with
the repo's `secrets.GITHUB_TOKEN` (workflow, "Log in to GitHub Container
Registry" step) and pushes `ghcr.io/moodlab/abmdash`. The `build-dashboard`
job then runs the render with five secrets: `REDCAP_API_TOKEN`,
`GOOGLE_SERVICE_ACCOUNT_JSON`, `ABS_USERNAME`, `ABS_PASSWORD`,
`STATICRYPT_PASSWORD` — check they are all set under Settings → Secrets and
variables → Actions. To re-trigger a manual run, use *Run workflow* (the
workflow listens for `workflow_dispatch`).

## 8. staticrypt: missing password is not an error

**If you see** in the build output:

```
No encryption (set STATICRYPT_PASSWORD to encrypt)
```

**Cause.** Nothing is broken. The `STATICRYPT_PASSWORD` variable is empty, so
the dashboard was rendered **without** password protection. The Dockerfile
defaults it to `""`; the build only encrypts when it is non-empty.

**Fix.** If the dashboard should be password-protected, set
`STATICRYPT_PASSWORD` in `.Renviron` (local) or as a repository secret (CI),
then re-render with `make docker-render`.

## 9. renv restore fails or hangs

**If you see** `renv::restore()` fail (network errors, cache lock, missing
CRAN packages):

**Cause.** Package restore needs a working connection to CRAN and enough disk
space; the cache can also get into a bad state.

**Fix.** Re-run the restore from the repo root — it is idempotent:

```
Rscript -e 'renv::restore()'
```

**Direction of travel.** `renv::restore()` *re-installs what the lockfile
pins* — that is the recovery command. `renv::snapshot()` is **never** a
recovery command: it *writes* the current state into the lockfile. It is only
for updating the lockfile after deliberately adding or removing a dependency —
never to "recover" from a broken install, because it overwrites the good
lockfile with whatever is currently installed.

## Still stuck?

- The module notes under `../docs/okf/` document each area in depth — start
  at [`../docs/okf/index.md`](../docs/okf/index.md).
- The topic vignettes give deeper walk-throughs:
  [`redcap-troubleshooting.md`](redcap-troubleshooting.md),
  [`google-troubleshooting.md`](google-troubleshooting.md),
  [`abs-troubleshooting.md`](abs-troubleshooting.md),
  [`docker-troubleshooting.md`](docker-troubleshooting.md),
  [`ci-troubleshooting.md`](ci-troubleshooting.md).
- Run `make test` from the repo root — the test suite reports which modules
  behave correctly with your current environment.
