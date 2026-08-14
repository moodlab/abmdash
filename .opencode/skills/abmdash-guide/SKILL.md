---
name: abmdash-guide
description: >-
  Learn-the-repo walkthrough for the abmdash R package — the private,
  staticrypt-encrypted Quarto dashboard for the ABM study, deployed to GitHub
  Pages. Explains repo purpose, the render-and-encrypt pipeline (R package →
  quarto → staticrypt → docs/ commit), CI jobs, the 10 R module map, data
  sources (REDCap, Google Sheets, Google Calendar, ABS portal), local run via
  the Makefile, and the behavior-lock test suite as executable specs. Use when
  someone asks to learn abmdash, understand how this repo works, what modules
  exist, or how the dashboard architecture is put together. Also for
  troubleshooting: debug, diagnose, probe, or investigate errors, broken
  tests, failing CI, or reported symptoms — see the Debug workflows section.
---

# abmdash — Learn the Repo

A single-file orientation for anyone (agent or human) new to this repository.
Read top to bottom once; then use the module map as a lookup table. For
troubleshooting (not covered here) see the [troubleshooting vignettes](vignettes/).

## 1. Repo Purpose

`abmdash` is an R package (see [DESCRIPTION](DESCRIPTION)) that builds the ABM
project dashboard: a **private** summary of study data (screening, enrollment,
compliance, follow-ups) for the Attention Bias Modification study. The output is
a static Quarto dashboard, encrypted at rest with staticrypt, and published to
GitHub Pages. The repo is both the data-access layer and the publishing
pipeline — there is no running server.

Three facts shape every decision in this codebase:

- **The dashboard must stay private.** Every build step exists to keep the
  rendered HTML encrypted (staticrypt) and the credentials out of the repo.
- **It is read-mostly glue.** The R package is a thin effectful shell over
  external APIs (REDCap, Google Sheets, Google Calendar, the ABS portal) that
  transforms data into data frames for the dashboard pages.
- **Behavior is locked, not just tested.** The test suite replays recorded API
  traffic offline and pins output snapshots — the tests ARE the executable
  spec (see [§6](#6-behavior-lock-suite-as-executable-specs)).

## 2. Architecture

A daily pipeline turns API data into an encrypted, published dashboard:

```
R package (R/*.R)
  → Quarto render (inst/dashboard) — pages written from package functions
  → staticrypt encrypt (build-dashboard.sh) — password-protect the HTML
  → commit rendered docs/ → GitHub Pages
```

Key moving parts:

- **CI workflow** [.github/workflows/build-dashboard.yml](.github/workflows/build-dashboard.yml):
  - `build-image` job — builds the Docker image (2-stage
    `rocker/r-ver:4.5.1`, node 18 + staticrypt installed in stage 2; see
    [Dockerfile](Dockerfile)).
  - `build-dashboard` job (needs `build-image`) — runs the render +
    encrypt + commit step via [build-dashboard.sh](build-dashboard.sh).
  - Scheduled by **cron `0 6 * * *`** (daily at 6 AM UTC).
- **Dockerfile** — two stages, both `FROM rocker/r-ver:4.5.1`; stage 2 adds
  Node.js 18 and `staticrypt` via npm.
- **Output contract** — encrypted dashboard lands in `docs/` (`index.html` +
  `site_libs/` + `.staticrypt.json`) and is served by GitHub Pages. `docs/` is
  gitignored-then-committed by the pipeline (see [build-dashboard.sh](build-dashboard.sh)).
- **Tests** — a separate workflow [.github/workflows/test.yml](.github/workflows/test.yml)
  runs the behavior-lock suite on every push (pins R 4.5.1, matching the
  snapshot R version).

Source of truth for the pipeline details: [Dockerfile](Dockerfile),
[build-dashboard.sh](build-dashboard.sh), and
[.github/workflows/build-dashboard.yml](.github/workflows/build-dashboard.yml).

## 3. Module Map

10 R source files under `R/`. Each module is one file; the file boundary IS
the module boundary (see the OKF bundle at [docs/okf/index.md](docs/okf/index.md)
for concept docs — note the bundle lags the source, see the accuracy note below).

| Module | Responsibility | Data source |
|---|---|---|
| [R/redcap_api.R](R/redcap_api.R) | REDCap API shell: records, metadata, survey completions, logs, reports; derives eligibility, weekly-screening, and enrollment stats | REDCap API |
| [R/gsheet_api.R](R/gsheet_api.R) | Read a Google Sheet (OAuth2/service account) into a data frame | Google Sheets |
| [R/gcal_api.R](R/gcal_api.R) | Google Calendar events via service-account auth | Google Calendar |
| [R/abs_login.R](R/abs_login.R) | ABS admin portal login (Livewire) + CSV download of session data | ABS portal |
| [R/compliance_summary.R](R/compliance_summary.R) | Participant progress view: current week, sessions completed/behind | Google Sheets |
| [R/compliance_tracking.R](R/compliance_tracking.R) | Expected vs actual sessions per week from gameplay data | Google Sheets |
| [R/trad_compliance.R](R/trad_compliance.R) | Traditional ABM compliance from ABS-downloaded session data (mirrors GABM summary shape) | ABS portal CSV |
| [R/demographics.R](R/demographics.R) | Demographic summary for enrolled participants | REDCap (report 13349) |
| [R/week12_tracking.R](R/week12_tracking.R) | Upcoming Week 12/16/28 follow-up appointments from W4 Acute in-person completions | REDCap |
| [R/run_initial_function.R](R/run_initial_function.R) | Starter/example export (`n + n`); kept as the canonical minimal function | — |

**Accuracy notes (the OKF bundle is stale post-refactor):**

- `R/redcap_api.R` has **16 internal helpers** (25 top-level functions total,
  9 of them exported) — not the two-helper count claimed in
  [docs/okf/modules/redcap_api.md](docs/okf/modules/redcap_api.md). When you
  need the real list, read [R/redcap_api.R](R/redcap_api.R) itself — it is the
  source of truth.
- There are **10 R files**, not nine: the OKF index omits
  [R/week12_tracking.R](R/week12_tracking.R) (1 exported function, so the
  bundle's boundary rule skipped it — it is still a real module).
- **33 exports** total, as counted in [NAMESPACE](NAMESPACE). When in doubt,
  trust `NAMESPACE`.

**Habit to form:** the OKF bundle is a curated *map*, not the terrain. For any
accuracy-sensitive question, verify against `R/*.R`, `NAMESPACE`, and
`DESCRIPTION` before trusting a concept doc.

## 4. Data Sources

Four external systems feed the dashboard; every one is accessed through a
dedicated module (no cross-wiring):

1. **REDCap** ([R/redcap_api.R](R/redcap_api.R)) — screening, eligibility,
   enrollment, demographics, follow-up scheduling. REST API at
   `redcap.prc.utexas.edu`, token from the `REDCAP_API_TOKEN` env var.
2. **Google Sheets** ([R/gsheet_api.R](R/gsheet_api.R)) — gameplay session
   data used by compliance modules. Auth via service account
   (`GOOGLE_SERVICE_ACCOUNT_JSON`).
3. **Google Calendar** ([R/gcal_api.R](R/gcal_api.R)) — study events.
   Service-account auth (`GOOGLE_SERVICE_ACCOUNT_JSON`).
4. **ABS portal** ([R/abs_login.R](R/abs_login.R)) — the Attention Bias Study
   admin site; username/password login (`ABS_USERNAME`/`ABS_PASSWORD`) plus a
   CSV download consumed by [R/trad_compliance.R](R/trad_compliance.R).

The staticrypt password for the rendered dashboard comes from
`STATICRYPT_PASSWORD` ([R/run_initial_function.R](R/run_initial_function.R)
shows the encryption call).

## 5. Local Run

You need credentials in `.Renviron` (gitignored — never committed) to reach
real data: `REDCAP_API_TOKEN`, `GOOGLE_SERVICE_ACCOUNT_JSON`,
`ABS_USERNAME`, `ABS_PASSWORD`, `STATICRYPT_PASSWORD`. Without them, the
behavior-lock suite still runs fully offline (recorded fixtures, see §6).

The [Makefile](Makefile) defines the 7 standard targets:

| Target | What it does |
|---|---|
| `make test` | Run the full test suite (`devtools::test()`) |
| `make test-trad` | Run only the trad-compliance tests |
| `make docker-build` | Build the Docker image locally (same as the CI build-image job) |
| `make docker-render` | Full local render + encrypt (same as the CI build-dashboard job) |
| `make docker-test-auth` | Test ABS portal auth inside Docker (needs `.Renviron`) |
| `make lint` | R package checks (`devtools::check()`) |
| `make serve` | Serve the rendered `docs/` dashboard locally on port 8000 |

Quickstart: `make test` to verify the environment, then `make serve` and open
<http://localhost:8000> (the built dashboard must already exist in `docs/`).

## 6. Behavior-Lock Suite as Executable Specs

The tests are not just regression checks — they are the executable
specification of this package. Every API module is pinned by recorded HTTP
fixtures replayed offline, plus committed RDS snapshots compared with waldo.

Entry points:

- [tests/testthat/test-redcap-behavior-lock.R](tests/testthat/test-redcap-behavior-lock.R) —
  the canonical example: offline replay of recorded REDCap traffic, including
  empty-result and error branches, with no-token guards on every mock file.
- [tests/testthat/test-snapshot-lock.R](tests/testthat/test-snapshot-lock.R) —
  snapshot-locking harness (`expect_snapshot_locked()`), pinned RDS files under
  `tests/testthat/_snaps/snapshot-lock/`.
- [tests/testthat/helper-harness.R](tests/testthat/helper-harness.R) —
  fixture loading (`load_fixture()`) and isolated-env helpers used by all tests.

The operational rules for recording, redaction, and committing fixtures live in
**[RECORDING.md](RECORDING.md) at the repo root** (fixture namespacing,
credential redaction, the no-token commit rule, the `""` vs `NA` convention,
row-order policy, snapshot version pinning). If you change behavior, you change
the tests that pin it — the tests ARE the spec.

## 7. Verify Your Understanding

You have the right mental model if you can answer all of these from memory (or
know exactly which file to open):

1. What does this repo build, and why is the output encrypted?
2. What are the two CI jobs in
   [.github/workflows/build-dashboard.yml](.github/workflows/build-dashboard.yml),
   and when do they run?
3. Name the 10 modules and which external system each one talks to.
4. Where do REDCap, Google Sheets, Google Calendar, and the ABS portal each
   come into the pipeline?
5. How do you run the full test suite locally, and why does it work without
   any credentials?
6. Where does the behavior-lock recording/redaction documentation live?

If you cannot answer #3 and #6 confidently, re-read §3 and §6 before touching
code.

## 8. Non-Engineer Plain-Language Guide

**What is this?** A website that shows how the ABM study is going — how many
people have signed up, completed screening, stayed on track with their weekly
sessions, and who has follow-up appointments coming up. The site is private:
anyone who opens it needs a password, and the password-protection is applied
automatically every time the site is rebuilt.

**Where does the data come from?** Four places, all read automatically (nobody
types numbers in by hand):

- **REDCap** (Research Electronic Data Capture — the study's main participant
  database) — records participants and their progress.
- **Google Sheets** — where weekly gameplay session data is logged.
- **Google Calendar** — study events.
- **The ABS portal** — an administrative website that records traditional ABM
  sessions.

**How does it get built?** Once a day, a computer (GitHub's automated
pipeline, running in a Docker container) logs into these systems, pulls the
latest data, generates the website pages, locks them with a password, and
publishes the result to the web. You don't need to run anything yourself to
see the current site.

**How do I run it myself?** If you have the project on your computer and have
been given the credentials file (`.Renviron`), type `make test` to check
everything works, then `make serve` to view the site locally in your browser
at <http://localhost:8000>. The [Makefile](Makefile) has seven commands, all
starting with `make`, that cover testing, building, and serving — you will
only ever need `make test` and `make serve` for day-to-day use.

**What should I never do?** Never put real passwords or API tokens in a file
that gets committed to the repository. The project has automated checks
([RECORDING.md](RECORDING.md)) that refuse to merge anything containing a
token-shaped string — keep credentials in `.Renviron`, which is never
committed.

## 9. Debug Workflows

Symptom → probe → likely-file diagnostics for the most common failures in
this repo. **Every probe here is offline**: it is an `Rscript
-e 'devtools::test(filter="...")'` run, a `make test-trad`, an `rg` over
source, or a file existence check. Nothing below talks to a live API,
needs credentials, or needs the network.

### Probing rules — read this first

- **Never run `make test filter=X`.** The Makefile `test` target is a bare
  `devtools::test()` — it never reads `filter`, so `make test filter=X`
  silently runs the **full suite**. Use
  `Rscript -e 'devtools::test(filter="<name>")'` instead. `make test-trad`
  is the **only** filtered make target (it wraps
  `devtools::test(filter = "trad-compliance")`).
- **Filters are substrings of test-file names — use full strings.**
  `filter="compliance"` matches **three** files (`test-compliance-summary.R`,
  `test-compliance-tracking.R`, `test-trad-compliance.R`) — over-match. Use
  `filter="compliance-summary"` to pin one module.
- **Every filter below matches a real `tests/testthat/` file** — if a filter
  runs zero tests, the name is wrong, not the environment.

Behavior-lock suites are executable docs:
`Rscript -e 'devtools::test(filter="<name>")'`. Green = behavior LOCKED —
bug is upstream (credentials, data shape, environment). Red = behavior
CHANGED in code. Never use `make test filter=X` — Makefile test ignores
filter, silently runs the full suite; `make test-trad` is the only filtered
make target. Filters are substrings of test-file names — use full strings.

> **⚠ Requires credentials AND network — do NOT use as probes.** `make
> docker-test-auth` (needs `.Renviron` with `ABS_USERNAME`/`ABS_PASSWORD`,
> plus network to the ABS portal) and `make docker-render` (needs all
> credentials plus network for `renv::restore()` and quarto) verify real
> integrations. `make docker-build` needs network (pulls base images) but no
> credentials. These three are *verification* targets, never *diagnostic*
> probes — if you need to debug, run the offline probes in the table below
> first.

### Symptom → probe → likely file

| Symptom (verbatim error fragment) | Probe (offline) | Likely file |
|---|---|---|
| `row names contain missing values` | `Rscript -e 'devtools::test(filter="redcap-behavior-lock")'` | [R/redcap_api.R](R/redcap_api.R):471 (`get_eligible_participants`), :615 (`eligibility_mask`), :657 (`get_weekly_screening_stats`) |
| `Failed to parse private key PEM` (openssl runtime string — NOT a `stop()` in source, so `rg` returns zero) | `Rscript -e 'devtools::test(filter="google-token-signing")'` | [R/gsheet_api.R](R/gsheet_api.R):267, [R/gcal_api.R](R/gcal_api.R):160 (`openssl::read_key`) |
| `GOOGLE_SERVICE_ACCOUNT_JSON environment variable is not set or is empty` | `Rscript -e 'devtools::test(filter="gsheet-api")'` | [R/gsheet_api.R](R/gsheet_api.R):208 |
| GCal event errors (token, parse, event shape) | `Rscript -e 'devtools::test(filter="gcal-api")'` | [R/gcal_api.R](R/gcal_api.R) |
| REDCap fetch/parse errors | `Rscript -e 'devtools::test(filter="redcap-behavior-lock")'` | [R/redcap_api.R](R/redcap_api.R) |
| Eligibility regressions | `Rscript -e 'devtools::test(filter="eligible-participants")'` | [R/redcap_api.R](R/redcap_api.R):471 |
| Snapshot drift (pinned RDS changed) | `Rscript -e 'devtools::test(filter="snapshot-lock")'` | `tests/testthat/_snaps/snapshot-lock/` |
| FAQ-quoted error string suspect | `Rscript -e 'devtools::test(filter="faq-verbatim")'` | [tests/testthat/test-faq-verbatim.R](tests/testthat/test-faq-verbatim.R) |
| Trad-compliance regression | `make test-trad` | [R/trad_compliance.R](R/trad_compliance.R) |
| ABS offline (login/download behavior) | `Rscript -e 'devtools::test(filter="abs-login")'` — green **with 1 skip** is the expected offline result | [tests/testthat/test-abs-login.R](tests/testthat/test-abs-login.R):519 (`skip_if_not`) |
| CI `build-image` job red | inspect [Dockerfile](Dockerfile) + [renv.lock](renv.lock) — renv/base-image/network (CI cache is GHA's, not local `/tmp/docker-cache`) | [Dockerfile](Dockerfile), [renv.lock](renv.lock) |
| CI `build-dashboard` job red | inspect secrets/render/rebase — [build-dashboard.sh](build-dashboard.sh) + [.github/workflows/build-dashboard.yml](.github/workflows/build-dashboard.yml) | [build-dashboard.sh](build-dashboard.sh), workflow |
| `renv::restore()` fails/hangs | `rg "renv" Dockerfile renv.lock` | [Dockerfile](Dockerfile), [renv.lock](renv.lock) |
| staticrypt error/absent password | `rg "staticrypt" build-dashboard.sh .github/workflows/build-dashboard.yml` | [build-dashboard.sh](build-dashboard.sh), workflow |
| Fixture-scan violation (token-shaped string in fixtures) | `Rscript -e 'devtools::test(filter="fixture-scan-google")'` | [tests/testthat/test-fixture-scan-google.R](tests/testthat/test-fixture-scan-google.R) |
| Calendar event order wrong | `Rscript -e 'devtools::test(filter="combined-calendar")'` | [R/gcal_api.R](R/gcal_api.R) |
| Missing R package (`httr2`/`jsonlite`/`jose` — `... package is required`) | `rg "package is required" R/` | [R/gsheet_api.R](R/gsheet_api.R):37/41/202, [R/gcal_api.R](R/gcal_api.R):32/36/121/208/212, [R/abs_login.R](R/abs_login.R):24, [R/redcap_api.R](R/redcap_api.R):40 |
| `Could not find enrollment_targets.csv file` | `rg "enrollment_targets" R/run_initial_function.R` | [R/run_initial_function.R](R/run_initial_function.R):90-104, [inst/extdata/enrollment_targets.csv](inst/extdata/enrollment_targets.csv) |

### CI job distinction

The two CI jobs fail for **different** reasons — do not conflate them:

- **`build-image`** (first job): builds the Docker image. Reds come from
  `renv::restore()` (lockfile drift, missing package, network), the base
  image, or Docker layer cache. CI uses **GitHub Actions cache**, not the
  local `/tmp/docker-cache` that `make docker-build` uses.
- **`build-dashboard`** (needs `build-image`): render + encrypt + commit.
  Reds come from secrets (missing/wrong env vars), the render step, or the
  `git pull --rebase` in [build-dashboard.sh](build-dashboard.sh).

### abs-login offline skip disclosure

`tests/testthat/test-abs-login.R:519` has
`skip_if_not(Sys.getenv("ABS_USERNAME") != "" && Sys.getenv("ABS_PASSWORD") != "")`.
Offline (no `.Renviron` credentials) **"green with 1 skip" is the expected
result** — it is NOT a failure and NOT "all pass". The skip is the single
live-login test (line 519); the other 37 tests in the file still run and
pin the recorded-fixture behavior.
