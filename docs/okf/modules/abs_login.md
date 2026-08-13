---
type: Module
title: ABS Portal Login + CSV Download
description: Effectful Livewire-authenticated httr2 session against the ABS admin portal, plus test-data CSV download.
resource: R/abs_login.R
tags: [r, abs, livewire, httr2, auth]
timestamp: 2026-08-13
pure: false
---

# Responsibility

What this module does: authenticates to the ABS (Attention Bias Study) admin
portal at `https://abs.la.utexas.edu` via its Livewire update endpoint, returns
a cookie-preserving `httr2` request, and downloads the complete test-data CSV
through the Livewire `downloadCompleteCsv` action.

What it does NOT do: it does not compute compliance (that is
[trad_compliance](trad_compliance.md)), does not touch REDCap or Google APIs,
and never stores credentials.

# Interface

All 5 exported functions (NAMESPACE):

- `abs_login(base_url = "https://abs.la.utexas.edu", login_path = "/admin/login", check_connection = TRUE)` — returns authenticated `httr2` request (session cookies preserved in a temp cookie jar)
- `test_abs_connection(base_url = "https://abs.la.utexas.edu", verbose = TRUE)` — logical reachability probe
- `verify_abs_login(session, test_path = "/admin")` — logical check that the session still authenticates
- `download_abs_csv(session, tests_path = "/admin/tests", save_path = NULL, ...)` — data.frame of the full test-data CSV
- `preview_abs_csv(session, n = 6)` — prints head, invisibly returns the full data.frame

Internal helpers: `extract_livewire_snapshot(html)`, `extract_csrf_token(html)`.

# Dependencies

- [trad_compliance](trad_compliance.md) — consumes `abs_login()` + `download_abs_csv()`
- R packages: `httr2` (HTTP + cookie jar), `jsonlite` (Livewire payload/response), `utils` (CSV)

# Invariants

- `ABS_USERNAME` and `ABS_PASSWORD` env vars are required; both are `trimws()`ed and stripped of surrounding quotes (secrets-manager artifact hygiene).
- SSL verification disabled (`ssl_verifypeer = 0`, `ssl_verifyhost = 0`) and `http_version = 2` forced — the ABS server sends malformed HTTP/2 headers.
- Login success is detected by the presence of a Livewire `effects.redirect`; failure raises a diagnostic error (HTTP status, component effects, HTML text) for the dashboard.
- `download_abs_csv()` raises "Not authenticated" when the tests page redirects to the login page.

# Examples

```r
session <- abs_login()
if (verify_abs_login(session)) {
  data <- download_abs_csv(session)
}
preview_abs_csv(session, n = 6)
```
