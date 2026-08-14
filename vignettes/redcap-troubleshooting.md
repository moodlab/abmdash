# Troubleshooting REDCap data access

## What this guide is for

The dashboard reads screening, eligibility, and enrollment numbers from REDCap
using the functions in `R/redcap_api.R`. When those numbers go missing (or the
dashboard fails to render), the problem is almost always one of three things:

1.  The `REDCAP_API_TOKEN` environment variable is missing or empty.
2.  The token points at the wrong project or lacks permission.
3.  A REDCap empty string (`""`) leaked into a numeric calculation — the
    "`""` vs `NA`" bug class.

Each section below follows the same pattern: **If you see** (the error) →
**Cause** (why it happens) → **Fix** (what to do). All code blocks are shown
for reference only and are never executed while you read this page.

If you are not sure which of these you are hitting, run the checks below first.

## Check your token before anything else

The dashboard reads the token from the `REDCAP_API_TOKEN` environment
variable. On a local machine, credentials live in the `.Renviron` file at the
repo root. Load them before running any R script:

```
# From the repo root, in a terminal:
#   source ./load-env.sh
# then in R:
Sys.getenv("REDCAP_API_TOKEN")
```

`source ./load-env.sh` reads `.Renviron` and exports every variable in it. If
it reports an error, the file is missing or unreadable (see the first row of
the table below).

## Common errors and fixes

| If you see | Cause | Fix |
|---|---|---|
| `REDCAP_API_TOKEN environment variable is not set or is empty` | The token is missing or blank in your environment | Add `REDCAP_API_TOKEN=<your-token>` to `.Renviron` (one line, no quotes needed), then run `source ./load-env.sh` and restart R |
| `Failed to parse REDCap response` or HTTP `401`/`403` | The token is wrong, was rotated, or belongs to a different REDCap project | Re-copy the token from the REDCap project page → *API* tab; confirm it is the token for the project that backs screening report 14081 and enrollment report 13387 |
| `Error in data.frame(..., check.rows = FALSE): row names contain missing values` | REDCap sends `""` (empty string), **not** `NA`, for unset fields. `as.numeric("")` returns `NA`, and an `NA` used as a row index inserts an all-`NA` row, which `data.frame()` then refuses to build | Parse each numeric field **once** with `suppressWarnings(as.numeric(...))` and guard on the *parsed* value — never on the raw string. Keep `USE.NAMES = FALSE` in `sapply()`. See the next section for the pattern (fixed in PR #39) |
| Screens or counts appear but are all `NA` / blank | The same `""` vs `NA` problem, but in a field that was not caught by the guard | Apply the parse-once pattern below to any field you convert with `as.numeric()` |

## The `""` vs `NA` bug class (PR #39)

REDCap returns an empty string, `""`, for a field a participant never filled
in. R treats that as *text*, not as a missing value:

```
# REDCap gives us the empty string, not NA
raw <- c("17", "", "22", "14")

# as.numeric("") silently becomes NA -- no warning unless you ask for it
suppressWarnings(as.numeric(""))   # NA

# BAD: guard on the RAW string, then convert -- NA leaks into the row index
# bad <- raw[!is.na(raw) & as.numeric(raw) >= 17]

# GOOD: parse once, then guard on the PARSED value
score <- suppressWarnings(as.numeric(raw))
eligible <- score[!is.na(score) & score >= 17]   # drop NA rows — never impute 0
```

The codebase **drops** `NA` rows with `!is.na(...)` — it never substitutes `0`.
Imputing 0 would push a blank answer into the PHQ ≥ 17 eligibility check and
could let someone who never answered look eligible.

Two rules that prevent this whole class of bug:

- **Parse once.** Convert a field with `suppressWarnings(as.numeric(...))`
  into a new vector, then test *that* vector. Never write
  `!is.na(x) & as.numeric(x) >= N` — the raw-string guard looks right but
  still lets an `NA` through when `x` is `""`.
- **Keep values out of names.** When using `sapply()`, pass
  `USE.NAMES = FALSE` so a value vector never becomes the row names of the
  result. Row names that contain `NA` are exactly what produces *"row names
  contain missing values"*.

This bug shipped once already (issue #38, fixed in PR #39). If you see the
row-names error, you have re-introduced the raw-string guard somewhere.

## Still stuck?

- Re-read the module notes in `../docs/okf/modules/redcap_api.md` — they
  document the `""` invariant and the parse-once rule in detail.
- Run `make test` from the repo root; the REDCap tests will tell you whether
  the token and the parsing behave as expected.
- If the error only appears inside the Docker build, see the Docker
  troubleshooting vignette — the container gets its token from the same
  environment variables, and a stale image can hide a `.Renviron` change.
