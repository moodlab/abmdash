# Content-grep guard for vignettes/faq.Rmd
#
# The FAQ vignette uses eval=FALSE chunks: R CMD check builds the page without
# ever executing the prose. A fabricated or paraphrased error string would
# therefore still produce a green build — this file is the load-bearing
# content gate. It locks four invariants:
#
#   (a) every quoted error string in the FAQ exists verbatim in the repo
#       source (R/, Makefile, Dockerfile, build-dashboard.sh, workflow);
#   (b) every fix command the FAQ names exists in this repo's
#       Makefile/Dockerfile — no generic internet advice;
#   (c) the FAQ never references a `scripts/` directory (the helper scripts
#       build-dashboard.sh, debug-docker.sh, load-env.sh live at repo ROOT);
#   (d) the REDCap "" vs NA entry carries all required elements (exact error
#       string, fix pattern, both manifestation sites, PR #39, OKF link).
#
# If you edit the FAQ: update the strings below to match. If you remove an
# error entry from the FAQ, remove its checks here too.

library(testthat)

# Repo-root-relative file reader (this file lives at tests/testthat/).
repo_read <- function(rel_path) {
  paste(readLines(test_path("..", "..", rel_path), warn = FALSE), collapse = "\n")
}

# Concatenate every R/ source file — the "rg -F across R/" equivalent.
r_source <- paste(vapply(
  list.files(test_path("..", "..", "R"), pattern = "\\.R$", full.names = TRUE),
  function(f) paste(readLines(f, warn = FALSE), collapse = "\n"),
  character(1)
), collapse = "\n")

makefile <- repo_read("Makefile")
dockerfile <- repo_read("Dockerfile")
build_sh <- repo_read("build-dashboard.sh")
workflow <- repo_read(".github/workflows/build-dashboard.yml")
faq <- repo_read("vignettes/faq.Rmd")

# Searchable corpus, mirroring the spec's rg -F sweep targets.
corpus <- paste(r_source, makefile, dockerfile, build_sh, workflow,
                collapse = "\n")

expect_verbatim <- function(string, haystack = corpus) {
  expect_true(
    grepl(string, haystack, fixed = TRUE),
    label = sprintf("verbatim string not found in repo source: %s", string)
  )
}

expect_verbatim_faq <- function(string) {
  expect_verbatim(string, haystack = faq)
}

test_that("every quoted error string in the FAQ exists verbatim in the repo", {
  # Entry 1: REDCap "" vs NA bug class (R/redcap_api.R)
  expect_verbatim("row names contain missing values")
  expect_verbatim("REDCAP_API_TOKEN environment variable is not set or is empty")
  # Entry 2: Google service account (R/gsheet_api.R, R/gcal_api.R)
  expect_verbatim("GOOGLE_SERVICE_ACCOUNT_JSON environment variable is not set or is empty")
  expect_verbatim("Failed to parse GOOGLE_SERVICE_ACCOUNT_JSON:")
  # Entry 3: ABS login (R/abs_login.R)
  expect_verbatim("ABS_USERNAME environment variable is not set")
  expect_verbatim("ABS_PASSWORD environment variable is not set")
  expect_verbatim("Login failed: no redirect in response.")
  expect_verbatim("Not authenticated. Session may have expired. Please login again.")
  # Entry 4: missing packages (R/abs_login.R, R/gcal_api.R, R/gsheet_api.R, R/redcap_api.R)
  expect_verbatim("httr2 package is required. Please install it with: install.packages")
  # Entry 5: enrollment targets CSV (R/run_initial_function.R)
  expect_verbatim("Could not find enrollment_targets.csv file")
  # Entry 6: local build failure symptom (build-dashboard.sh)
  expect_verbatim("Dashboard was not created")
  # Entry 7: staticrypt no-encryption notice (build-dashboard.sh)
  expect_verbatim("No encryption (set STATICRYPT_PASSWORD to encrypt)")
  # Entry 8: renv restore (Dockerfile)
  expect_verbatim("renv::restore()")
})

test_that("every fix command in the FAQ exists in this repo", {
  # renv::restore() is the package-recovery path baked into the Dockerfile
  expect_true(
    grepl("renv::restore()", dockerfile, fixed = TRUE),
    label = "renv::restore() must exist in Dockerfile"
  )
  # Makefile targets the FAQ points users at
  for (target in c("docker-build", "docker-render", "docker-test-auth",
                   "test:", "test-trad:", "lint:")) {
    expect_true(
      grepl(target, makefile, fixed = TRUE),
      label = sprintf("Makefile target missing: %s", target)
    )
  }
  # staticrypt is installed in the Dockerfile and invoked by the workflow
  expect_true(grepl("staticrypt", dockerfile, fixed = TRUE))
  expect_true(grepl("staticrypt", workflow, fixed = TRUE))
})

test_that("no scripts/ path prefix in the FAQ", {
  expect_false(
    grepl("scripts/", faq, fixed = TRUE),
    label = paste(
      "FAQ must reference root-level scripts (build-dashboard.sh,",
      "debug-docker.sh, load-env.sh) — there is no scripts/ directory"
    )
  )
})

test_that("REDCap entry is complete (exact error, pattern, sites, PR #39, OKF)", {
  expect_verbatim_faq("row names contain missing values")
  expect_verbatim_faq("suppressWarnings(as.numeric(")
  expect_verbatim_faq("USE.NAMES")
  expect_verbatim_faq("get_eligible_participants")
  expect_verbatim_faq("get_weekly_screening_stats")
  expect_verbatim_faq("#39")
  expect_verbatim_faq("docs/okf/modules/redcap_api.md")
})

test_that("renv guidance is correct direction (restore to recover; snapshot never)", {
  expect_verbatim_faq("renv::restore()")
  snapshot_lines <- grep("snapshot", strsplit(faq, "\n")[[1]], value = TRUE)
  expect_true(
    length(snapshot_lines) >= 1,
    label = "FAQ should mention renv::snapshot() (to say when to use it)"
  )
  expect_true(
    all(grepl("never", snapshot_lines, fixed = TRUE)),
    label = paste(
      "any line mentioning renv::snapshot() must also say 'never':",
      "snapshot() updates the lockfile, it is never a recovery step"
    )
  )
})

test_that("destructive commands appear only in WARNING context", {
  faq_lines <- strsplit(faq, "\n")[[1]]
  destructive <- grep("rm -rf|prune|docker rm", faq_lines)
  expect_true(
    length(destructive) >= 1,
    label = paste(
      "FAQ should warn about the build-dashboard.sh 'rm -rf docs'",
      "clobber hazard (it wipes docs/ including docs/okf/)"
    )
  )
  expect_true(
    all(grepl("WARNING", faq_lines[destructive], fixed = TRUE)),
    label = "every rm -rf / prune / docker rm line must carry WARNING context"
  )
})

test_that("FAQ cross-links OKF module docs for module-level entries", {
  expect_verbatim_faq("docs/okf/modules/redcap_api.md")
  expect_verbatim_faq("docs/okf/modules/gsheet_api.md")
  expect_verbatim_faq("docs/okf/modules/gcal_api.md")
  expect_verbatim_faq("docs/okf/modules/abs_login.md")
  expect_verbatim_faq("docs/okf/modules/run_initial_function.md")
})
