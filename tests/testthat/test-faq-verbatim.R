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
#       string, fix pattern, both manifestation sites, PR #39, OKF link);
#   (e) every ```-fenced error block in the FAQ exists verbatim in the repo
#       source (the FAQ -> corpus direction — a paraphrased or fabricated
#       error string cannot slip past the corpus-side checks in (a));
#   (f) every knitr chunk in the FAQ is eval=FALSE, so no error string is
#       ever executed at build time.
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

# Reduce a fenced error block to its core error substring: trim whitespace and
# strip leading decoration ("❌ ", "⚠️ ", "> ") and severity labels
# ("ERROR: ", "WARNING: ") so a prefixed FAQ block still matches the plain
# error string in the corpus. Never reduces to the empty string.
core_error_substring <- function(block) {
  s <- trimws(block)
  repeat {
    prev <- s
    stripped <- sub("^[^[:alnum:]()]+\\s*", "", s)
    if (!nzchar(stripped)) break
    s <- sub("^(ERROR|WARNING|WARN|FATAL|INFO|NOTE)[:：]\\s*", "", stripped,
             ignore.case = TRUE)
    if (identical(s, prev)) break
  }
  s
}

# An OKF module link in the FAQ must (a) carry the exact link text and (b)
# point at a file that actually exists on disk (the FAQ lives in vignettes/,
# so its "../docs/..." links resolve to the repo root).
expect_okf_link <- function(link) {
  expect_verbatim_faq(link)
  expect_true(
    file.exists(test_path("..", "..", link)),
    label = sprintf("OKF module doc linked by the FAQ must exist on disk: %s", link)
  )
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
  expect_verbatim("httr2 package is required. Please install it with: install.packages('httr2')")
  # Entry 5: enrollment targets CSV (R/run_initial_function.R)
  expect_verbatim("Could not find enrollment_targets.csv file")
  # Entry 6: local build failure symptom (build-dashboard.sh)
  expect_verbatim("Dashboard was not created")
  # Entry 7: staticrypt no-encryption notice (build-dashboard.sh)
  expect_verbatim("No encryption (set STATICRYPT_PASSWORD to encrypt)")
  # Entry 8: renv restore (Dockerfile)
  expect_verbatim("renv::restore()")
})

test_that("every fenced error block in the FAQ exists verbatim in the repo", {
  # FAQ -> corpus direction: extract every ```-fenced block from the FAQ and
  # assert its core error substring appears in the repo source. This closes
  # the asymmetry of the corpus-side checks above — a paraphrased or
  # fabricated error string cannot pass CI. ```{r ...} chunk fences are
  # excluded: their interiors are code (e.g. the google-env JSON example is
  # not an error string and is not in the corpus).
  faq_lines <- strsplit(faq, "\n")[[1]]
  blocks <- character(0)
  in_block <- FALSE
  code_chunk <- FALSE
  block_lines <- character(0)
  for (ln in faq_lines) {
    if (grepl("^```", ln)) {
      if (in_block) {
        if (!code_chunk && length(block_lines) > 0) {
          blocks <- c(blocks, paste(block_lines, collapse = "\n"))
        }
        in_block <- FALSE
      } else {
        in_block <- TRUE
        code_chunk <- grepl("^```\\{", ln)
        block_lines <- character(0)
      }
    } else if (in_block) {
      block_lines <- c(block_lines, ln)
    }
  }
  # Floor guard so an empty/regressed extraction cannot vacuously pass.
  expect_true(
    length(blocks) >= 8,
    label = "FAQ should contain at least 8 fenced error blocks"
  )
  for (block in blocks) {
    core <- core_error_substring(block)
    expect_true(
      nzchar(core) && grepl(core, corpus, fixed = TRUE),
      label = sprintf(
        "fenced error block in FAQ not found verbatim in repo source:\n%s\ncore: %s",
        block, core
      )
    )
  }
})

test_that("every knitr chunk in the FAQ is eval=FALSE", {
  # eval=TRUE chunks would execute quoted error strings at build time; the
  # FAQ's error blocks are prose for humans, never code.
  chunk_lines <- grep("^```\\{", strsplit(faq, "\n")[[1]], value = TRUE)
  expect_true(
    length(chunk_lines) >= 1,
    label = "FAQ should contain at least one knitr chunk"
  )
  expect_true(
    all(grepl("eval=FALSE", chunk_lines, fixed = TRUE)),
    label = paste(
      "every knitr chunk in vignettes/faq.Rmd must set eval=FALSE:",
      paste(chunk_lines, collapse = " | ")
    )
  )
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
  expect_okf_link("docs/okf/modules/redcap_api.md")
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
  expect_okf_link("docs/okf/modules/redcap_api.md")
  expect_okf_link("docs/okf/modules/gsheet_api.md")
  expect_okf_link("docs/okf/modules/gcal_api.md")
  expect_okf_link("docs/okf/modules/abs_login.md")
  expect_okf_link("docs/okf/modules/run_initial_function.md")
})

test_that("README points at the FAQ vignette", {
  readme <- repo_read("README.md")
  expect_true(
    grepl("vignettes/faq.Rmd", readme, fixed = TRUE),
    label = "README should link the FAQ vignette (vignettes/faq.Rmd)"
  )
})
