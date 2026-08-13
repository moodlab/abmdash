# Behavior-lock pilot: process_trad_compliance_data (AC-3.1)
#
# This file is named test-snapshot-lock.R (not test-harness-snapshot-lock-trad.R)
# so that testthat's snapshot machinery writes the lock artifact to
# _snaps/snapshot-lock/trad.rds — the exact path AC-8 namespaces. Parallel
# modules will add test-snapshot-lock-<module>.R files; each artifact lands in
# _snaps/snapshot-lock/ without collisions.
#
# Replaces the vacuous coverage in test-trad-compliance.R: with a real clock
# the trad fixture (2026-03-01..17) is older than active_window_weeks = 5 and
# every participant is filtered out, so the old tests passed on an empty
# data.frame. This test freezes the clock and locks the FULL 8-column output.
#
# Frozen clock: 2026-03-20 12:00:00 UTC
#   P001 first session 2026-03-01 10:00 UTC
#     -> difftime = 19 d 2 h = 19.083333 d = 2.726190 wk -> round(, 2) = 2.73
#     -> current_week = ceiling(2.726190) = 3
#     -> expected_sessions = round(2.726190 * 4) = 11 (<= 16)
#     -> sessions_behind = 11 - 5 = 6 -> "Behind by 6"
#   P002 first session 2026-03-15 10:00 UTC
#     -> difftime = 5 d 2 h = 5.083333 d = 0.726190 wk -> round(, 2) = 0.73
#     -> current_week = ceiling(0.726190) = 1
#     -> expected_sessions = round(0.726190 * 4) = 3
#     -> sessions_behind = 3 - 2 = 1 -> "Behind by 1"
#   P003 first session 2025-01-01 -> ~63 wk > active_window_weeks = 5 -> excluded
#   test_user rows filtered by exclude_pattern "test" before aggregation.
#
# The exact-value asserts are the mock-propagation TRIPWIRE: if the clock mock
# does not propagate to Sys.time() inside process_trad_compliance_data, the
# real clock (2026-08+) reports ~23 weeks for P001 and every assert above
# fails loudly instead of passing vacuous.

test_that("process_trad_compliance_data is snapshot-locked at frozen clock", {
  local_isolated_env()
  with_fixed_clock("2026-03-20 12:00:00", tz = "UTC")

  trad_data <- load_fixture("trad", "trad_csv_sample.csv")
  result <- process_trad_compliance_data(trad_data)

  # Non-vacuous shape: 2 active participants, old participant excluded
  expect_equal(nrow(result), 2)
  expect_true(all(c("P001", "P002") %in% result$id))
  expect_false("P003" %in% result$id)
  expect_false(any(grepl("test", result$id, ignore.case = TRUE)))

  # Exact derived values from the frozen clock (mock-propagation tripwire)
  p001 <- result[result$id == "P001", ]
  p002 <- result[result$id == "P002", ]
  expect_equal(p001$weeks_from_start, 2.73)
  expect_equal(p002$weeks_from_start, 0.73)
  expect_equal(p001$current_week, 3)
  expect_equal(p002$current_week, 1)
  expect_equal(p001$total_sessions, 5)
  expect_equal(p002$total_sessions, 2)
  expect_equal(p001$expected_sessions, 11)
  expect_equal(p002$expected_sessions, 3)
  expect_equal(p001$sessions_behind, 6)
  expect_equal(p002$sessions_behind, 1)
  expect_equal(p001$status, "Behind by 6")
  expect_equal(p002$status, "Behind by 1")
  expect_equal(p001$start_date, "2026-03-01")
  expect_equal(p002$start_date, "2026-03-15")

  # Full 8-column snapshot lock (id, current_week, weeks_from_start,
  # total_sessions, expected_sessions, sessions_behind, start_date, status)
  expect_named(
    result,
    c("id", "current_week", "weeks_from_start", "total_sessions",
      "expected_sessions", "sessions_behind", "start_date", "status")
  )
  expect_snapshot_locked("trad", result)
})
