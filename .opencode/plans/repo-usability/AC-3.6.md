---
ac: 3.6
depends_on: AC-3.3
risk: medium
status: spec
---

# AC-3.6: Refactor gsheet_api.R + gcal_api.R (ZERO behavior change)

## Executable Spec
- **predicate (12 conjuncts):**
  1. NAMESPACE byte-identical (7 exports).
  2. Full lock suite green (test-gsheet-api 31, test-combined-calendar 12 UNSORTED c("A2","A1","B1"), test-fixture-scan-google, test-google-token-signing, test-compliance-tracking mock signature, test-gcal-api).
  3. No loops: rg 'for \(|while (' R/gsheet_api.R R/gcal_api.R → 0.
  4. Order/no-sort/dedup preserved (fixture order, never sort()).
  5. **Stop/error strings verbatim for ALL 22 sinks incl. UNLOCKED gcal env-stop L42** ("GOOGLE_SERVICE_ACCOUNT_JSON environment variable is not set or is empty"), gsheet L163 env-stop, gsheet L139 "Could not extract spreadsheet ID from URL", "Token response is not a list" checks.
  6. **URLs unchanged** (rg googleapis\.com byte-identical pre/post — mock URL matching locks).
  7. **Dead-code REMOVED with evidence**: gsheet L328-342 nested tryCatch collapses to single as.POSIXct(timestamps, format="%m/%d/%Y %H:%M:%S", tz="UTC") — empirical: as.POSIXct on character NEVER errors (format mismatch → NA, no warning; only list input errors, impossible from data[[col]]). Comment cites evidence.
  8. **SNEAKY-PASS GUARD (NEW test):** ISO-format timestamp input → recent_count==0 (NA parse) — refactorer must NOT "fix" the dead fallback into a multi-format parser (MDY fixture keeps lock green while ISO behavior silently flips).
  9. No new deps (no purrr/lubridate/etc).
  10. man/ regen idempotent (document() twice, second clean).
  11. **No type.convert/as.numeric in values-parsing path** (all-character invariant; dual consumers check_recent_responses + compliance_tracking).
  12. **message() call count preserved** (gsheet L359 per-date loop → lapply emitting exactly N message() calls, NOT one concatenated — side effects are behavior).
- **probe:** uv run Rscript -e 'library(testthat); test_dir("tests/testthat", reporter="fail")' && rg -c 'googleapis\.com' ... && rg -n 'for \(|while \(|type\.convert|as\.numeric' ... && git diff --stat NAMESPACE man/
- **negative:** ISO-NA guard missing; wrong-mock tripwire broken (modules merged across seam); URL byte-diff; concatenated message; man/ non-idempotent; "helpful" multi-format parser sneaky-pass.
- **verification:** code. ui: block NOT applicable.
- **fixture status:** 1 NEW test in test-gsheet-api.R (ISO-NA); all else existing.
- **rubric anchor:** §5, §3, §4.

## Design Intent
§1 no signature changes; read-sheet path all-character. §2 parse/extract helpers pure; HTTP/token effectful at edge; message() side effects preserved. §3 gsheet/gcal REMAIN separate modules (separate token fns — merging breaks wrong-mock tripwire); parse_service_account_json extraction within-file preferred (signature divergence). §4 rename map: extract_sheet_id → parse_sheet_id_from_url; intent-first helper names. §5 kill L359 loop (lapply preserving N messages) + L245 concat-loop (fetch_calendar_items helper + unlist(lapply, recursive=FALSE)) + collapse dead tryCatch. Base R lapply/unlist over purrr.

## Technical Context
- Files: R/gsheet_api.R, R/gcal_api.R, man/, test-gsheet-api.R (1 NEW test), NAMESPACE (must NOT change).
- Extraction map (gsheet): build_range_param, parse_service_account_json (dup 3×→1), sign_and_exchange_jwt (dup→1 within file), build_sheet_api_url, parse_sheet_values. (gcal): fetch_calendar_items, own copies of parse/sign helpers (seam preserved).
- Dead-code evidence: L328-342 branches unreachable from sheet data (character df); replace with single call + comment; negative test pins ISO→NA.
- Message-count: exactly length(recent_dates) message() calls — verify via sink capture counting calls.
- Unlocked-sink grep list must survive verbatim (15 kwargs: env-stops, sheet-ID stop, token-not-list, no-access_token, parse failures).
- NOTE: "Could not parse timestamps" L338 stop dies with tryCatch; ISO input now yields all-NA (not stop) — covered by the negative test.

## Dependencies
- depends_on: AC-3.3. Blocks: wave-3b consumer refactors.
- conflict set: R/gsheet_api.R, R/gcal_api.R, man/, test-gsheet-api.R.
- risk: medium.

### Progress
- [ ] refactor — pending
### Decision Log
- spec-resolved — dead-code removal adopted (empirical proof + ISO-NA negative test guard).
### Surprises & Discoveries
- (none yet)
### Idempotence & Recovery
- Safe retry: per-commit lock-green.
- Rollback: git revert; lock suite catches drift.