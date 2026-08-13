# Modules

One concept doc per `R/` source file with >1 public function. `week12_tracking.R`
(1 export) is covered as a dependency mention inside [run_initial_function](run_initial_function.md).

## Concepts

- [redcap_api](redcap_api.md) - effectful shell over the REDCap API (9 exports, 2 internal helpers)
- [abs_login](abs_login.md) - Livewire session auth + CSV download for the ABS admin portal (5 exports, 2 internal)
- [gsheet_api](gsheet_api.md) - read Google Sheets via service-account OAuth2 (4 exports, 2 internal)
- [gcal_api](gcal_api.md) - read Google Calendar events via service-account OAuth2 (3 exports, 1 internal)
- [run_initial_function](run_initial_function.md) - bootstrap/misc: entry stub, dashboard encryption, clock, enrollment targets (4 exports)
- [compliance_summary](compliance_summary.md) - pure per-participant compliance view (2 exports)
- [compliance_tracking](compliance_tracking.md) - expected-vs-actual session compliance from Google Sheets (2 exports)
- [trad_compliance](trad_compliance.md) - traditional ABM compliance from the ABS portal (1 export, 1 internal)
- [demographics](demographics.md) - summarize REDCap demographic reports (2 exports)
