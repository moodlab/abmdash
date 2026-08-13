# JWT signing path exercised end-to-end with a THROWAWAY RSA key — AC-3.3(h)
#
# The key is generated in-memory by openssl::rsa_keygen() at test time and is
# NEVER committed: no key material on disk, nothing in fixtures (the
# fixture-scan in test-fixture-scan-google.R enforces this). The token
# endpoint is mocked at the HTTP layer via httr2::with_mocked_responses(),
# which matches the URL only — the JWT assertion body is never inspected — so
# the per-run key non-determinism is harmless.
#
# These tests prove the full jose path end-to-end: jose::jwt_claim() ->
# openssl::read_key() -> jose::jwt_encode_sig() -> httr2 POST to
# oauth2.googleapis.com/token -> resp_body_json() -> access_token.

test_that("get_google_access_token signs a JWT and parses the token response", {
  key <- openssl::rsa_keygen(2048)
  service_account <- list(
    client_email = "svc-test@example.com",
    private_key = openssl::write_pem(key)
  )

  token <- httr2::with_mocked_responses(
    mock_google_fixture(testthat::test_path("fixtures", "gcal")),
    get_google_access_token(service_account)
  )

  expect_equal(token, "fake-token-abcdef")
})

test_that("get_google_sheets_access_token signs a JWT from env and returns token", {
  key <- openssl::rsa_keygen(2048)
  sa_json <- jsonlite::toJSON(
    list(client_email = "svc-test@example.com", private_key = openssl::write_pem(key)),
    auto_unbox = TRUE
  )
  withr::local_envvar(GOOGLE_SERVICE_ACCOUNT_JSON = sa_json)

  token <- httr2::with_mocked_responses(
    mock_google_fixture(testthat::test_path("fixtures", "gcal")),
    get_google_sheets_access_token()
  )

  expect_equal(token, "fake-token-abcdef")
})
