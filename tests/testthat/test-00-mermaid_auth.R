# This test should be run first (hence 00) - otherwise  other tests make so that .state$token is not NULL and tests fail

test_that("token_available returns a message if .state$token is NULL and verbose is TRUE", {
  expect_message(token_available())
})

test_that("token_available returns a FALSE if .state$token is NULL and verbose is FALSE", {
  expect_false(token_available(verbose = FALSE))
})

test_that("mermaid_token uses MERMAID_TOKEN as a bearer token when set, without touching .state$token", {
  # Restore any MERMAID_TOKEN already set (e.g. a headless run of the whole suite).
  old_token <- Sys.getenv("MERMAID_TOKEN", unset = NA)
  on.exit(if (is.na(old_token)) Sys.unsetenv("MERMAID_TOKEN") else Sys.setenv(MERMAID_TOKEN = old_token))
  Sys.setenv(MERMAID_TOKEN = "abc")
  token <- mermaid_token()
  expect_identical(token$headers[["Authorization"]], "Bearer abc")
  expect_false(token_available(verbose = FALSE))
})
