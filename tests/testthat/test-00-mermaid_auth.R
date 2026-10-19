# This test should be run first (hence 00) - otherwise  other tests make so that .state$token is not NULL and tests fail

test_that("token_available returns a message if .state$token is NULL and verbose is TRUE", {
  expect_message(token_available())
})

test_that("token_available returns a FALSE if .state$token is NULL and verbose is FALSE", {
  expect_false(token_available(verbose = FALSE))
})

test_that("mermaid_token uses MERMAID_TOKEN as a bearer token when set, without touching .state$token", {
  Sys.setenv(MERMAID_TOKEN = "abc")
  on.exit(Sys.unsetenv("MERMAID_TOKEN"))
  token <- mermaid_token()
  expect_identical(token$headers[["Authorization"]], "Bearer abc")
  expect_false(token_available(verbose = FALSE))
})
