# This test should be run first (hence 00) - otherwise  other tests make so that .state$token is not NULL and tests fail

test_that("token_available returns a message if .state$token is NULL and verbose is TRUE", {
  expect_message(token_available())
})

test_that("token_available returns a FALSE if .state$token is NULL and verbose is FALSE", {
  expect_false(token_available(verbose = FALSE))
})
