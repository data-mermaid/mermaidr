test_that("token_available returns a message if .state$token is NULL and verbose is TRUE", {
  expect_message(token_available())
})

test_that("token_available returns a FALSE if .state$token is NULL and verbose is FALSE", {
  expect_false(token_available(verbose = FALSE))
})
