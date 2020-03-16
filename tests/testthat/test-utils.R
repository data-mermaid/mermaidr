test_that("check_internet throws an error when there is no internet", {
  with_mock(
    "curl::has_internet" = function() FALSE,
    expect_error(check_internet())
  )
})

test_that("as_id throws error when passed a data frame that is not 1 row.", {
  expect_error(
    as_id(
      data.frame(id = c(1, 2))
    ),
    "must be a 1 row data frame or a length 1 character vector."
  )
  expect_error(
    as_id(
      data.frame(id = character())
    ),
    "must be a 1 row data frame or a length 1 character vector."
  )
})

test_that("as_id throws error when passed a list.", {
  expect_error(
    as_id(list(id = 1)),
    "must be a 1 row data frame or a length 1 character vector."
  )
})

test_that("as_id throws error when passed vector of length greater than 1.", {
  expect_error(
    as_id(c("ab", "cd")),
    "must be a 1 row data frame or a length 1 character vector."
  )
})

test_that("as_id throws error when passed a zero-length vector.", {
  expect_error(
    as_id(character()),
    "must be a 1 row data frame or a length 1 character vector."
  )
})

test_that("as_id throws error when passed a non-character vector.", {
  expect_error(
    as_id(1),
    "must be a 1 row data frame or a length 1 character vector."
  )
  expect_error(
    as_id(TRUE),
    "must be a 1 row data frame or a length 1 character vector."
  )
})

test_that('as_id returns the contents of the "id" column when it is passed a 1 row data frame.', {
  expect_equal(
    as_id(data.frame(id = "abcde")),
    "abcde"
  )
})

test_that("as_id returns the object when passed a length 1 character vector", {
  name <- "test"
  expect_equal(as_id(name), name)
})

test_that('check_id_in_df returns an error when passed a data frame that doesn\'t contain an "id" column.', {
  x <- data.frame(a = 1)
  expect_error(
    check_id_in_df(x, "x"),
    '`x` must contain a column "id".'
  )
})

test_that('check_id_in_df returns the "id" column when present.', {
  res <- check_id_in_df(data.frame(id = 5))
  expect_equal(
    names(res),
    "id"
  )
})

test_that("check_limit returns error if limit is not a length 1 positive integer.", {
  expect_error(
    check_limit(limit = -1),
    "`limit` must be a positive integer."
  )
  expect_error(
    check_limit(limit = 0),
    "`limit` must be a positive integer."
  )
  expect_error(
    check_limit(limit = -Inf),
    "`limit` must be a positive integer."
  )
  expect_error(
    check_limit(limit = 1.2),
    "`limit` must be a positive integer."
  )
  expect_error(
    check_limit(limit = c(1, 2)),
    "`limit` must be a length 1 positive integer."
  )
})

test_that("check_limit returns the limit if it is a length 1 positive integer.", {
  limit <- 1
  expect_equal(check_limit(limit), limit)
})

test_that("spf does string formatting and produces an error", {
  x <- 1
  expect_error(spf("x is %s", x), "x is 1")
})

test_that("all_contain_value returns TRUE if all list elements contain the value, FALSE if not", {
  x <- list(a = c("A", "B"), b = c("A"))
  expect_true(all_contain_value(x, "A"))
  expect_false(all_contain_value(x, "B"))
})
