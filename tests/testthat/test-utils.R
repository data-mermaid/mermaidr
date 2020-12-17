test_that("check_internet throws an error when there is no internet", {
  with_mock(
    "curl::has_internet" = function() FALSE,
    expect_error(check_internet())
  )
})

test_that("as_id throws error when passed a list.", {
  expect_error(
    as_id(list(id = 1)),
    "must be a data frame or character vector."
  )
})

test_that("as_id doesn't error when passed vector of length greater than 1.", {
  expect_silent(
    as_id(c("ab", "cd"))
  )
  expect_equal(
    as_id(c("ab", "cd")),
    c("ab", "cd")
  )
})

test_that("as_id throws error when passed a non-character vector.", {
  expect_error(
    as_id(1),
    "must be a data frame or character vector."
  )
  expect_error(
    as_id(TRUE),
    "must be a data frame or character vector."
  )
})

test_that('as_id returns the contents of the "id" column when it is passed a row data frame.', {
  expect_equal(
    as_id(data.frame(id = "abcde")),
    "abcde"
  )
  expect_equal(
    as_id(data.frame(id = c("abcde", "fghi"))),
    c("abcde", "fghi")
  )
})

test_that("as_id returns the object when passed a character vector", {
  name <- "test"
  expect_equal(as_id(name), name)
  expect_equal(as_id(c("name1", "name2")), c("name1", "name2"))
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

test_that("check_limit returns error if limit is not a length 1 positive integer or NULL.", {
  expect_error(
    check_limit(limit = -1),
    "must be NULL or a length 1 positive integer"
  )
  expect_error(
    check_limit(limit = 0),
    "must be NULL or a length 1 positive integer"
  )
  expect_error(
    check_limit(limit = -Inf),
    "must be NULL or a length 1 positive integer"
  )
  expect_error(
    check_limit(limit = 1.2),
    "must be NULL or a length 1 positive integer"
  )
  expect_error(
    check_limit(limit = c(1, 2)),
    "must be NULL or a length 1 positive integer"
  )
})

test_that("check_limit returns the limit if it is a length 1 positive integer.", {
  limit <- 1
  expect_equal(check_limit(limit), limit)
})

test_that("check_limit does not error if limit is NULL, returns NULL", {
  expect_equal(check_limit(NULL), NULL)
  expect_silent(check_limit(NULL))
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

test_that("replace_one_for_many properly replaces one element of a vector with multiple", {
  expect_equal(sub_one_for_many(c("observations", "sampleevents", "sampleunits"), "observations", c("colonies_bleached", "percent_cover")), c("colonies_bleached", "percent_cover", "sampleevents", "sampleunits"))
  expect_equal(sub_one_for_many(c("observations", "sampleevents"), "observations", c("colonies_bleached", "percent_cover")), c("colonies_bleached", "percent_cover", "sampleevents"))
  expect_equal(sub_one_for_many(c("sampleevents", "observations"), "observations", c("colonies_bleached", "percent_cover")), c("sampleevents", "colonies_bleached", "percent_cover"))
  expect_equal(sub_one_for_many(c("sampleevents", "observations", "sampleunits"), "observations", c("colonies_bleached", "percent_cover")), c("sampleevents", "colonies_bleached", "percent_cover", "sampleunits"))
  expect_equal(sub_one_for_many("observations", "observations", c("colonies_bleached", "percent_cover")), c("colonies_bleached", "percent_cover"))
  expect_equal(sub_one_for_many(c("sampleevents", "sampleunits"), "observations", c("colonies_bleached", "percent_cover")), c("sampleevents", "sampleunits"))
})
