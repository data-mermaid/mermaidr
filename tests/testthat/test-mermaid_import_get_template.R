test_that("mermaid_import_get_template returns a single, 0 row named tibble if there is one method passed", {
  skip_if_offline()
  skip_on_ci()
  skip_on_cran()

  template <- mermaid_import_get_template("fishbelt")
  expect_s3_class(template, "tbl_df")
  expect_true(nrow(template) == 0)
  expect_named(template)
})

test_that("check_excel_file errors if file is not xls/x and passes if it is", {
  expect_error(check_excel_file("test.csv"))
  expect_error(check_excel_file(".csv"))
  expect_error(check_excel_file("xlsx"))
  expect_error(check_excel_file(".xlsx"))
  expect_silent(check_excel_file("test.xlsx"))
  expect_silent(check_excel_file("test.xls"))
  expect_silent(check_excel_file("test.XLSX"))
  expect_silent(check_excel_file("/Users/sharla/Documents/Consulting/Wildlife Conservation Society/mermaidr/test.xlsx"))
})
