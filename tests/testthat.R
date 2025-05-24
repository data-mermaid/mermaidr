library(testthat)
library(mermaidr)

# Copy over .httr-oauth to tests/testthat/

# test_token <- here::here("tests/testthat/.httr-oauth")
#
# fs::file_delete(test_token)
# fs::file_copy(here::here(".httr-oauth"), test_token)

test_check("mermaidr")
