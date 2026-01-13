library(yaml)

column_blacklist <- read_yaml(here::here("data-raw", "column_blacklist.yml"))

usethis::use_data(column_blacklist, overwrite = TRUE)

legacy_columns <- read_yaml(here::here("data-raw", "legacy_columns.yml"))

usethis::use_data(legacy_columns, overwrite = TRUE)
