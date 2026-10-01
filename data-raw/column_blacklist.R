library(yaml)

blacklist_columns <- read_yaml(here::here("data-raw", "column_blacklist.yml"))
legacy_columns <- yaml::read_yaml(here::here("data-raw/legacy_columns.yml"))

usethis::use_data(blacklist_columns, legacy_columns, overwrite = TRUE, internal = TRUE)
