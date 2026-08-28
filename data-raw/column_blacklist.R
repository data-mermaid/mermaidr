library(yaml)

blacklist_columns <- read_yaml(here::here("data-raw", "column_blacklist.yml"))

usethis::use_data(blacklist_columns, overwrite = TRUE)
