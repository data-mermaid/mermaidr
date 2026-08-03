library(yaml)

blacklist_columns <- read_yaml(here::here("data-raw", "column_blacklist.yml"))

usethis::use_data(blacklist_columns, overwrite = TRUE)

legacy_columns <- read_yaml(here::here("data-raw", "legacy_columns.yml"))

usethis::use_data(legacy_columns, overwrite = TRUE)

tested_endpoints <- c(
  "projects",
  "sites",
  "managements",
  "choices",
  "benthicattributes",
  "fishfamilies",
  "fishgenera",
  "fishspecies",
  "fishsizes",
  "projecttags",
  "me",
  "invertattributes",
  "invertspecies",
  "projectsmanagements",
  "projectssites",
  "summarysampleevents",
  "labelmappings",
  "classification/labelmappings"
)

usethis::use_data(tested_endpoints, overwrite = TRUE)
