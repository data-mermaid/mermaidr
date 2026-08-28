base_url <- "https://dev-api.datamermaid.org"
mermaid_audience <- base_url
mermaid_authorize_url <- "https://datamermaid.auth0.com/authorize"
mermaid_access_url <- "https://datamermaid.auth0.com/oauth/token"
mermaid_key <- "6q1XvYG0n75ZaLbFko0gUV4xGud4uPyG"
ua <- httr::user_agent("https://github.com/data-mermaid/mermaidr")

# During development:
# blacklist_columns <- yaml::read_yaml(here::here("data-raw/column_blacklist.yml"))
