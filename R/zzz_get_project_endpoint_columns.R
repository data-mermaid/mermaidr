# Combine columns for all get_project_*() endpoints

mermaid_project_endpoint_columns <- list(
  managements = project_managements_columns,
  sites = project_sites_columns
)

# from mermaid_get_project_endpoint
mermaid_project_endpoint_columns <- append(mermaid_project_endpoint_columns, project_other_endpoint_columns)

# from mermaid_get_project_data
mermaid_project_endpoint_columns <- append(mermaid_project_endpoint_columns, project_data_columns)

# from mermaid_get_project_classification_images
mermaid_project_endpoint_columns <- append(mermaid_project_endpoint_columns, classification_images_endpoint_columns)

# convert for testing
mermaid_project_endpoint_columns_test <- purrr::map(mermaid_project_endpoint_columns, snakecase::to_snake_case)
