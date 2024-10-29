mermaid_get_project_classification_images <- function(project, exclude = NULL, limit = NULL, token = mermaid_token()) {
  get_project_endpoint(project, "classification/images",
    filter = list(exclude = paste0(exclude, collapse = ",")),
    limit = limit, token = token
  )
}

classification_images_endpoint_columns <- list(
  "classification/images" = c(
    "id", "updated_by", "classification_status", "patch_size", "num_confirmed",
    "num_unconfirmed", "num_unclassified", "points", "created_on", "updated_on",
    "collect_record_id", "image", "thumbnail", "annotations_file", "feature_vector_file",
    "name", "original_image_name", "original_image_width", "original_image_height",
    "photo_timestamp", "location", "comments", "data", "created_by"
  )
)

classification_images_endpoint_columns_expanded <- list(
  "classification/images" = "classification_status"
)
