check_internet <- function() {
  if (!curl::has_internet()) {
    stop("`mermaidr` does not work offline. Please check your internet connection.",
      call. = FALSE
    )
  }
}

check_limit <- function(limit) {
  if (length(limit) != 1) {
    stop("`limit` must be a length 1 positive integer vector.",
      call. = FALSE
    )
  } else if (!is.numeric(limit) ||
    !(limit %% 1 == 0) ||
    limit <= 0 ||
    limit == Inf) {
    stop("`limit` must be a positive integer.",
      call. = FALSE
    )
  } else {
    limit
  }
}

as_id <- function(x) {
  object_name <- deparse(substitute(x))

  if (is.data.frame(x)) {
    if (nrow(x) == 1) {
      x <- check_id_in_df(x, object_name)
      as.character(x[["id"]])
    } else {
      stop(paste0("`", object_name, "` must be a 1 row data frame or a length 1 character vector."),
        call. = FALSE
      )
    }
  } else if (!(is.vector(x) && length(x) == 1 && is.character(x))) {
    stop(paste0("`", object_name, "` must be a 1 row data frame or a length 1 character vector."),
      call. = FALSE
    )
  } else {
    x
  }
}

check_id_in_df <- function(x, name) {
  if (!("id" %in% names(x))) {
    stop(paste0("`", name, '` must contain a column "id".'),
      call. = FALSE
    )
  } else {
    x
  }
}

spf <- function(...) {
  stop(sprintf(...), call. = FALSE)
}

all_contain_value <- function(output, value) {
  all(unlist(
    lapply(output, FUN = function(x) any(x == value))
  ) == TRUE)
}
