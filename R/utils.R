check_internet <- function() {
  if (!curl::has_internet()) {
    stop("`mermaidr` does not work offline. Please check your internet connection.",
      call. = FALSE
    )
  }
}

check_limit <- function(limit) {
  if (is.null(limit)) {
    limit
  } else if (length(limit) != 1) {
    stop("`limit` must be NULL or a length 1 positive integer.",
      call. = FALSE
    )
  } else if (!is.numeric(limit) ||
    !(limit %% 1 == 0) ||
    limit <= 0 ||
    limit == Inf) {
    stop("`limit` must be NULL or a length 1 positive integer.",
      call. = FALSE
    )
  } else {
    limit
  }
}

as_id <- function(x) {
  object_name <- deparse(substitute(x))

  if (is.data.frame(x)) {
    x <- check_id_in_df(x, object_name)
    as.character(x[["id"]])
  } else if (!(is.vector(x) && is.character(x))) {
    stop(paste0("`", object_name, "` must be a data frame or character vector."),
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

all_contain_value <- function(x, value) {
  all(unlist(
    lapply(x, FUN = function(x) any(grepl(value, x)))
  ) == TRUE)
}

sub_one_for_many <- function(x, pattern, replacement) {
  removal_index <- which(x == pattern)
  if (length(removal_index) == 0) {
    x
  } else if (removal_index == 1 & length(x) == 1) {
    replacement
  } else if (removal_index == 1 & length(x) > 1) {
    c(replacement, x[2:length(x)])
  } else if (removal_index == length(x)) {
    c(x[1:(length(x) - 1)], replacement)
  } else if (removal_index > 1) {
    c(x[1:(removal_index - 1)], replacement, x[(removal_index + 1):length(x)])
  }
}

combine_coltypes_and_bind_rows <- function(data, .id = NULL) {
  # Try to bind rows - if fails, then do all this
  res <- try(dplyr::bind_rows(data, .id = .id), silent = TRUE)

  if (inherits(res, "try-error")) {

    # Go through each column and get its ptype from each data set
    # If not all the same, combine and get the overall ptype and cast to that
    # Then bind together
    cols <- data %>%
      purrr::map(names) %>%
      unlist() %>%
      unique()

    for (col in cols) {
      col_ptypes <- data %>%
        purrr::map(function(x) {
          if (is.null(x[[col]])) {
            return(NULL)
          }
          vec_class <- x[[col]] %>%
            vctrs::vec_ptype() %>%
            class()

          paste0(vec_class, collapse = "_")
        })

      col_ptypes <- col_ptypes %>%
        purrr::compact() %>%
        unlist() %>%
        unique()

      if (length(col_ptypes) > 1) {
        combined_coltype <- data %>%
          purrr::map(col) %>%
          unlist() %>%
          vctrs::vec_ptype() %>%
          class()
        data <- data %>%
          purrr::map(function(x) {
            if (!is.null(x[[col]])) {
              x[[col]] <- methods::as(x[[col]], combined_coltype)
            }

            x
          })
      }
    }

    res <- data %>%
      dplyr::bind_rows(.id = .id)
  }

  res
}
