# summarise_all_validations_statuses returns the correct messaging

    Code
      res %>% tidyr::uncount(weights = n) %>% summarise_all_statuses(c("error",
        "warning", "ok"), "validate", "NONE")
    Message
      ✖ 4 records produced errors in validation
      • 2 records produced warnings in validation
      ✔ 1 record successfully validated without warnings or errors

