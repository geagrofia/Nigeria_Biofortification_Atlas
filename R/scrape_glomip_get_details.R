library(readr)
library(dplyr)
library(httr2)
library(jsonlite)
library(purrr)
library(tibble)


# Read the CSV created in the ID-checking step
id_check <- read_csv("glomip_id_check.csv", show_col_types = FALSE)

# Keep only valid IDs
valid_ids <- id_check |>
  filter(status == "success") |>
  pull(catalog_id) |>
  unique()

length(valid_ids)


base_url <- "https://glomip.cgiar.org/product-catalog/details/"

get_full_variety <- function(id) {
  tryCatch(
    {
      response <- request(paste0(base_url, id)) |>
        req_user_agent("Research data retrieval") |>
        req_perform()

      data <- resp_body_json(
        response,
        simplifyVector = FALSE
      )

      if (is.null(data$variety)) {
        return(NULL)
      }

      data$catalog_id <- id
      data
    },
    error = function(e) {
      message("Error for ID ", id, ": ", conditionMessage(e))
      NULL
    }
  )
}

# Retrieve all valid records
full_records <- map(valid_ids, function(id) {
  result <- get_full_variety(id)
  Sys.sleep(1)
  result
})

# Remove unsuccessful responses
full_records <- compact(full_records)

length(full_records)


# Convert every field to a single character value
to_csv_cell <- function(value) {
  if (is.null(value)) {
    return(NA_character_)
  }

  if (is.list(value) || length(value) > 1) {
    return(as.character(
      toJSON(value, auto_unbox = TRUE, null = "null")
    ))
  }

  as.character(value)
}

# Convert each variety record to a one-row tibble
variety_rows <- map(full_records, function(x) {
  fields <- lapply(x, to_csv_cell)

  as_tibble(fields, .name_repair = "minimal")
})

# Combine records with consistent column types
varieties_full <- bind_rows(variety_rows)

# Save the complete catalogue
write_csv(
  varieties_full,
  "glomip_varieties_full.csv",
  na = ""
)

# Inspect
View(varieties_full)

nrow(varieties_full)
length(valid_ids)
