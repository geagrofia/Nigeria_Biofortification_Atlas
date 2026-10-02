library(httr2)
library(jsonlite)
library(purrr)
library(dplyr)
library(tibble)
library(stringr)

base_url <- "https://glomip.cgiar.org/product-catalog/details/"

dir.create("glomip_json", showWarnings = FALSE)

# Start with a limited test range
ids <- 1:300

results <- map_dfr(ids, function(id) {
  url <- paste0(base_url, id)

  result <- tryCatch(
    {
      resp <- request(url) |>
        req_user_agent("Research catalogue data retrieval") |>
        req_perform()

      x <- resp_body_json(resp, simplifyVector = FALSE)

      # Check that the response is a non-empty JSON object
      if (!is.list(x) || length(x) == 0) {
        stop("Unexpected response structure")
      }

      # Save the original JSON record
      writeLines(
        toJSON(x, auto_unbox = TRUE, pretty = TRUE),
        file.path("glomip_json", paste0(id, ".json"))
      )

      tibble(
        catalog_id = id,
        status = "success",
        variety = as.character(x$variety %||% NA_character_)
      )
    },
    error = function(e) {
      tibble(
        catalog_id = id,
        status = paste("failed:", conditionMessage(e)),
        variety = NA_character_
      )
    }
  )

  # Be considerate of the website
  Sys.sleep(1)

  result
})

write.csv(
  results,
  "glomip_id_check.csv",
  row.names = FALSE
)

# Review successful IDs
results |> filter(status == "success")
