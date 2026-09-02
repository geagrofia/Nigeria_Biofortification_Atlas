# ============================================================
# Scraper for https://www.seedportal.org.ng/variety.php
# Extracts all improved seed variety data into a data frame
# and saves to CSV and Excel.

# Each seed variety has a separate page with more information.
#
#
# The detail page URL format is:
#   variety.php?varid=XXX&cropid=YYY&task=view
#
# And the detail pages contain these extra fields: National Code, Original Name, Agroecological Zones, Origin/Source, Developing Institute, Breeder(s)/Collaborators, Potential Yield (t/ha). The links on the list pages contain both varid and cropid, so the strategy is: scrape the links from each list page, then visit each detail page.
# Ran a command, read a file
#
# The script is a two-stage scraper:
#
# Stage 1 — Collect links loops all 41 list pages and harvests every varid= link, then deduplicates them. The detail page URL format is variety.php?varid=XXX&cropid=YYY&task=view.
#
# Stage 2 — Scrape detail pages visits each link and extracts the full set of fields that only appear on the detail page:
# Column	Example
# Crop Name	Cowpea
# National Code	NGVU-16-34
# Variety Name	FUAMPEA 2
# Original Name	UAM09 1051-1
# Outstanding Characteristics	Resistance to Striga...
# Agroecological Zones	Northern Guinea and Sudan Savanna
# Origin/Source	IAR-ABU, Zaria and IITA
# Developing Institute	FUAM and IAR-ABU, Zaria
# Breeder(s)/Collaborators	L.O. Omoigui, L.L. Bello...
# Potential Yield (t/ha)	2 (numeric)
# Year of Release	2016
# Year of Registration	2016

# Runtime estimate: with ~820 varieties (41 pages × ~20 entries) at 1.5s delay, expect around 25–30 minutes total. You can reduce DELAY_SECS to 0.5 if you want it faster, but 1.5s is polite to the server.

# Auto-retry — if a page fetch fails, it waits 3 seconds and tries once more before skipping

# Combined output saved to a single seedportal_varieties.csv and seedportal_varieties.xlsx

#
# Requirements:
#   install.packages(c("rvest", "dplyr", "stringr", "writexl"))
# ============================================================

library(rvest)
library(dplyr)
library(stringr)
library(writexl)

BASE <- "https://www.seedportal.org.ng"
LIST_URL <- paste0(BASE, "/variety.php?keyword=&category=&task=view&page=")
N_PAGES <- 41
DELAY_SECS <- 1.5

# ── STEP 1: Collect detail links + summary yield ─────────────

cat("=== STEP 1: Collecting detail page links & summary yield ===\n")

collect_links <- function(page_num) {
  url <- paste0(LIST_URL, page_num)
  cat(sprintf("  List page %d / %d\n", page_num, N_PAGES))

  page <- tryCatch(read_html(url), error = function(e) {
    Sys.sleep(3)
    tryCatch(read_html(url), error = function(e2) NULL)
  })
  if (is.null(page)) {
    return(tibble())
  }

  # Raw page text split into lines
  lines <- page |>
    html_text2() |>
    str_split("\n") |>
    _[[1]] |>
    str_trim() |>
    (\(x) x[x != ""])()

  # Detail links
  hrefs <- page |> html_elements("a") |> html_attr("href")
  detail_hrefs <- hrefs[str_detect(hrefs, "varid=") & !is.na(hrefs)]
  detail_urls <- ifelse(
    str_starts(detail_hrefs, "http"),
    detail_hrefs,
    paste0(BASE, "/", str_remove(detail_hrefs, "^/"))
  )

  # Summary yield — extract from "Outstanding Characteristics" lines.
  # Every entry has a "View Details" link, but not every entry has a yield.
  # We must extract yield per-entry from the header lines, not from a
  # separate vector, so counts always match.
  header_idx <- which(str_detect(lines, "^»\\s+\\d+\\.\\s+Crop:"))

  get_yield <- function(idx) {
    # Look at the next few lines after the header for Outstanding Characteristics
    candidates <- lines[seq(idx + 1, min(idx + 5, length(lines)))]
    char_line <- candidates[str_detect(
      candidates,
      "Outstanding Characteristics"
    )]
    if (length(char_line) == 0) {
      return(NA_real_)
    }
    val <- str_match(char_line[1], "\\((\\d+\\.?\\d*)\\s*t/ha\\)")[, 2]
    suppressWarnings(as.numeric(val))
  }

  yields <- sapply(header_idx, get_yield)

  # Align: one yield per detail URL (same order on page)
  n <- length(detail_urls)
  if (n == 0) {
    return(tibble())
  }

  # Pad yields with NA if fewer were found than links (shouldn't happen, but safe)
  if (length(yields) < n) {
    yields <- c(yields, rep(NA_real_, n - length(yields)))
  }

  tibble(
    detail_url = detail_urls,
    summary_yield = yields[seq_len(n)]
  )
}

link_df <- lapply(seq_len(N_PAGES), function(p) {
  res <- collect_links(p)
  if (p < N_PAGES) {
    Sys.sleep(DELAY_SECS)
  }
  res
}) |>
  bind_rows()

# Deduplicate keeping first occurrence (preserves summary_yield)
link_df <- link_df[!duplicated(link_df$detail_url), ]

cat(sprintf("\nFound %d unique variety detail pages.\n\n", nrow(link_df)))

# ── STEP 2: Scrape each detail page ──────────────────────────

cat("=== STEP 2: Scraping detail pages ===\n")

# All known field labels in the order they appear on the page.
# Used to delimit extraction: each field ends where the next begins.
FIELD_LABELS <- c(
  "Crop Name",
  "National Code",
  "Variety Name",
  "Original Name",
  "Outstanding Characteristics",
  "Agroecological Zones",
  "Origin/Source",
  "Developing Institute",
  "Breeder\\(s\\)/Collaborators",
  "Potential Yield \\(t/ha\\)",
  "Year of Release",
  "Year of Registration"
)

# Build a single alternation pattern of all labels (for delimiter detection)
LABELS_PATTERN <- paste(FIELD_LABELS, collapse = "|")

extract_field <- function(text, label) {
  # Regex: after "Label:" capture everything up to the next label or end-of-string
  pattern <- sprintf(
    "(?s)%s\\s*:\\s*(.*?)(?=(?:%s)\\s*:|$)",
    label,
    LABELS_PATTERN
  )
  m <- str_match(text, regex(pattern, ignore_case = FALSE))
  if (is.na(m[1, 2])) {
    return(NA_character_)
  }
  # Collapse internal whitespace / newlines; strip separators
  m[1, 2] |>
    str_replace_all("[\\n·]+", " ") |>
    str_squish() |>
    na_if("")
}

parse_detail <- function(url, idx, total) {
  cat(sprintf("  Detail page %d / %d\n", idx, total))

  page <- tryCatch(read_html(url), error = function(e) {
    Sys.sleep(3)
    tryCatch(read_html(url), error = function(e2) NULL)
  })
  if (is.null(page)) {
    return(tibble(detail_url = url))
  }

  text <- page |> html_text2()

  tibble(
    detail_url = url,
    `Crop Name` = extract_field(text, "Crop Name"),
    `National Code` = extract_field(text, "National Code"),
    `Variety Name` = extract_field(text, "Variety Name"),
    `Original Name` = extract_field(text, "Original Name"),
    `Outstanding Characteristics` = extract_field(
      text,
      "Outstanding Characteristics"
    ),
    `Agroecological Zones` = extract_field(text, "Agroecological Zones"),
    `Origin/Source` = extract_field(text, "Origin/Source"),
    `Developing Institute` = extract_field(text, "Developing Institute"),
    `Breeder(s)/Collaborators` = extract_field(
      text,
      "Breeder\\(s\\)/Collaborators"
    ),
    `Potential Yield (t/ha)` = {
      raw <- extract_field(text, "Potential Yield \\(t/ha\\)")
      suppressWarnings(as.numeric(raw))
    },
    `Year of Release` = {
      raw <- extract_field(text, "Year of Release")
      suppressWarnings(as.integer(str_extract(raw, "\\d{4}")))
    },
    `Year of Registration` = {
      raw <- extract_field(text, "Year of Registration")
      suppressWarnings(as.integer(str_extract(raw, "\\d{4}")))
    }
  )
}

total <- nrow(link_df)
details <- vector("list", total)

for (i in seq_len(total)) {
  details[[i]] <- parse_detail(link_df$detail_url[[i]], i, total)
  if (i < total) Sys.sleep(DELAY_SECS)
}

detail_df <- bind_rows(details)

# Join summary yield from Step 1
df <- left_join(detail_df, link_df, by = "detail_url") |>
  rename(`Summary Yield (t/ha)` = summary_yield)

cat(sprintf("\n✅ Scraping done! %d varieties collected.\n\n", nrow(df)))

# ── STEP 3: Post-processing — derived indicator columns ──────

write.csv(df, "seedportal_varieties_preprocessing.csv", row.names = FALSE)
write_xlsx(df, "seedportal_varieties_preprocessing.xlsx")

df <- read.csv("seedportal_varieties_preprocessing.csv")


cat("=== STEP 3: Post-processing ===\n")

flag <- function(x, pattern) {
  as.integer(str_detect(x, regex(pattern, ignore_case = TRUE)))
}

df <- df |>
  mutate(
    oc = coalesce(`Outstanding Characteristics`, ""),
    agro = coalesce(`Agroecological Zones`, ""),

    # ── Nutrient indicators (from Outstanding Characteristics) ──
    zinc = flag(oc, "zinc|\\bZn\\b"),
    vit_A = flag(oc, "vitamin\\s*a|vit\\s*a|beta.?carotene"),
    iron = flag(oc, "\\biron\\b|\\bFe\\b"),

    # ── Agroecological zone indicators ──────────────────────────

    # Base detections
    .has_n_guinea = str_detect(
      agro,
      regex("northern\\s*guinea|N\\.?\\s*guinea|northern", ignore_case = TRUE)
    ),
    .has_s_guinea = str_detect(
      agro,
      regex("southern\\s*guinea|S\\.?\\s*guinea|southern", ignore_case = TRUE)
    ),
    .has_guinea = str_detect(agro, regex("guinea", ignore_case = TRUE)),
    .has_savanna = str_detect(
      agro,
      regex("savanna|savana", ignore_case = TRUE)
    ),
    .has_sudan = str_detect(agro, regex("sudan|sudano", ignore_case = TRUE)),
    .has_derived = str_detect(
      agro,
      regex("derived|transition", ignore_case = TRUE)
    ),

    # "guinea" alone (without northern/southern qualifier) → both zones
    .guinea_only = .has_guinea & !.has_n_guinea & !.has_s_guinea,

    # "savanna" without any transition/derived, guinea or sudan qualifier → all savanna zones
    .savanna_only = .has_savanna & !.has_derived & !.has_guinea & !.has_sudan,

    n_guinea = as.integer(.has_n_guinea | .guinea_only | .savanna_only),
    s_guinea = as.integer(.has_s_guinea | .guinea_only | .savanna_only),
    sudan = as.integer(.has_sudan | .savanna_only),
    derived = as.integer(.has_derived | .savanna_only),
    forest = flag(agro, "forest"),
    mid_alt = flag(agro, "mid.?alt"),
    lowland = flag(agro, "lowland|\\blow\\b"),
    sahel = flag(agro, "sahel"),
    all_zones = flag(agro, "\\ball\\b"),

    .keep = "unused" # drop the temporary .has_* / .xxx columns
  ) |>
  # Remove internal working columns (those starting with ".")
  select(-starts_with("."))

cat("Post-processing complete.\n\n")

# ── Preview ───────────────────────────────────────────────────
print(
  df |>
    select(
      `Crop Name`,
      `Variety Name`,
      `Summary Yield (t/ha)`,
      `Potential Yield (t/ha)`,
      zinc,
      vit_A,
      iron,
      n_guinea,
      s_guinea,
      sudan,
      forest,
      derived,
      mid_alt,
      lowland,
      sahel,
      all_zones
    )
)

# ── Save outputs ─────────────────────────────────────────────
write.csv(df, "tab_data/seedportal_varieties.csv", row.names = FALSE)
write_xlsx(df, "tab_data/seedportal_varieties.xlsx")

cat("\nSaved: tab_data/seedportal_varieties.csv\n")
cat("Saved: tab_data/seedportal_varieties.xlsx\n")
