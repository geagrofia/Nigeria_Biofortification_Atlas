# ============================================================
# Scraper for https://www.seedportal.org.ng/variety.php
# Step 1: Scrapes all 41 list pages to collect detail URLs
#         and the summary-level yield (from the list page)
# Step 2: Visits each detail page to extract full variety info
# Step 3: Post-processing — derived indicator columns
# Saves combined data to CSV + Excel.
#
# Requirements:
#   install.packages(c("rvest", "dplyr", "stringr", "writexl"))
# ============================================================

library(rvest)
library(dplyr)
library(stringr)
library(writexl)

# Extract the last yield figure before "t/ha" in a string.
# Handles: (8.2t/ha)  8.2t/ha  (4.5-5t/ha)  4.5-5t/ha
# For ranges, returns the upper bound.
extract_yield <- function(txt) {
  txt <- coalesce(txt, "")
  # Match optional opening paren, optional lower bound + dash, then the target number, then t/ha
  all_matches <- str_match_all(
    txt,
    "\\(?(?:\\d+\\.?\\d*\\s*-\\s*)?(\\d+\\.?\\d*)\\s*t/ha\\)?"
  )[[1]]
  if (nrow(all_matches) == 0) {
    return(NA_real_)
  }
  suppressWarnings(as.numeric(all_matches[nrow(all_matches), 2]))
}

BASE <- "https://www.seedportal.org.ng"
LIST_URL <- paste0(BASE, "/variety.php?keyword=&category=&task=view&page=")
N_PAGES <- 41
DELAY_SECS <- 1.5

# ── STEP 1: Collect detail links + summary yield ─────────────

cat("=== STEP 1: Collecting detail page links & summary yield ===\n")

# ── DIAGNOSTIC — run this block first to check page structure ──
# Uncomment the lines below, run them, then share the output so
# the scraper can be adjusted to match the actual page structure.
#
# test_page <- read_html(paste0(LIST_URL, "1"))
#
# # Check 1: what do the raw lines look like?
# test_lines <- test_page |> html_text2() |> str_split("\n") |> _[[1]] |> str_trim() |> (\(x) x[nchar(x) > 0])()
# cat("--- First 60 non-empty lines ---\n")
# cat(paste(head(test_lines, 60), collapse = "\n"), "\n\n")
#
# # Check 2: are there any varid= links?
# test_hrefs <- test_page |> html_elements("a") |> html_attr("href")
# cat("--- All hrefs on page 1 ---\n")
# cat(paste(head(test_hrefs[!is.na(test_hrefs)], 30), collapse = "\n"), "\n\n")
#
# # Check 3: does the » character appear?
# cat("--- Lines containing » ---\n")
# cat(paste(test_lines[str_detect(test_lines, "»")], collapse = "\n"), "\n\n")
# ── END DIAGNOSTIC ─────────────────────────────────────────────

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

  # --- URLs: from anchor elements (hrefs are not in html_text2 output) ---
  # Each entry has exactly one varid= link (the variety name link and
  # "View Details" share the same href, so we take unique hrefs in order)
  hrefs <- page |>
    html_elements("a[href*='varid=']") |>
    html_attr("href") |>
    unique()

  if (length(hrefs) == 0) {
    return(tibble())
  }

  detail_urls <- ifelse(
    str_starts(hrefs, "http"),
    hrefs,
    paste0(BASE, "/", str_remove(hrefs, "^/"))
  )

  # --- Yields: from text lines, one per entry, matched by position ---
  lines <- page |>
    html_text2() |>
    str_split("\n") |>
    _[[1]] |>
    str_trim() |>
    (\(x) x[nchar(x) > 0])()

  entry_starts <- which(str_detect(lines, "^»\\s+\\d+\\.\\s+Crop:\\s+\\S"))
  cat(sprintf(
    "    -> %d entries, %d unique URLs\n",
    length(entry_starts),
    length(detail_urls)
  ))

  yields <- sapply(seq_along(entry_starts), function(i) {
    start <- entry_starts[i]
    end <- if (i < length(entry_starts)) {
      entry_starts[i + 1] - 1
    } else {
      length(lines)
    }
    block <- lines[start:end]
    char_line <- block[str_detect(block, "Outstanding Characteristics")]
    if (length(char_line) > 0) extract_yield(char_line[1]) else NA_real_
  })

  # Match by position — both vectors are in page order
  n <- min(length(detail_urls), length(yields))
  if (n == 0) {
    return(tibble())
  }

  tibble(
    detail_url = detail_urls[seq_len(n)],
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

# Deduplicate across pages keeping first occurrence (preserves summary_yield)
link_df <- distinct(link_df, detail_url, .keep_all = TRUE)

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

write.csv(
  df,
  "tab_data/seedportal_varieties_preprocessing.csv",
  row.names = FALSE
)
write_xlsx(df, "tab_data/seedportal_varieties_preprocessing.xlsx")

df <- read.csv("tab_data/seedportal_varieties_preprocessing.csv")


cat("=== STEP 3: Post-processing ===\n")

flag <- function(x, pattern) {
  as.integer(str_detect(x, regex(pattern, ignore_case = TRUE)))
}

#  # OLD METHOD
#
# # Extract a numeric micronutrient content value from an OC string.
# # Strategy: find all numbers followed by a mass/concentration unit anywhere
# # in the string, then return the one that appears closest after the
# # nutrient keyword. This avoids relying on a fixed lookahead distance.
# # Units matched: mg, ug, µg, mcg — optionally followed by /kg, /g, /100g etc.
# NUTRIENT_UNIT_PATTERN <- "(?:\\d+\\.?\\d*)\\s*(?:mg|µg|mcg|ug|ppm)(?:/[a-zA-Z0-9]+)?"
#
# extract_nutrient_content <- Vectorize(
#   function(oc, nutrient_pattern) {
#     if (is.na(oc) || oc == "") {
#       return(NA_real_)
#     }
#
#     # Find where the nutrient keyword ends
#     kw_match <- regexpr(nutrient_pattern, oc, ignore.case = TRUE, perl = TRUE)
#     if (kw_match == -1) {
#       return(NA_real_)
#     }
#     kw_end <- kw_match[1] + attr(kw_match, "match.length") - 1
#
#     # Find all value+unit occurrences using base gregexpr (positions + values together)
#     hits <- gregexpr(
#       NUTRIENT_UNIT_PATTERN,
#       oc,
#       ignore.case = TRUE,
#       perl = TRUE
#     )[[1]]
#     if (hits[1] == -1) {
#       return(NA_real_)
#     }
#
#     hit_starts <- as.integer(hits)
#     hit_lengths <- attr(hits, "match.length")
#
#     # Keep only hits that start after the keyword
#     after <- hit_starts > kw_end
#     if (!any(after)) {
#       return(NA_real_)
#     }
#
#     # Extract the substring of the first hit after the keyword, then pull the number
#     first_start <- hit_starts[which(after)[1]]
#     first_length <- hit_lengths[which(after)[1]]
#     hit_str <- substr(oc, first_start, first_start + first_length - 1)
#
#     suppressWarnings(as.numeric(str_extract(hit_str, "\\d+\\.?\\d*")))
#   },
#   vectorize.args = "oc"
# )

# Extract a numeric micronutrient content value from an OC string.
# Strategy: find all numbers followed by a mass/concentration unit anywhere
# in the string, then return the one that appears closest after the
# nutrient keyword. This avoids relying on a fixed lookahead distance.
# Units matched: mg, ug, µg, mcg — optionally followed by /kg, /g, /100g etc.
NUTRIENT_UNIT_PATTERN <- "(?:\\d+\\.?\\d*)\\s*(?:mg|µg|mcg|ug|ppm)(?:/[a-zA-Z0-9]+)?"


# ── DEBUG version of extract_nutrient_content ─────────────────
# Call debug_nutrient_content(oc, pattern) on a single string to
# see exactly what each step finds. Remove or comment out after fixing.
debug_nutrient_content <- function(oc, nutrient_pattern) {
  cat("=== DEBUG extract_nutrient_content ===\n")
  cat("OC       :", oc, "\n")
  cat("Pattern  :", nutrient_pattern, "\n\n")

  kw_group <- paste0("(?:", nutrient_pattern, ")")

  # Strategy 1
  pre_pattern <- paste0(
    "(\\d+\\.?\\d*)\\s*(?:mg|µg|mcg|ug)(?:/[a-zA-Z0-9]+)?.{0,20}?",
    kw_group
  )
  cat("Strategy 1 pre_pattern:", pre_pattern, "\n")
  m_pre <- regmatches(
    oc,
    regexpr(pre_pattern, oc, ignore.case = TRUE, perl = TRUE)
  )
  cat(
    "Strategy 1 match      :",
    if (length(m_pre) > 0) m_pre else "(none)",
    "\n"
  )
  if (length(m_pre) > 0) {
    val <- suppressWarnings(as.numeric(str_extract(m_pre, "^\\d+\\.?\\d*")))
    cat("Strategy 1 value      :", val, "\n")
    if (!is.na(val)) {
      cat("==> RESULT:", val, "\n\n")
      return(val)
    }
  }

  # Strategy 2
  kw_match <- regexpr(kw_group, oc, ignore.case = TRUE, perl = TRUE)
  cat(
    "\nStrategy 2 keyword match:",
    kw_match[1],
    "length:",
    attr(kw_match, "match.length"),
    "\n"
  )
  if (kw_match == -1) {
    cat("==> RESULT: NA (no keyword)\n\n")
    return(NA_real_)
  }
  kw_end <- kw_match[1] + attr(kw_match, "match.length") - 1
  cat("Strategy 2 kw_end     :", kw_end, "\n")

  hits <- gregexpr(NUTRIENT_UNIT_PATTERN, oc, ignore.case = TRUE, perl = TRUE)[[
    1
  ]]
  cat("Strategy 2 unit hits  :", as.integer(hits), "\n")
  cat(
    "Strategy 2 unit strs  :",
    sapply(seq_along(as.integer(hits)), function(i) {
      s <- as.integer(hits)[i]
      l <- attr(hits, "match.length")[i]
      substr(oc, s, s + l - 1)
    }),
    "\n"
  )
  if (hits[1] == -1) {
    cat("==> RESULT: NA (no unit matches)\n\n")
    return(NA_real_)
  }

  hit_starts <- as.integer(hits)
  hit_lengths <- attr(hits, "match.length")
  after <- hit_starts > kw_end
  cat("Strategy 2 after kw   :", after, "\n")
  if (!any(after)) {
    cat("==> RESULT: NA (no units after keyword)\n\n")
    return(NA_real_)
  }

  after_idx <- which(after)
  distances <- hit_starts[after_idx] - kw_end
  closest_idx <- after_idx[which.min(distances)]
  cat("Strategy 2 distances  :", distances, "\n")
  cat(
    "Strategy 2 closest hit:",
    substr(
      oc,
      hit_starts[closest_idx],
      hit_starts[closest_idx] + hit_lengths[closest_idx] - 1
    ),
    "\n"
  )
  val <- suppressWarnings(as.numeric(str_extract(
    substr(
      oc,
      hit_starts[closest_idx],
      hit_starts[closest_idx] + hit_lengths[closest_idx] - 1
    ),
    "\\d+\\.?\\d*"
  )))
  cat("==> RESULT:", val, "\n\n")
  val
}


extract_nutrient_content <- Vectorize(
  function(oc, nutrient_pattern) {
    if (is.na(oc) || oc == "") {
      return(NA_real_)
    }

    # Wrap nutrient pattern in non-capturing group so | alternation
    # doesn't break the surrounding pattern
    kw_group <- paste0("(?:", nutrient_pattern, ")")

    # Strategy 1: value appears BEFORE the keyword within 20 chars
    # e.g. "(2.42mg of Vit. A"
    pre_pattern <- paste0(
      "(\\d+\\.?\\d*)\\s*(?:mg|µg|mcg|ug)(?:/[a-zA-Z0-9]+)?.{0,20}?",
      kw_group
    )
    m_pre <- regmatches(
      oc,
      regexpr(pre_pattern, oc, ignore.case = TRUE, perl = TRUE)
    )
    if (length(m_pre) > 0) {
      val <- suppressWarnings(as.numeric(str_extract(m_pre, "^\\d+\\.?\\d*")))
      if (!is.na(val)) return(val)
    }

    # Strategy 2: value appears AFTER the keyword
    # Find the keyword position, then find the unit hit CLOSEST to it
    kw_match <- regexpr(kw_group, oc, ignore.case = TRUE, perl = TRUE)
    if (kw_match == -1) {
      return(NA_real_)
    }
    kw_end <- kw_match[1] + attr(kw_match, "match.length") - 1

    hits <- gregexpr(
      NUTRIENT_UNIT_PATTERN,
      oc,
      ignore.case = TRUE,
      perl = TRUE
    )[[1]]
    if (hits[1] == -1) {
      return(NA_real_)
    }

    hit_starts <- as.integer(hits)
    hit_lengths <- attr(hits, "match.length")

    after <- hit_starts > kw_end
    if (!any(after)) {
      return(NA_real_)
    }

    # Take the hit closest to (i.e. smallest distance from) the keyword end
    after_idx <- which(after)
    distances <- hit_starts[after_idx] - kw_end
    closest_idx <- after_idx[which.min(distances)]

    hit_str <- substr(
      oc,
      hit_starts[closest_idx],
      hit_starts[closest_idx] + hit_lengths[closest_idx] - 1
    )
    suppressWarnings(as.numeric(str_extract(hit_str, "\\d+\\.?\\d*")))
  },
  vectorize.args = "oc"
)


df <- df |>
  mutate(
    oc = coalesce(Outstanding.Characteristics, ""),
    agro = coalesce(Agroecological.Zones, ""),

    # ── Nutrient indicators (from Outstanding Characteristics) ──
    zinc = flag(oc, "zinc|\\bZn\\b"),
    vit_A = flag(oc, "vitamin\\s*a|\\bvit\\s*a\\b|vit.\\s*a|carot"),
    iron = flag(oc, "\\biron\\b|\\bFe\\b"),

    # ── Micronutrient content values ─────────────────────────────
    zinc_content = extract_nutrient_content(oc, "zinc|\\bZn\\b"),
    iron_content = extract_nutrient_content(oc, "\\biron\\b|\\bFe\\b"),
    vitA_content = extract_nutrient_content(
      oc,
      "vitamin\\s*a|\\bvit\\s*a\\b|vit.\\s*a|carot"
    ),

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
      Crop.Name,
      Variety.Name,
      Summary.Yield..t.ha.,
      Potential.Yield..t.ha.,
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
