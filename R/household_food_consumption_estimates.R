# ============================================================

# Nigeria GHS-Panel Wave 5

# Household food consumption estimates

#

# LEVELS:

# - State

# - Geopolitical zone

#

# ESTIMATE:

# Annual kg consumed per person

#

# SURVEY DESIGN:

# PSU     = cluster

# Strata  = strata

# Weight  = wt_cross_wave5

#

# OUTPUTS:

# - State × food product estimates

# - Zone × food product estimates

# - Standard errors

# - 95% confidence intervals

# - Coefficients of variation

# - State sample diagnostics

#

# IMPORTANT:

# Results are food PRODUCT consumption.

# They are NOT yet raw-crop-equivalent consumption.

#

# ============================================================

# ------------------------------------------------------------

# 1. PACKAGES

# ------------------------------------------------------------

library(tidyverse)
library(survey)

# ------------------------------------------------------------

# 2. FILE LOCATIONS

# ------------------------------------------------------------

data_dir <- "D:/DatosProyecto/SpatialData/Africa/Pais/Nigeria/LSMS-GHS 2023-2024/Post Harvest Wave 5/Household"

hh_file <- file.path(
  data_dir,
  "secta_harvestw5.csv"
)

roster_file <- file.path(
  data_dir,
  "sect1_harvestw5.csv"
)

food_file <- file.path(
  data_dir,
  "sect5b_harvestw5.csv"
)

# ------------------------------------------------------------

# 3. READ DATA

# Because we're working with the NBS-provided CSV exports, we assign a character so R doesn't guess the column types.

# ------------------------------------------------------------

hh <- read_csv(
  hh_file,
  col_types = cols(.default = col_character()),
  show_col_types = FALSE
)

roster <- read_csv(
  roster_file,
  col_types = cols(.default = col_character()),
  show_col_types = FALSE
)

food <- read_csv(
  food_file,
  col_types = cols(.default = col_character()),
  show_col_types = FALSE
)

# ------------------------------------------------------------

# However, we need to explicitly convert the variables we use for calculations to numeric.

# ------------------------------------------------------------

hh <- hh %>%
  mutate(
    wt_cross_wave5 = as.numeric(wt_cross_wave5)
  )

roster <- roster %>%
  mutate(
    NEWMEMBER = as.numeric(NEWMEMBER)
  )

food <- food %>%
  mutate(
    s5bq2a = as.numeric(s5bq2a),
    s5bq2_cvn = as.numeric(s5bq2_cvn),
    item_cd_int = as.numeric(stringi::stri_extract_first_regex(
      item_cd,
      "[0-9]+"
    ))
  )


# ------------------------------------------------------------

# 4. HOUSEHOLD SIZE

#

# Existing members:

# s1q4 == "1. YES"

#

# New members:

# NEWMEMBER == 1

#

# New members have s1q4 missing.

# ------------------------------------------------------------

roster <- roster %>%
  mutate(
    household_member = s1q4 == "1. YES" |
      NEWMEMBER == 1
  )

hh_size <- roster %>%
  filter(household_member) %>%
  count(
    hhid,
    name = "hh_size"
  )

# Check

summary(hh_size$hh_size)

# ------------------------------------------------------------

# 5. HOUSEHOLD ANALYSIS FILE

#

# Only completed households with a valid cross-sectional

# weight and household size are retained.

# ------------------------------------------------------------

hh_analysis <- hh %>%
  filter(
    interview_result == "1. COMPLETE",
    !is.na(wt_cross_wave5),
    wt_cross_wave5 > 0
  ) %>%
  select(
    hhid,
    zone,
    state,
    lga,
    sector,
    ea,
    cluster,
    strata,
    wt_cross_wave5
  ) %>%
  left_join(
    hh_size,
    by = "hhid"
  ) %>%
  filter(
    !is.na(hh_size),
    hh_size > 0
  )

# Number of households

nrow(hh_analysis)

# ------------------------------------------------------------

# 6. CHECK SURVEY DESIGN VARIABLES

# ------------------------------------------------------------

# Number of strata

n_distinct(hh_analysis$strata)

# Number of PSUs

n_distinct(hh_analysis$cluster)

# Households by state

state_sample <- hh_analysis %>%
  group_by(
    zone,
    state
  ) %>%
  summarise(
    n_households = n(),
    n_psu = n_distinct(cluster),
    weighted_population = sum(
      wt_cross_wave5 * hh_size,
      na.rm = TRUE
    ),
    mean_household_size = weighted.mean(
      hh_size,
      wt_cross_wave5,
      na.rm = TRUE
    ),
    .groups = "drop"
  ) %>%
  arrange(state)

state_sample

# ------------------------------------------------------------

# 7. CREATE SURVEY DESIGN

#

# cluster = original PSU

# strata  = original geopolitical-zone stratum

# weight  = Wave 5 cross-sectional weight

#

# nest = TRUE because the original cluster identifiers are

# treated as nested within the strata.

#

# lonely.psu = "adjust" prevents failures when a domain

# contains only one PSU within a stratum.

# ------------------------------------------------------------

options(
  survey.lonely.psu = "adjust"
)

design_hh <- svydesign(
  ids = ~cluster,
  strata = ~strata,
  weights = ~wt_cross_wave5,
  data = hh_analysis,
  nest = TRUE
)

# ------------------------------------------------------------

# 8. FOOD PRODUCT DICTIONARY

# ------------------------------------------------------------

food_lookup <- tribble(
  ~item_cd , ~product                     ,

        10 , "Guinea corn / sorghum"      ,
        11 , "Millet"                     ,
        13 , "Rice - local"               ,
        14 , "Rice - imported"            ,
        16 , "Maize flour"                ,
        17 , "Yam flour"                  ,
        18 , "Cassava flour"              ,
        19 , "Wheat flour"                ,
        20 , "Maize - unshelled/on cob"   ,
        22 , "Maize - shelled/off cob"    ,
        23 , "Other grains/flour"         ,

        25 , "Bread"                      ,
        26 , "Cake"                       ,
        27 , "Buns/Pofpof/Donuts"         ,
        28 , "Biscuits"                   ,
        29 , "Meat pie/Sausage roll"      ,

        30 , "Cassava roots"              ,
        31 , "Yam roots"                  ,
        32 , "Gari - white"               ,
        33 , "Gari - yellow"              ,
        34 , "Cocoyam"                    ,
        35 , "Plantains"                  ,
        36 , "Sweet potatoes"             ,
        37 , "Potatoes"                   ,
        38 , "Other roots/tubers"         ,

        40 , "Soya beans"                 ,
        41 , "Brown beans"                ,
        42 , "White beans"                ,
        43 , "Groundnuts - unshelled"     ,
        44 , "Groundnuts - shelled"       ,
        45 , "Other nuts/seeds/pulses"    ,
        46 , "Coconut"                    ,
        47 , "Kola nut"                   ,
        48 , "Cashew nut"                 ,

        50 , "Palm oil"                   ,
        51 , "Butter/Margarine"           ,
        52 , "Groundnut oil"              ,
        53 , "Other oil/fat"              ,
        54 , "Shea butter"                ,
        56 , "Animal fat"                 ,

        60 , "Bananas"                    ,
        61 , "Orange/Tangerine"           ,
        62 , "Mangoes"                    ,
        63 , "Avocado pear"               ,
        64 , "Pineapples"                 ,
        66 , "Other fruits"               ,
        67 , "Pawpaw"                     ,
        68 , "Watermelon"                 ,
        69 , "Apples"                     ,

        70 , "Tomatoes"                   ,
        71 , "Tomato puree - canned"      ,
        72 , "Onions"                     ,
        73 , "Garden eggs/Eggplant"       ,
        74 , "Okra - fresh"               ,
        75 , "Okra - dried"               ,
        76 , "Fresh pepper"               ,
        77 , "Dry pepper"                 ,
        78 , "Leaves"                     ,
        79 , "Other vegetables"           ,

        80 , "Chicken"                    ,
        81 , "Duck"                       ,
        82 , "Other domestic poultry"     ,
        83 , "Agricultural eggs"          ,
        84 , "Local eggs"                 ,
        85 , "Other eggs"                 ,

        90 , "Beef"                       ,
        91 , "Mutton"                     ,
        92 , "Pork"                       ,
        93 , "Goat"                       ,
        94 , "Wild game/Bush meat"        ,
        95 , "Canned beef"                ,
        96 , "Other meat"                 ,

       100 , "Fish - fresh"               ,
       101 , "Fish - frozen"              ,
       102 , "Fish - smoked"              ,
       103 , "Fish - dried"               ,
       104 , "Snails"                     ,
       105 , "Seafood"                    ,
       106 , "Canned fish/seafood"        ,
       107 , "Other fish/seafood"         ,

       110 , "Fresh milk"                 ,
       111 , "Milk powder"                ,
       112 , "Baby milk powder"           ,
       113 , "Milk - tinned"              ,
       114 , "Cheese/wara"                ,
       115 , "Other milk products"        ,

       120 , "Coffee"                     ,
       121 , "Chocolate drinks"           ,
       122 , "Tea"                        ,

       130 , "Sugar"                      ,
       132 , "Honey"                      ,
       133 , "Other sweets"               ,

       141 , "Salt"                       ,
       142 , "Ogbono - unground"          ,
       143 , "Ogbono - ground"            ,
       144 , "Ground pepper"              ,
       145 , "Melon - shelled"            ,
       146 , "Melon - unshelled"          ,
       147 , "Melon - ground"             ,
       148 , "Other spices"               ,

       150 , "Bottled water"              ,
       151 , "Sachet water"               ,
       152 , "Malt drinks"                ,
       153 , "Soft drinks"                ,
       154 , "Fruit juice"                ,
       155 , "Other non-alcoholic drinks" ,

       160 , "Beer"                       ,
       161 , "Palm wine"                  ,
       162 , "Pito"                       ,
       163 , "Gin"                        ,
       164 , "Other alcoholic beverages"  ,

       601 , "Guava"
)

# ------------------------------------------------------------

# 9. FOOD CONSUMPTION

#

# s5bq1 = consumed during previous 7 days

# s5bq2a = reported quantity

# s5bq2_cvn = conversion to kg

#

# kg/week = quantity × conversion

# kg/year = kg/week × 365/7

# ------------------------------------------------------------

food_consumed <- food %>%
  filter(
    s5bq1 == "1. Yes",
    !is.na(s5bq2a),
    s5bq2a >= 0
  ) %>%
  mutate(
    kg_week = s5bq2a * s5bq2_cvn,
    kg_year = kg_week * 365 / 7
  )

# ------------------------------------------------------------

# 10. MISSING CONVERSION FACTORS

# ------------------------------------------------------------

missing_conversion <- food_consumed %>%
  filter(
    is.na(s5bq2_cvn)
  ) %>%
  count(
    item_cd,
    item_cd_alt,
    item_os,
    sort = TRUE
  )

missing_conversion

# ------------------------------------------------------------

# 11. ATTACH PRODUCT NAMES

# ------------------------------------------------------------

food_consumed <- food_consumed %>%
  left_join(
    food_lookup,
    by = c("item_cd_int" = "item_cd")
  )

# ------------------------------------------------------------

# 12. INSPECT ALTERNATIVE FOOD CODES

#

# These should NOT yet be automatically converted to the

# standard products.

# ------------------------------------------------------------

alternative_codes <- food_consumed %>%
  filter(
    !is.na(item_cd_alt)
  ) %>%
  count(
    item_cd,
    item_cd_alt,
    item_os,
    sort = TRUE
  )

alternative_codes

# ------------------------------------------------------------

# 13. KEEP STANDARD FOOD PRODUCTS

#

# At this stage we deliberately exclude observations where

# product is NA.

#

# We will deal with item_cd_alt separately after establishing

# the food-product dictionary.

# ------------------------------------------------------------

food_standard <- food_consumed %>%
  filter(
    !is.na(product),
    !is.na(kg_year),
    kg_year >= 0
  )

# ------------------------------------------------------------

# 14. AGGREGATE FOOD TO HOUSEHOLD × PRODUCT

#

# A household can potentially have more than one observation

# contributing to a product.

# ------------------------------------------------------------

food_hh <- food_standard %>%
  group_by(
    hhid,
    product
  ) %>%
  summarise(
    kg_year = sum(
      kg_year,
      na.rm = TRUE
    ),
    .groups = "drop"
  )

# ------------------------------------------------------------

# 15. FUNCTION FOR ONE PRODUCT

#

# This is the important part.

#

# We start with ALL households and then attach the food

# quantity for the selected product.

#

# Therefore:

#

# consumer household -> measured kg/year

# non-consumer        -> 0 kg/year

#

# The denominator remains household size for EVERY household.

#

# This avoids the serious bias that would occur if we only

# analysed households that consumed the product.

# ------------------------------------------------------------

make_product_design <- function(product_name) {
  product_data <- hh_analysis %>%
    select(
      hhid,
      zone,
      state,
      lga,
      sector,
      ea,
      cluster,
      strata,
      wt_cross_wave5,
      hh_size
    ) %>%
    left_join(
      food_hh %>%
        filter(
          product == product_name
        ) %>%
        select(
          hhid,
          kg_year
        ),
      by = "hhid"
    ) %>%
    mutate(
      kg_year = replace_na(
        kg_year,
        0
      )
    )

  svydesign(
    ids = ~cluster,
    strata = ~strata,
    weights = ~wt_cross_wave5,
    data = product_data,
    nest = TRUE
  )
}

# ------------------------------------------------------------

# 16. FUNCTION TO EXTRACT RATIO ESTIMATE

#

# Ratio:

#

# weighted total kg/year

# ---------------------

# weighted total persons

#

# This gives kg/person/year.

# ------------------------------------------------------------

extract_ratio <- function(
  design,
  by_variable,
  denominator = ~hh_size
) {
  result <- svyby(
    formula = ~kg_year,
    by = by_variable,
    design = design,
    FUN = svyratio,
    denominator = denominator,
    na.rm = TRUE,
    keep.names = FALSE,
    vartype = c("se", "cv")
  )

  names(result) <- c(
    all.vars(by_variable),
    "estimate_kg_person_year",
    "se",
    "cv"
  )

  result
}

# ------------------------------------------------------------

# 17. PRODUCTS TO ANALYSE

# ------------------------------------------------------------

products <- sort(
  unique(
    food_hh$product
  )
)

length(products)
products

# ------------------------------------------------------------

# 18. STATE-LEVEL RESULTS

# ------------------------------------------------------------

state_results_list <- vector(
  "list",
  length(products)
)

for (i in seq_along(products)) {
  p <- products[i]

  message(
    "State estimates: ",
    p,
    " (",
    i,
    "/",
    length(products),
    ")"
  )

  d <- make_product_design(p)

  result <- extract_ratio(
    d,
    ~state
  )

  result <- extract_ratio(d, ~state) %>%
    mutate(product = p, .before = 1)

  state_results_list[[i]] <- result
}

state_results <- bind_rows(
  state_results_list
)

# ------------------------------------------------------------

# 19. ADD 95% CONFIDENCE INTERVALS

# ------------------------------------------------------------

state_results <- state_results %>%
  mutate(
    ci_low = pmax(
      0,
      estimate_kg_person_year -
        1.96 * se
    ),

    ci_high = estimate_kg_person_year +
      1.96 * se
  )

# ------------------------------------------------------------

# 20. ADD STATE SAMPLE INFORMATION

# ------------------------------------------------------------

state_results <- state_results %>%
  left_join(
    state_sample,
    by = "state"
  ) %>%
  select(
    state,
    zone,
    product,
    estimate_kg_person_year,
    se,
    ci_low,
    ci_high,
    cv,
    n_households,
    n_psu,
    weighted_population,
    mean_household_size
  ) %>%
  arrange(
    state,
    product
  )

# ------------------------------------------------------------

# 21. PRECISION FLAG

#

# These are analytical flags, not NBS thresholds.

# ------------------------------------------------------------

state_results <- state_results %>%
  mutate(
    precision_flag = case_when(
      is.na(cv) ~
        "No estimate",

      cv < 10 ~
        "CV < 10%",

      cv < 20 ~
        "CV 10–20%",

      cv < 30 ~
        "CV 20–30% — caution",

      TRUE ~
        "CV > 30% — low precision"
    )
  )

# ------------------------------------------------------------

# 22. ZONE-LEVEL RESULTS

# ------------------------------------------------------------

zone_results_list <- vector(
  "list",
  length(products)
)

for (i in seq_along(products)) {
  p <- products[i]

  message(
    "Zone estimates: ",
    p,
    " (",
    i,
    "/",
    length(products),
    ")"
  )

  d <- make_product_design(p)

  result <- extract_ratio(d, ~zone) %>%
    mutate(product = p, .before = 1)

  zone_results_list[[i]] <- result
}

zone_results <- bind_rows(
  zone_results_list
) %>%
  mutate(
    ci_low = pmax(
      0,
      estimate_kg_person_year -
        1.96 * se
    ),
    ci_high = estimate_kg_person_year +
      1.96 * se
  ) %>%
  arrange(
    zone,
    product
  )

# ------------------------------------------------------------

# 23. CHECK RESULTS

# ------------------------------------------------------------

state_results

zone_results

# ------------------------------------------------------------

# 24. EXAMPLE:

# MAIZE PRODUCTS

# ------------------------------------------------------------

maize_results_state <- state_results %>%
  filter(
    product %in%
      c(
        "Maize flour",
        "Maize - unshelled/on cob",
        "Maize - shelled/off cob"
      )
  )

maize_results_state

# ------------------------------------------------------------

# 25. EXAMPLE:

# CASSAVA PRODUCTS

#

# DO NOT add these together yet.

#

# They represent different processed/raw products.

# ------------------------------------------------------------

cassava_results_state <- state_results %>%
  filter(
    product %in%
      c(
        "Cassava roots",
        "Cassava flour",
        "Gari - white",
        "Gari - yellow"
      )
  )

cassava_results_state

# ------------------------------------------------------------

# 26. STATE PRECISION SUMMARY

# ------------------------------------------------------------

state_precision_summary <- state_results %>%
  group_by(
    state
  ) %>%
  summarise(
    n_products = n(),

    n_cv_gt_30 = sum(
      cv > 30,
      na.rm = TRUE
    ),

    median_cv = median(
      cv,
      na.rm = TRUE
    ),

    .groups = "drop"
  ) %>%
  arrange(
    desc(n_cv_gt_30)
  )

state_precision_summary

# ------------------------------------------------------------

# 27. SAVE OUTPUTS

# ------------------------------------------------------------

output_dir <- file.path(
  data_dir,
  "outputs"
)

dir.create(
  output_dir,
  showWarnings = FALSE,
  recursive = TRUE
)

# State × product

write_csv(
  state_results,
  file.path(
    output_dir,
    "GHS_wave5_food_consumption_state.csv"
  )
)

# Zone × product

write_csv(
  zone_results,
  file.path(
    output_dir,
    "GHS_wave5_food_consumption_zone.csv"
  )
)

# State sample information

write_csv(
  state_sample,
  file.path(
    output_dir,
    "GHS_wave5_state_sample_diagnostics.csv"
  )
)

# Precision summary

write_csv(
  state_precision_summary,
  file.path(
    output_dir,
    "GHS_wave5_state_precision_summary.csv"
  )
)

# Alternative codes

write_csv(
  alternative_codes,
  file.path(
    output_dir,
    "GHS_wave5_alternative_food_codes.csv"
  )
)

# Missing conversion factors

write_csv(
  missing_conversion,
  file.path(
    output_dir,
    "GHS_wave5_missing_conversion_factors.csv"
  )
)

# ============================================================

# END OF SCRIPT

# ============================================================
