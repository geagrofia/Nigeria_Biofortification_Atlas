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

# Post-harvest files

ph_data_dir <- "D:/DatosProyecto/SpatialData/Africa/Pais/Nigeria/LSMS-GHS 2023-2024/Post Harvest Wave 5/Household"

hh_file <- file.path(
  ph_data_dir,
  "secta_harvestw5.csv"
)

roster_file <- file.path(
  ph_data_dir,
  "sect1_harvestw5.csv"
)

food_file <- file.path(
  ph_data_dir,
  "sect5b_harvestw5.csv"
)

# Post-planting files

pp_data_dir <- "D:/DatosProyecto/SpatialData/Africa/Pais/Nigeria/LSMS-GHS 2023-2024/Post Planting Wave 5/Household"

hh_planting_file <- file.path(
  pp_data_dir,
  "secta_plantingw5.csv"
)

roster_planting_file <- file.path(
  pp_data_dir,
  "sect1_plantingw5.csv"
)

food_planting_file <- file.path(
  pp_data_dir,
  "sect6b_plantingw5.csv"
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

hh_planting <- read_csv(
  hh_planting_file,
  col_types = cols(.default = col_character()),
  show_col_types = FALSE
)

roster_planting <- read_csv(
  roster_planting_file,
  col_types = cols(.default = col_character()),
  show_col_types = FALSE
)

food_planting <- read_csv(
  food_planting_file,
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

roster_planting <- roster_planting %>%
  mutate(
    NEWMEMBER = as.numeric(NEWMEMBER)
  )

hh_planting <- hh_planting %>%
  mutate(
    wt_cross_wave5 = as.numeric(wt_cross_wave5)
  )

food_planting <- food_planting %>%
  mutate(
    s6bq2a = as.numeric(s6bq2a),
    s6bq2_cvn = as.numeric(s6bq2_cvn),
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

hh_size_planting <- roster_planting %>%
  mutate(
    household_member = s1q4 == "1. YES" | NEWMEMBER == 1
  ) %>%
  filter(household_member) %>%
  count(
    hhid,
    name = "hh_size"
  )

# Check

summary(hh_size$hh_size)
summary(hh_size_planting$hh_size)

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

hh_analysis_planting <- hh_planting %>%
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
    hh_size_planting,
    by = "hhid"
  ) %>%
  filter(
    !is.na(hh_size),
    hh_size > 0
  )

# Number of households

nrow(hh_analysis)
nrow(hh_analysis_planting)
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

# s5bq1 or s6bq1 = consumed during previous 7 days

# s5bq2a or s6bq2a = reported quantity

# s5bq2_cvn or s6bq2_cvn = conversion to kg

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

food_planting_consumed <- food_planting %>%
  filter(
    s6bq1 == "1. Yes",
    !is.na(s6bq2a),
    s6bq2a >= 0
  ) %>%
  mutate(
    kg_week = s6bq2a * s6bq2_cvn
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

food_planting_consumed <- food_planting_consumed %>%
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

food_standard_planting <- food_planting_consumed %>%
  filter(
    !is.na(product),
    !is.na(kg_week),
    kg_week >= 0
  )

# ------------------------------------------------------------

# 14. AGGREGATE FOOD TO HOUSEHOLD × PRODUCT

#

# A household can potentially have more than one observation

# contributing to a product.

# ------------------------------------------------------------

food_hh <- food_standard %>%
  group_by(hhid, product) %>%
  summarise(kg_year = sum(kg_year, na.rm = TRUE), .groups = "drop")

food_hh_planting <- food_standard_planting %>%
  group_by(hhid, product) %>%
  summarise(kg_week = sum(kg_week, na.rm = TRUE), .groups = "drop")

# ------------------------------------------------------------

# ------------------------------------------------------------
# 14A. CONSUMPTION-PREVALENCE DIAGNOSTICS
# ------------------------------------------------------------

# ------------------------------------------------------------
# CONSUMPTION DIAGNOSTICS
#
# Calculates, for each geographic unit and food product:
#
# 1. % of households consuming
# 2. % of population living in consuming households
# 3. kg/person/week among consumers
# 4. number of consuming households
# 5. total number of eligible households
#
# geographic variable can be:
#   "state"
#   "zone"
# ------------------------------------------------------------

make_consumption_diagnostics <- function(
  hh_analysis,
  food_standard,
  survey_period,
  geographic_variable = "state"
) {
  products <- sort(
    unique(food_standard$product)
  )

  results <- vector(
    "list",
    length(products)
  )

  for (i in seq_along(products)) {
    p <- products[i]

    message(
      survey_period,
      " - ",
      geographic_variable,
      " diagnostics: ",
      p,
      " (",
      i,
      "/",
      length(products),
      ")"
    )

    # --------------------------------------------------------
    # ALL ELIGIBLE HOUSEHOLDS
    # --------------------------------------------------------

    hh <- hh_analysis %>%
      select(
        hhid,
        zone,
        state,
        cluster,
        strata,
        wt_cross_wave5,
        hh_size
      ) %>%

      left_join(
        food_standard %>%
          filter(
            product == p
          ) %>%
          group_by(
            hhid
          ) %>%
          summarise(
            kg_week = sum(
              kg_week,
              na.rm = TRUE
            ),
            .groups = "drop"
          ),
        by = "hhid"
      ) %>%

      mutate(
        # A household is a consumer if it has a food record
        # for this product.
        consumed = !is.na(kg_week),

        # Non-consumers have zero consumption.
        kg_week = replace_na(
          kg_week,
          0
        )
      )

    # --------------------------------------------------------
    # SURVEY DESIGN
    # --------------------------------------------------------

    d <- svydesign(
      ids = ~cluster,
      strata = ~strata,
      weights = ~wt_cross_wave5,
      data = hh,
      nest = TRUE
    )

    # --------------------------------------------------------
    # HOUSEHOLD CONSUMPTION PREVALENCE
    # --------------------------------------------------------

    f_hh <- as.formula(
      paste0(
        "~consumed"
      )
    )

    f_geo <- as.formula(
      paste0(
        "~",
        geographic_variable
      )
    )

    hh_prev <- svyby(
      f_hh,
      f_geo,
      d,
      svymean,
      na.rm = TRUE
    )

    # Find the estimate column rather than assuming its name
    estimate_col <- setdiff(
      names(hh_prev),
      geographic_variable
    )[1]

    hh_prev <- hh_prev %>%
      rename(
        hh_consumption_prevalence = all_of(estimate_col)
      ) %>%
      mutate(
        hh_consumption_percent = 100 * hh_consumption_prevalence
      )

    # --------------------------------------------------------
    # POPULATION CONSUMPTION PREVALENCE
    #
    # A household contributes its household size to the
    # population denominator.
    # --------------------------------------------------------

    population_design <- svydesign(
      ids = ~cluster,
      strata = ~strata,
      weights = ~wt_cross_wave5,
      data = hh,
      nest = TRUE
    )

    # Use a population-weighted numerator and denominator
    # explicitly rather than changing the survey weights.
    hh <- hh %>%
      mutate(
        consuming_persons = hh_size * as.numeric(consumed),

        total_persons = hh_size
      )

    d_population <- svydesign(
      ids = ~cluster,
      strata = ~strata,
      weights = ~wt_cross_wave5,
      data = hh,
      nest = TRUE
    )

    pop_ratio <- svyby(
      ~consuming_persons,
      f_geo,
      d_population,
      svytotal,
      na.rm = TRUE
    )

    pop_total <- svyby(
      ~total_persons,
      f_geo,
      d_population,
      svytotal,
      na.rm = TRUE
    )

    pop_prev <- pop_ratio %>%
      select(
        all_of(geographic_variable),
        consuming_persons
      ) %>%
      left_join(
        pop_total %>%
          select(
            all_of(geographic_variable),
            total_persons
          ),
        by = geographic_variable
      ) %>%
      mutate(
        population_consumption_prevalence = consuming_persons /
          total_persons,

        population_consumption_percent = 100 *
          population_consumption_prevalence
      )

    # --------------------------------------------------------
    # NUMBER OF CONSUMING HOUSEHOLDS
    #
    # These are UNWEIGHTED sample counts and are useful as a
    # diagnostic for small sample sizes.
    # --------------------------------------------------------

    sample_counts <- hh %>%
      group_by(
        .data[[geographic_variable]]
      ) %>%
      summarise(
        n_households = n(),

        n_consuming_households = sum(
          consumed,
          na.rm = TRUE
        ),

        .groups = "drop"
      ) %>%
      rename(
        geographic_unit = all_of(geographic_variable)
      )

    # --------------------------------------------------------
    # CONSUMPTION AMONG CONSUMERS
    #
    # kg/person/week among households that actually consumed
    # the product.
    # --------------------------------------------------------

    consumer_hh <- hh %>%
      filter(
        consumed
      )

    if (nrow(consumer_hh) > 0) {
      d_consumer <- svydesign(
        ids = ~cluster,
        strata = ~strata,
        weights = ~wt_cross_wave5,
        data = consumer_hh,
        nest = TRUE
      )

      consumer_results <- lapply(
        sort(
          unique(
            consumer_hh[[geographic_variable]]
          )
        ),
        function(g) {
          dc <- subset(
            d_consumer,
            get(geographic_variable) == g
          )

          if (nrow(dc) == 0) {
            return(
              tibble(
                geographic_unit = g,
                consumer_kg_person_week = NA_real_,
                consumer_se = NA_real_,
                consumer_cv = NA_real_
              )
            )
          }

          r <- svyratio(
            ~kg_week,
            ~hh_size,
            dc,
            na.rm = TRUE
          )

          estimate <- as.numeric(
            coef(r)
          )

          se <- as.numeric(
            SE(r)
          )

          tibble(
            geographic_unit = g,
            consumer_kg_person_week = estimate,
            consumer_se = se,
            consumer_cv = if_else(
              estimate > 0,
              100 * se / estimate,
              NA_real_
            )
          )
        }
      ) %>%
        bind_rows()
    } else {
      consumer_results <- tibble(
        geographic_unit = character(),
        consumer_kg_person_week = numeric(),
        consumer_se = numeric(),
        consumer_cv = numeric()
      )
    }

    # --------------------------------------------------------
    # COMBINE
    # --------------------------------------------------------

    results[[i]] <-
      hh_prev %>%

      select(
        all_of(geographic_variable),
        hh_consumption_prevalence,
        hh_consumption_percent
      ) %>%

      rename(
        geographic_unit = all_of(geographic_variable)
      ) %>%

      left_join(
        pop_prev %>%
          select(
            geographic_unit = all_of(geographic_variable),
            population_consumption_prevalence,
            population_consumption_percent
          ),
        by = "geographic_unit"
      ) %>%

      left_join(
        consumer_results,
        by = "geographic_unit"
      ) %>%

      left_join(
        sample_counts,
        by = "geographic_unit"
      ) %>%

      mutate(
        product = p,
        survey_period = survey_period,
        geographic_level = geographic_variable,
        .before = 1
      )
  }

  bind_rows(
    results
  )
}

# ------------------------------------------------------------
# 14B. CALCULATE DIAGNOSTICS FOR BOTH VISITS
# ------------------------------------------------------------

state_consumption_diagnostics <-
  make_consumption_diagnostics(
    hh_analysis,
    food_standard,
    "Post-Harvest"
  )


state_consumption_diagnostics_planting <-
  make_consumption_diagnostics(
    hh_analysis_planting,
    food_standard_planting,
    "Post-Planting"
  )


state_consumption_diagnostics <-
  bind_rows(
    state_consumption_diagnostics_planting,
    state_consumption_diagnostics_harvest
  )


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

# ------------------------------------------------------------
# 27. POST-PLANTING SEASONAL FOOD CONSUMPTION
#
# GHS-Panel Wave 5 was administered twice: post-planting
# (July-September 2023) and post-harvest (January-March 2024).
# The post-planting food module is Section 6B, while the
# post-harvest module used above is Section 5B.
#
# Both ask about quantity consumed during the previous 7 days.
# Therefore the seasonal estimates below are kg/person/week.
# They are NOT two halves of an annual total.
# ------------------------------------------------------------

# Post-planting missing conversion factors
missing_conversion_planting <- food_planting_consumed %>%
  filter(is.na(s6bq2_cvn)) %>%
  count(
    item_cd,
    item_cd_alt,
    item_os,
    sort = TRUE
  )

# Post-planting alternative food codes
alternative_codes_planting <- food_planting_consumed %>%
  filter(!is.na(item_cd_alt)) %>%
  count(
    item_cd,
    item_cd_alt,
    item_os,
    sort = TRUE
  )

# Household × product, post-planting
food_hh_planting <- food_standard_planting %>%
  group_by(
    hhid,
    product
  ) %>%
  summarise(
    kg_week = sum(
      kg_week,
      na.rm = TRUE
    ),
    .groups = "drop"
  )

# Post-planting state sample diagnostics
state_sample_planting <- hh_analysis_planting %>%
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
  )

# Function for a single post-planting product
make_product_design_planting <- function(product_name) {
  product_data <- hh_analysis_planting %>%
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
      food_hh_planting %>%
        filter(product == product_name) %>%
        select(
          hhid,
          kg_week
        ),
      by = "hhid"
    ) %>%
    mutate(
      kg_week = replace_na(
        kg_week,
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

# Extract kg/person/week using the same ratio approach as the
# post-harvest analysis.
extract_ratio_week <- function(
  design,
  by_variable
) {
  result <- svyby(
    formula = ~kg_week,
    by = by_variable,
    design = design,
    FUN = svyratio,
    denominator = ~hh_size,
    na.rm = TRUE,
    keep.names = FALSE,
    vartype = c("se", "cv")
  )

  names(result) <- c(
    all.vars(by_variable),
    "estimate_kg_person_week",
    "se",
    "cv"
  )

  result %>%
    mutate(
      ci_low = pmax(
        0,
        estimate_kg_person_week - 1.96 * se
      ),
      ci_high = estimate_kg_person_week + 1.96 * se,
      # Annualized equivalent is included only as a convenient
      # comparison scale. It is NOT an observed annual total.
      estimate_kg_person_year_equivalent = estimate_kg_person_week * 365 / 7
    )
}

products_planting <- sort(
  unique(
    food_hh_planting$product
  )
)

state_results_planting_list <- vector(
  "list",
  length(products_planting)
)

zone_results_planting_list <- vector(
  "list",
  length(products_planting)
)

for (i in seq_along(products_planting)) {
  p <- products_planting[i]

  message(
    "Post-Planting state estimates: ",
    p,
    " (",
    i,
    "/",
    length(products_planting),
    ")"
  )

  d <- make_product_design_planting(p)

  state_results_planting_list[[i]] <-
    extract_ratio_week(
      d,
      ~state
    ) %>%
    mutate(
      survey_period = "Post-Planting",
      product = p,
      .before = 1
    )

  zone_results_planting_list[[i]] <-
    extract_ratio_week(
      d,
      ~zone
    ) %>%
    mutate(
      survey_period = "Post-Planting",
      product = p,
      .before = 1
    )
}

state_results_planting <- bind_rows(
  state_results_planting_list
) %>%
  left_join(
    state_sample_planting,
    by = c(
      "state"
    )
  ) %>%
  mutate(
    precision_flag = case_when(
      is.na(cv) ~ "No estimate",
      cv < 10 ~ "CV < 10%",
      cv < 20 ~ "CV 10–20%",
      cv < 30 ~ "CV 20–30% — caution",
      TRUE ~ "CV > 30% — low precision"
    )
  ) %>%
  arrange(
    state,
    product
  )

zone_results_planting <- bind_rows(
  zone_results_planting_list
) %>%
  arrange(
    zone,
    product
  )

# ------------------------------------------------------------
# 28. COMBINE THE TWO SEASONAL OBSERVATIONS
#
# To make the two visits directly comparable, the post-harvest
# annual estimate is expressed back as kg/person/week.
# The original annual post-harvest output is left unchanged.
# ------------------------------------------------------------

state_seasonal_results <- bind_rows(
  state_results_planting %>%
    transmute(
      survey_period,
      state,
      zone,
      product,
      estimate_kg_person_week,
      se,
      cv,
      ci_low,
      ci_high,
      estimate_kg_person_year_equivalent,
      precision_flag,
      n_households,
      n_psu,
      weighted_population,
      mean_household_size
    ),

  state_results %>%
    transmute(
      survey_period = "Post-Harvest",
      state,
      zone,
      product,
      estimate_kg_person_week = estimate_kg_person_year / (365 / 7),
      se = se / (365 / 7),
      cv,
      ci_low = ci_low / (365 / 7),
      ci_high = ci_high / (365 / 7),
      estimate_kg_person_year_equivalent = estimate_kg_person_year,
      precision_flag,
      n_households,
      n_psu,
      weighted_population,
      mean_household_size
    )
) %>%
  arrange(
    state,
    product,
    survey_period
  )

zone_seasonal_results <- bind_rows(
  zone_results_planting %>%
    transmute(
      survey_period,
      zone,
      product,
      estimate_kg_person_week,
      se,
      cv,
      ci_low,
      ci_high,
      estimate_kg_person_year_equivalent
    ),

  zone_results %>%
    transmute(
      survey_period = "Post-Harvest",
      zone,
      product,
      estimate_kg_person_week = estimate_kg_person_year / (365 / 7),
      se = se / (365 / 7),
      cv,
      ci_low = ci_low / (365 / 7),
      ci_high = ci_high / (365 / 7),
      estimate_kg_person_year_equivalent = estimate_kg_person_year
    )
) %>%
  arrange(
    zone,
    product,
    survey_period
  )

# Direct state-level seasonal comparison
state_seasonal_comparison <- state_seasonal_results %>%
  select(
    survey_period,
    state,
    zone,
    product,
    estimate_kg_person_week
  ) %>%
  pivot_wider(
    names_from = survey_period,
    values_from = estimate_kg_person_week
  ) %>%
  mutate(
    difference_post_harvest_minus_post_planting = `Post-Harvest` -
      `Post-Planting`,
    ratio_post_harvest_to_post_planting = if_else(
      `Post-Planting` > 0,
      `Post-Harvest` / `Post-Planting`,
      NA_real_
    )
  ) %>%
  arrange(
    state,
    product
  )

# Direct zone-level seasonal comparison
zone_seasonal_comparison <- zone_seasonal_results %>%
  select(
    survey_period,
    zone,
    product,
    estimate_kg_person_week
  ) %>%
  pivot_wider(
    names_from = survey_period,
    values_from = estimate_kg_person_week
  ) %>%
  mutate(
    difference_post_harvest_minus_post_planting = `Post-Harvest` -
      `Post-Planting`,
    ratio_post_harvest_to_post_planting = if_else(
      `Post-Planting` > 0,
      `Post-Harvest` / `Post-Planting`,
      NA_real_
    )
  ) %>%
  arrange(
    zone,
    product
  )

# ------------------------------------------------------------
# 29. SAVE OUTPUTS

# ------------------------------------------------------------

# All files

data_dir <- "D:/DatosProyecto/SpatialData/Africa/Pais/Nigeria/LSMS-GHS 2023-2024"

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

#----post-planting----

# State × product

write_csv(
  state_results_planting,
  file.path(
    output_dir,
    "GHS_wave5_food_consumption_state_planting.csv"
  )
)

# Zone × product

write_csv(
  zone_results_planting,
  file.path(
    output_dir,
    "GHS_wave5_food_consumption_zone_planting.csv"
  )
)

# State sample information

write_csv(
  state_sample_planting,
  file.path(
    output_dir,
    "GHS_wave5_state_sample_diagnostics_planting.csv"
  )
)

# # Precision summary
#
# write_csv(
#   state_precision_summary,
#   file.path(
#     output_dir,
#     "GHS_wave5_state_precision_summary.csv"
#   )
# )

# Alternative codes

write_csv(
  alternative_codes_planting,
  file.path(
    output_dir,
    "GHS_wave5_alternative_food_codes_planting.csv"
  )
)

# Missing conversion factors

write_csv(
  missing_conversion_planting,
  file.path(
    output_dir,
    "GHS_wave5_missing_conversion_factors_planting.csv"
  )
)


#-----Comparison-----

# State × product

write_csv(
  state_seasonal_results,
  file.path(
    output_dir,
    "GHS_wave5_food_consumption_state_seasonal.csv"
  )
)


write_csv(
  state_seasonal_comparison,
  file.path(
    output_dir,
    "GHS_wave5_food_consumption_state_seasonal_comparison.csv"
  )
)


# Zone × product

write_csv(
  zone_seasonal_results,
  file.path(
    output_dir,
    "GHS_wave5_food_consumption_zone_seasonal.csv"
  )
)

write_csv(
  zone_seasonal_comparison,
  file.path(
    output_dir,
    "GHS_wave5_food_consumption_zone_seasonal_comparison.csv"
  )
)

# ============================================================

# END OF SCRIPT

# ============================================================
