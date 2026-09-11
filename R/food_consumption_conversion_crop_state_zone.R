# ============================================================
# GHS WAVE 5 — NIGERIA
# FOOD CONSUMPTION TO CROP-EQUIVALENT CONVERSION
# ============================================================
#
# PURPOSE
# -------
# This script converts the food consumption results produced
# from the GHS Wave 5 analysis into underlying crop equivalents.
#
# INPUTS
# ------
# Two files are required:
#
#   1. food_state_results.csv
#      State-level food consumption
#
#   2. food_zone_results.csv
#      Zone-level food consumption
#
# These files should ALREADY contain the survey-weighted
# consumption estimates expressed as:
#
#       kg/person/year
#
# The household-level survey weighting, household-size
# adjustment, etc. are therefore NOT repeated here.
#
# OUTPUTS
# -------
#   1. crop_state_results.csv
#      Crop-equivalent consumption by state
#
#   2. crop_zone_results.csv
#      Crop-equivalent consumption by zone
#
#   3. conversion_lookup.csv
#      The complete conversion-factor table, including
#      sources and notes.
#
#
# IMPORTANT
# ---------
# This script distinguishes between:
#
#   A. DIRECT CROP PRODUCTS
#      e.g. maize grain, millet, sorghum, cassava roots
#
#   B. PROCESSED CROP PRODUCTS
#      e.g. maize flour, wheat flour, gari, rice
#
#   C. COMPOSITE FOODS
#      e.g. bread, cake, biscuits
#
# Composite foods require information on the proportion of
# the food made from the underlying crop and are therefore
# NOT assigned arbitrary conversion factors.
#
# ============================================================

# ============================================================
# 1. PACKAGES
# ============================================================

library(tidyverse)


# ============================================================
# 2. INPUT FILES
# ============================================================
#
# Change these paths if necessary.
#
# If the CSV files are in the same directory as this script,
# the filenames alone are sufficient.
#
# ============================================================

state_file <- "tab_data/GHS food per capita outputs/GHS_wave5_food_consumption_state_seasonal_comparison.csv"

zone_file <- "tab_data/GHS food per capita outputs/GHS_wave5_food_consumption_zone_seasonal_comparison.csv"


# ============================================================
# 3. LOAD STATE-LEVEL RESULTS
# ============================================================

food_state_results_seasonal <- read_csv(
  state_file,
  show_col_types = FALSE
)

# convert to kg per year

food_state_results <- food_state_results_seasonal |>
  mutate(
    estimate_kg_person_year = (((`Post-Harvest` + `Post-Planting`) / 2) *
      (365 / 7))
  )


# ============================================================
# 4. LOAD ZONE-LEVEL RESULTS
# ============================================================

food_zone_results_seasonal <- read_csv(
  zone_file,
  show_col_types = FALSE
)

# convert to kg per year

food_zone_results <- food_zone_results_seasonal |>
  mutate(
    estimate_kg_person_year = (((`Post-Harvest` + `Post-Planting`) / 2) *
      (365 / 7))
  )


# ============================================================
# 5. CHECK INPUT FILES
# ============================================================

required_columns <- c(
  "product",
  "estimate_kg_person_year"
)


missing_state_columns <-
  setdiff(
    required_columns,
    names(food_state_results)
  )

missing_zone_columns <-
  setdiff(
    required_columns,
    names(food_zone_results)
  )


if (length(missing_state_columns) > 0) {
  stop(
    "The state-level file is missing: ",
    paste(
      missing_state_columns,
      collapse = ", "
    )
  )
}


if (length(missing_zone_columns) > 0) {
  stop(
    "The zone-level file is missing: ",
    paste(
      missing_zone_columns,
      collapse = ", "
    )
  )
}


# ============================================================
# 6. CROP CONVERSION LOOKUP
# ============================================================
#
# factor_mid = principal factor used for the analysis
#
# factor_low / factor_high =
# approximate sensitivity range where appropriate
#
# factor = crop-equivalent kg / food-product kg
#
# For a direct crop:
#
#       1 kg maize grain
#       =
#       1 kg maize crop product
#
# therefore factor = 1.00.
#
# ============================================================

conversion_lookup <- tribble(
  ~product                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    , ~crop                              , ~factor_low , ~factor_mid , ~factor_high ,
  ~status                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     , ~basis                             , ~source     , ~source_url , ~notes       ,

  "Guinea corn / sorghum"                                                                                                                                                                                                                                                                                                                                                                                                                                                                     , "Sorghum"                          , 1.00        , 1.00        , 1.00         ,
  "provisional"                                                                                                                                                                                                                                                                                                                                                                                                                                                                               , "direct grain"                     ,
  "GHS item: Guinea corn / sorghum"                                                                                                                                                                                                                                                                                                                                                                                                                                                           ,
  NA_character_                                                                                                                                                                                                                                                                                                                                                                                                                                                                               ,
  "Direct grain product."                                                                                                                                                                                                                                                                                                                                                                                                                                                                     ,

  "Millet"                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    , "Millet"                           , 1.00        , 1.00        , 1.00         ,
  "provisional"                                                                                                                                                                                                                                                                                                                                                                                                                                                                               , "direct grain"                     ,
  "GHS item: Millet"                                                                                                                                                                                                                                                                                                                                                                                                                                                                          ,
  NA_character_                                                                                                                                                                                                                                                                                                                                                                                                                                                                               ,
  "Direct grain product."                                                                                                                                                                                                                                                                                                                                                                                                                                                                     ,

  "Rice - local"                                                                                                                                                                                                                                                                                                                                                                                                                                                                              , "Rice"                             , 1.00        , 1.00        , 1.00         ,
  "provisional"                                                                                                                                                                                                                                                                                                                                                                                                                                                                               , "direct milled product"            ,
  "GHS item: Rice - local"                                                                                                                                                                                                                                                                                                                                                                                                                                                                    ,
  NA_character_                                                                                                                                                                                                                                                                                                                                                                                                                                                                               ,
  "Treated as rice as consumed."                                                                                                                                                                                                                                                                                                                                                                                                                                                              ,

  "Rice - imported"                                                                                                                                                                                                                                                                                                                                                                                                                                                                           , "Rice"                             , 1.00        , 1.00        , 1.00         ,
  "provisional"                                                                                                                                                                                                                                                                                                                                                                                                                                                                               , "direct milled product"            ,
  "GHS item: Rice - imported"                                                                                                                                                                                                                                                                                                                                                                                                                                                                 ,
  NA_character_                                                                                                                                                                                                                                                                                                                                                                                                                                                                               ,
  "Treated as rice as consumed."                                                                                                                                                                                                                                                                                                                                                                                                                                                              ,

  "Maize flour"                                                                                                                                                                                                                                                                                                                                                                                                                                                                               , "Maize"                            , 1.05        , 1.14        , 1.25         ,
  "provisional"                                                                                                                                                                                                                                                                                                                                                                                                                                                                               , "maize flour extraction"           ,
  "FAO — Quantification of Root Crops in National Food Balance Sheets"                                                                                                                                                                                                                                                                                                                                                                                                                        ,
  "https://www.fao.org/4/y9422e/y9422e04.htm"                                                                                                                                                                                                                                                                                                                                                                                                                                                 ,
  "Maize meal extraction approximately 80–95%."                                                                                                                                                                                                                                                                                                                                                                                                                                               ,

  "Maize - unshelled/on cob"                                                                                                                                                                                                                                                                                                                                                                                                                                                                  , "Maize"                            , 0.75        , 0.8         , 0.85         ,
  "provisional"                                                                                                                                                                                                                                                                                                                                                                                                                                                                               , "on-cob to grain conversion"       ,
  "Omnicalculator; SeedCo Zambia"                                                                                                                                                                                                                                                                                                                                                                                                                                                             ,
  "https://www.omnicalculator.com/biology/grain-conversion ; https://www.google.com/url?sa=i&source=web&rct=j&url=/goto?url%3DCAESaQHrOzAVLcavph2gJWT-NV7fTKMrDluyjBrI2pyPhWp3IandGoIPqjS2sYekF9De8QtlOVlzv2Xe2bvni7TGH_uPHvmLA2_l9vjUACTUnbdFNVwj9uE-hg-TJs6a_5P8h8sXO2dDdv1z4g&ved=2ahUKEwiMhOvYnOWWAxWUmYQIHTnDHR8QqYcPegoIAggACAAIGBAC&opi=89978449&cd&psig=AOvVaw3eZ4Hlxm_4fQiMTrWARoRx&ust=1789171319579000"                                                                            ,
  "Uses shelling/grain recovery factor."                                                                                                                                                                                                                                                                                                                                                                                                                                                      ,

  "Maize - shelled/off cob"                                                                                                                                                                                                                                                                                                                                                                                                                                                                   , "Maize"                            , 1.00        , 1.00        , 1.00         ,
  "provisional"                                                                                                                                                                                                                                                                                                                                                                                                                                                                               , "direct grain"                     ,
  "GHS item: Maize - shelled/off cob"                                                                                                                                                                                                                                                                                                                                                                                                                                                         ,
  NA_character_                                                                                                                                                                                                                                                                                                                                                                                                                                                                               ,
  "Direct maize grain."                                                                                                                                                                                                                                                                                                                                                                                                                                                                       ,

  "Yam flour"                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 , "Yam"                              , 3.33        , 3.63        , 4            ,
  "provisional"                                                                                                                                                                                                                                                                                                                                                                                                                                                                               , "yam flour conversion"             ,
  "YouTube recipe for making flour"                                                                                                                                                                                                                                                                                                                                                                                                                                                           ,
  "https://www.google.com/url?sa=i&source=web&rct=j&url=/goto?url%3DCAESYwHrOzAVZUTP58XwFDZod6mRyVmIcjQ6GoaeDmZYcQyomnpy1wrq-k2fLaiu5RWI5dkz_jJLG6MrYywXLunhFPKiEn052A7Bwkr7rjg9xfxIPFgtl9ECpcAGaLDNq1osZQjDfg&ved=2ahUKEwjorOuon-WWAxXRfTABHTXnFv8QqYcPegoIAggACAAIIhAC&opi=89978449&cd&psig=AOvVaw0Zrk8R1FSd2vh1SOUSvEOX&ust=1789172024227000"                                                                                                                                              ,
  "Uses conversion from yam flour to fresh yam equivalent."                                                                                                                                                                                                                                                                                                                                                                                                                                   ,

  "Cassava flour"                                                                                                                                                                                                                                                                                                                                                                                                                                                                             , "Cassava"                          , 3.75        , 4           , 4.25         ,
  "provisional"                                                                                                                                                                                                                                                                                                                                                                                                                                                                               , "cassava flour conversion"         ,
  "J Aristizabal  2017 "                                                                                                                                                                                                                                                                                                                                                                                                                                                                      ,
  "https://www.google.com/url?sa=i&source=web&rct=j&url=/goto?url%3DCAESigEB6zswFUvEIp_eTOLnYnvzde0i8HcEv4OJeq1WhLLblzOYqSyw5D42rQpZpyo2_a4w_MKmO6HplUkj9Y5vDOorkS_7W3waZwHECZZpyZbggtDMrm-IvmH8gj6hugcmLnrrsoLif5AsdFi2Hai-68CigySlygguhSosMcgWLRRp2TdgQAUUVj9xop0&ved=2ahUKEwjHyqW9oOWWAxXAgoQIHX85MMUQqYcPegoIAggACAAIFhAC&opi=89978449&cd&psig=AOvVaw2R1uvRmx7M9PlmXlUonnkT&ust=1789172335560000"                                                                                         ,
  "Uses conversion from cassava flour to appropriate cassava basis."                                                                                                                                                                                                                                                                                                                                                                                                                          ,

  "Cassava roots"                                                                                                                                                                                                                                                                                                                                                                                                                                                                             , "Cassava"                          , 1.00        , 1.00        , 1.00         ,
  "provisional"                                                                                                                                                                                                                                                                                                                                                                                                                                                                               , "direct roots"                     ,
  "GHS item: Cassava roots"                                                                                                                                                                                                                                                                                                                                                                                                                                                                   ,
  NA_character_                                                                                                                                                                                                                                                                                                                                                                                                                                                                               ,
  "Check GHS free-text units for prepared fufu/wrapped products."                                                                                                                                                                                                                                                                                                                                                                                                                             ,

  "Yam roots"                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 , "Yam"                              , 1.00        , 1.00        , 1.00         ,
  "provisional"                                                                                                                                                                                                                                                                                                                                                                                                                                                                               , "direct roots"                     ,
  "GHS item: Yam roots"                                                                                                                                                                                                                                                                                                                                                                                                                                                                       ,
  NA_character_                                                                                                                                                                                                                                                                                                                                                                                                                                                                               ,
  "Direct yam roots."                                                                                                                                                                                                                                                                                                                                                                                                                                                                         ,

  "Gari - white"                                                                                                                                                                                                                                                                                                                                                                                                                                                                              , "Cassava"                          , 3.5         , 4.5         , 6.0          ,
  "provisional"                                                                                                                                                                                                                                                                                                                                                                                                                                                                               , "gari to fresh cassava"            ,
  "FAO — cassava processing literature"                                                                                                                                                                                                                                                                                                                                                                                                                                                       ,
  "https://www.fao.org/4/x5458e/x5458e05.htm"                                                                                                                                                                                                                                                                                                                                                                                                                                                 ,
  "Highly variable processing yield."                                                                                                                                                                                                                                                                                                                                                                                                                                                         ,

  "Gari - yellow"                                                                                                                                                                                                                                                                                                                                                                                                                                                                             , "Cassava"                          , 3.5         , 4.5         , 6.0          ,
  "provisional"                                                                                                                                                                                                                                                                                                                                                                                                                                                                               , "gari to fresh cassava"            ,
  "FAO — cassava processing literature"                                                                                                                                                                                                                                                                                                                                                                                                                                                       ,
  "https://www.fao.org/4/x5458e/x5458e05.htm"                                                                                                                                                                                                                                                                                                                                                                                                                                                 ,
  "Highly variable processing yield."                                                                                                                                                                                                                                                                                                                                                                                                                                                         ,

  "Cocoyam"                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   , "Cocoyam"                          , 1.00        , 1.00        , 1.00         ,
  "provisional"                                                                                                                                                                                                                                                                                                                                                                                                                                                                               , "direct roots"                     ,
  "GHS item: Cocoyam"                                                                                                                                                                                                                                                                                                                                                                                                                                                                         ,
  NA_character_                                                                                                                                                                                                                                                                                                                                                                                                                                                                               ,
  "Direct crop product."                                                                                                                                                                                                                                                                                                                                                                                                                                                                      ,

  "Plantains"                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 , "Plantain"                         , 1.00        , 1.00        , 1.00         ,
  "provisional"                                                                                                                                                                                                                                                                                                                                                                                                                                                                               , "direct crop product"              ,
  "GHS item: Plantains"                                                                                                                                                                                                                                                                                                                                                                                                                                                                       ,
  NA_character_                                                                                                                                                                                                                                                                                                                                                                                                                                                                               ,
  "Direct crop product."                                                                                                                                                                                                                                                                                                                                                                                                                                                                      ,

  "Sweet potatoes"                                                                                                                                                                                                                                                                                                                                                                                                                                                                            , "Sweet potato"                     , 1.00        , 1.00        , 1.00         ,
  "provisional"                                                                                                                                                                                                                                                                                                                                                                                                                                                                               , "direct roots"                     ,
  "GHS item: Sweet potatoes"                                                                                                                                                                                                                                                                                                                                                                                                                                                                  ,
  NA_character_                                                                                                                                                                                                                                                                                                                                                                                                                                                                               ,
  "Direct crop product."                                                                                                                                                                                                                                                                                                                                                                                                                                                                      ,

  "Potatoes"                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  , "Potato"                           , 1.00        , 1.00        , 1.00         ,
  "provisional"                                                                                                                                                                                                                                                                                                                                                                                                                                                                               , "direct roots"                     ,
  "GHS item: Potatoes"                                                                                                                                                                                                                                                                                                                                                                                                                                                                        ,
  NA_character_                                                                                                                                                                                                                                                                                                                                                                                                                                                                               ,
  "Direct crop product."                                                                                                                                                                                                                                                                                                                                                                                                                                                                      ,

  "Soya beans"                                                                                                                                                                                                                                                                                                                                                                                                                                                                                , "Soybean"                          , 1.00        , 1.00        , 1.00         ,
  "provisional"                                                                                                                                                                                                                                                                                                                                                                                                                                                                               , "direct grain"                     ,
  "GHS item: Soya beans"                                                                                                                                                                                                                                                                                                                                                                                                                                                                      ,
  NA_character_                                                                                                                                                                                                                                                                                                                                                                                                                                                                               ,
  "Direct soybean grain."                                                                                                                                                                                                                                                                                                                                                                                                                                                                     ,

  "Brown beans"                                                                                                                                                                                                                                                                                                                                                                                                                                                                               , "Cowpea"                           , 1.00        , 1.00        , 1.00         ,
  "provisional"                                                                                                                                                                                                                                                                                                                                                                                                                                                                               , "direct dry grain"                 ,
  "GHS item: Brown beans"                                                                                                                                                                                                                                                                                                                                                                                                                                                                     ,
  NA_character_                                                                                                                                                                                                                                                                                                                                                                                                                                                                               ,
  "Brown beans treated as cowpea."                                                                                                                                                                                                                                                                                                                                                                                                                                                            ,

  "White beans"                                                                                                                                                                                                                                                                                                                                                                                                                                                                               , "Cowpea"                           , 1.00        , 1.00        , 1.00         ,
  "provisional"                                                                                                                                                                                                                                                                                                                                                                                                                                                                               , "direct dry grain"                 ,
  "GHS item: White beans"                                                                                                                                                                                                                                                                                                                                                                                                                                                                     ,
  NA_character_                                                                                                                                                                                                                                                                                                                                                                                                                                                                               ,
  "White beans treated as cowpea."                                                                                                                                                                                                                                                                                                                                                                                                                                                            ,

  "Groundnuts - unshelled"                                                                                                                                                                                                                                                                                                                                                                                                                                                                    , "Groundnut"                        , 0.65        , 0.7         , 0.75         ,
  "research"                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  , "unshelled to shelled conversion"  ,
  "Nzambani Agrifarms"                                                                                                                                                                                                                                                                                                                                                                                                                                                                        ,
  "https://www.google.com/url?sa=i&source=web&rct=j&url=/goto?url%3DCAESzAEB6zswFe6qsuL50Ak61sGftkrGIMYIVr54zPI-HAofFPziIemt0JWX3POqA6VJZW96ySFklK2p4XVkWSAVHSZ6oO9FAeo95RMDDy0_iBcD1aLk_iKWcrQyzK81oIqzylBdQySItDxcfwfXwg1UxGRJaoULSLdQREL6_XRoq7nlct9Q20drUtZIRwj0N7UNr0ljpxMeekGfpsEJTLdQneHM3XiF_1eB1YqamBIua4jWQCwv33x9iyFs-_Tz31NvpqTCAle01rktfq0T7xY&ved=2ahUKEwid88n7oeWWAxWvTDABHTrLBKMQqYcPegoIAggACAAIHhAC&opi=89978449&cd&psig=AOvVaw0jqGOvF_ai0Vgg6rLT2nQP&ust=1789172734614000" ,
  "Uses shelling/outturn factor."                                                                                                                                                                                                                                                                                                                                                                                                                                                             ,

  "Groundnuts - shelled"                                                                                                                                                                                                                                                                                                                                                                                                                                                                      , "Groundnut"                        , 1.00        , 1.00        , 1.00         ,
  "provisional"                                                                                                                                                                                                                                                                                                                                                                                                                                                                               , "direct kernel"                    ,
  "GHS item: Groundnuts - shelled"                                                                                                                                                                                                                                                                                                                                                                                                                                                            ,
  NA_character_                                                                                                                                                                                                                                                                                                                                                                                                                                                                               ,
  "Direct groundnut kernel."                                                                                                                                                                                                                                                                                                                                                                                                                                                                  ,

  "Palm oil"                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  , "Oil palm"                         , NA          , NA          , NA           ,
  "research"                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  , "palm oil to crop conversion"      ,
  "Further literature required"                                                                                                                                                                                                                                                                                                                                                                                                                                                               ,
  NA_character_                                                                                                                                                                                                                                                                                                                                                                                                                                                                               ,
  "Need to define crop endpoint."                                                                                                                                                                                                                                                                                                                                                                                                                                                             ,

  "Groundnut oil"                                                                                                                                                                                                                                                                                                                                                                                                                                                                             , "Groundnut"                        , NA          , NA          , NA           ,
  "research"                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  , "groundnut oil to crop conversion" ,
  "Further literature required"                                                                                                                                                                                                                                                                                                                                                                                                                                                               ,
  NA_character_                                                                                                                                                                                                                                                                                                                                                                                                                                                                               ,
  "Requires oil extraction rate."                                                                                                                                                                                                                                                                                                                                                                                                                                                             ,

  "Wheat flour"                                                                                                                                                                                                                                                                                                                                                                                                                                                                               , "Wheat"                            , 1.18        , 1.32        , 1.39         ,
  "provisional"                                                                                                                                                                                                                                                                                                                                                                                                                                                                               , "wheat flour extraction"           ,
  "FAO — Cereals"                                                                                                                                                                                                                                                                                                                                                                                                                                                                             ,
  "https://www.fao.org/4/X5557E/x5557e04.htm"                                                                                                                                                                                                                                                                                                                                                                                                                                                 ,
  "Wheat flour extraction approximately 72–85%."                                                                                                                                                                                                                                                                                                                                                                                                                                              ,

  "Bread"                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     , "Wheat"                            , NA          , NA          , NA           ,
  "research"                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  , "bread recipe"                     ,
  "Further literature required"                                                                                                                                                                                                                                                                                                                                                                                                                                                               ,
  NA_character_                                                                                                                                                                                                                                                                                                                                                                                                                                                                               ,
  "Requires wheat flour proportion."                                                                                                                                                                                                                                                                                                                                                                                                                                                          ,

  "Cake"                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      , "Wheat"                            , NA          , NA          , NA           ,
  "research"                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  , "cake recipe"                      ,
  "Further literature required"                                                                                                                                                                                                                                                                                                                                                                                                                                                               ,
  NA_character_                                                                                                                                                                                                                                                                                                                                                                                                                                                                               ,
  "Requires wheat flour proportion."                                                                                                                                                                                                                                                                                                                                                                                                                                                          ,

  "Buns/Pofpof/Donuts"                                                                                                                                                                                                                                                                                                                                                                                                                                                                        , "Wheat"                            , NA          , NA          , NA           ,
  "research"                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  , "recipe conversion"                ,
  "Further literature required"                                                                                                                                                                                                                                                                                                                                                                                                                                                               ,
  NA_character_                                                                                                                                                                                                                                                                                                                                                                                                                                                                               ,
  "Requires wheat flour proportion."                                                                                                                                                                                                                                                                                                                                                                                                                                                          ,

  "Biscuits"                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  , "Wheat"                            , NA          , NA          , NA           ,
  "research"                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  , "recipe conversion"                ,
  "Further literature required"                                                                                                                                                                                                                                                                                                                                                                                                                                                               ,
  NA_character_                                                                                                                                                                                                                                                                                                                                                                                                                                                                               ,
  "Requires wheat flour proportion."                                                                                                                                                                                                                                                                                                                                                                                                                                                          ,

  "Meat pie/Sausage roll"                                                                                                                                                                                                                                                                                                                                                                                                                                                                     , "Wheat"                            , NA          , NA          , NA           ,
  "research"                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  , "recipe conversion"                ,
  "Further literature required"                                                                                                                                                                                                                                                                                                                                                                                                                                                               ,
  NA_character_                                                                                                                                                                                                                                                                                                                                                                                                                                                                               ,
  "Requires wheat flour proportion."                                                                                                                                                                                                                                                                                                                                                                                                                                                          ,

  "Tomato puree - canned"                                                                                                                                                                                                                                                                                                                                                                                                                                                                     , "Tomato"                           , NA          , NA          , NA           ,
  "research"                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  , "tomato puree conversion"          ,
  "Further literature required"                                                                                                                                                                                                                                                                                                                                                                                                                                                               ,
  NA_character_                                                                                                                                                                                                                                                                                                                                                                                                                                                                               ,
  "Requires tomato solids/yield conversion."                                                                                                                                                                                                                                                                                                                                                                                                                                                  ,

  "Sugar"                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     , "Sugar"                            , NA          , NA          , NA           ,
  "research"                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  , "sugar to sugarcane"               ,
  "Further literature required"                                                                                                                                                                                                                                                                                                                                                                                                                                                               ,
  NA_character_                                                                                                                                                                                                                                                                                                                                                                                                                                                                               ,
  "Requires sugar recovery from sugarcane."
)


# ============================================================
# 7. SAVE CONVERSION LOOKUP
# ============================================================
#
# This is useful because the lookup is effectively a research
# worksheet. You can edit/add factors as literature is found.
#
# ============================================================

write_csv(
  conversion_lookup,
  "tab_data/GHS food per capita outputs/conversion_lookup.csv"
)


# ============================================================
# 8. CHECK FOR FOOD ITEMS WITH NO LOOKUP
# ============================================================

state_items <- food_state_results %>%

  distinct(
    product
  )


zone_items <- food_zone_results %>%

  distinct(
    product
  )


# missing_state_lookup <- state_items %>%
#
#   anti_join(
#     conversion_lookup %>%
#       select(item_cd),
#     by = "item_cd"
#   )
#
#
# missing_zone_lookup <- zone_items %>%
#
#   anti_join(
#     conversion_lookup %>%
#       select(item_cd),
#     by = "item_cd"
#   )

# if (nrow(missing_state_lookup) > 0) {
#
#   message(
#     "\nSTATE RESULTS — ITEMS WITHOUT A CONVERSION LOOKUP:\n"
#   )
#
#   print(
#     missing_state_lookup
#   )
#
# }
#
#
# if (nrow(missing_zone_lookup) > 0) {
#
#   message(
#     "\nZONE RESULTS — ITEMS WITHOUT A CONVERSION LOOKUP:\n"
#   )
#
#   print(
#     missing_zone_lookup
#   )
#
# }

# ============================================================
# 9. STATE-LEVEL CONVERSION
# ============================================================

food_state_crop <- food_state_results %>%

  left_join(
    conversion_lookup,
    by = "product"
  ) %>%

  mutate(
    crop_equivalent_low = estimate_kg_person_year * factor_low,

    crop_equivalent_mid = estimate_kg_person_year * factor_mid,

    crop_equivalent_high = estimate_kg_person_year * factor_high
  )


crop_state_results <- food_state_crop %>%

  filter(
    !is.na(crop),
    !is.na(factor_mid)
  ) %>%

  group_by(
    state,
    crop
  ) %>%

  summarise(
    crop_equivalent_low = sum(crop_equivalent_low, na.rm = TRUE),

    crop_equivalent_kg_person_year = sum(crop_equivalent_mid, na.rm = TRUE),

    crop_equivalent_high = sum(crop_equivalent_high, na.rm = TRUE),

    .groups = "drop"
  )

write_csv(
  crop_state_results,
  "tab_data/GHS food per capita outputs/crop_state_results.csv"
)


# ============================================================
# 10. ZONE-LEVEL CONVERSION
# ============================================================

food_zone_crop <- food_zone_results %>%

  left_join(
    conversion_lookup,
    by = "product"
  ) %>%

  mutate(
    crop_equivalent_low = estimate_kg_person_year * factor_low,

    crop_equivalent_mid = estimate_kg_person_year * factor_mid,

    crop_equivalent_high = estimate_kg_person_year * factor_high
  )


crop_zone_results <- food_zone_crop %>%

  filter(
    !is.na(crop),
    !is.na(factor_mid)
  ) %>%

  group_by(
    zone,
    crop
  ) %>%

  summarise(
    crop_equivalent_low = sum(crop_equivalent_low, na.rm = TRUE),

    crop_equivalent_kg_person_year = sum(crop_equivalent_mid, na.rm = TRUE),

    crop_equivalent_high = sum(crop_equivalent_high, na.rm = TRUE),

    .groups = "drop"
  )

write_csv(
  crop_zone_results,
  "tab_data/GHS food per capita outputs/crop_zone_results.csv"
)


# ============================================================
# 11. FINAL CHECKS
# ============================================================

message("\n")
message("============================================================")
message("CROP CONVERSION COMPLETE")
message("============================================================")


message(
  "\nState food-result rows: ",
  nrow(food_state_results)
)


message(
  "State converted rows: ",
  sum(!is.na(food_state_crop$factor_mid))
)


message(
  "State unconverted rows: ",
  sum(is.na(food_state_crop$factor_mid))
)


message(
  "\nZone food-result rows: ",
  nrow(food_zone_results)
)


message(
  "Zone converted rows: ",
  sum(!is.na(food_zone_crop$factor_mid))
)


message(
  "Zone unconverted rows: ",
  sum(is.na(food_zone_crop$factor_mid))
)


message(
  "\nState-level crops produced: ",
  n_distinct(
    crop_state_results$crop
  )
)


message(
  "Zone-level crops produced: ",
  n_distinct(
    crop_zone_results$crop
  )
)


# ============================================================
# 13. DISPLAY STATE RESULTS
# ============================================================

print(
  crop_state_results %>%
    arrange(
      state,
      crop
    )
)


# ============================================================
# 14. DISPLAY ZONE RESULTS
# ============================================================

print(
  crop_zone_results %>%
    arrange(
      zone,
      crop
    )
)

# ============================================================
# END OF SCRIPT
# ============================================================

# ============================================================
# SOURCES
# ============================================================
#
# The conversion factors in this script are based on the
# following sources where a suitable factor has been identified.
#
#
# ------------------------------------------------------------
# 1. FAO — Cassava processing
# ------------------------------------------------------------
#
# FAO.
# "An overview of traditional processing and utilization of
# cassava in Africa."
#
# https://www.fao.org/4/x5458e/x5458e05.htm
#
# Reports traditional fresh-cassava-root to gari yields in the
# approximate range of 15–20%, with substantial variation
# depending on variety, harvest age and environment.
#
# This implies approximately:
#
#     1 kg gari ≈ 5.0–6.7 kg fresh cassava roots
#
# A wider range is retained in the present script because
# Nigerian processing studies show substantial variation.
#
#
# ------------------------------------------------------------
# 2. FAO — Processing of roots and tubers
# ------------------------------------------------------------
#
# FAO.
# "Chapter 5: Processing of Roots and Tubers."
#
# https://www.fao.org/4/x5415e/x5415e05.htm
#
# Provides a detailed cassava-processing example in which
# approximately 22 kg gari is obtained from 100 kg fresh
# unwashed cassava roots.
#
# This corresponds to approximately:
#
#     1 kg gari ≈ 4.55 kg fresh cassava roots
#
#
# ------------------------------------------------------------
# 3. FAO — Root crops and national food balance sheets
# ------------------------------------------------------------
#
# FAO.
# "Quantification of Root Crops in National Food Balance
# Sheets."
#
# https://www.fao.org/4/y9422e/y9422e04.htm
#
# Indicative extraction rates include:
#
#     maize meal       80–95%
#     wheat flour      72–80%
#     sorghum flour    80–95%
#     millet flour     80–95%
#     paddy rice       65–75%
#
# These can be converted to product-to-grain factors by:
#
#     factor = 1 / extraction_rate
#
#
# ------------------------------------------------------------
# 4. FAO — Cereals
# ------------------------------------------------------------
#
# FAO.
# "Cereals."
#
# https://www.fao.org/4/X5557E/x5557e04.htm
#
# Provides cereal processing/extraction information including
# wheat, rice, sorghum and millet.
#
# Examples:
#
#     wheat flour extraction approximately 72–85%
#     sorghum extraction approximately 90%
#     millet extraction approximately 90%
#
#
# ------------------------------------------------------------
# 5. IRRI — Rice milling yields
# ------------------------------------------------------------
#
# International Rice Research Institute.
# "Milling yields."
#
# https://www.knowledgebank.irri.org/step-by-step-production/
# postharvest/milling/producing-good-quality-milled-rice/
# milling-yields
#
# Reported recovery varies with milling system:
#
#     laboratory            68–72%
#     single-stage mill     50–55%
#     modern multi-stage    65–70%
#
# Therefore, if a paddy-equivalent estimate is required:
#
#     paddy equivalent =
#         milled rice / milling recovery
#
# The current script does NOT apply this factor because the
# GHS item is treated as rice as consumed.
#
#
# ------------------------------------------------------------
# 6. Nigerian gari-processing literature
# ------------------------------------------------------------
#
# Recent research demonstrates considerable variation in the
# quantity of fresh cassava required to produce gari.
#
# One recent study reports fresh-root requirements ranging
# approximately from 5.1 to 12.9 tonnes fresh roots per tonne
# gari depending on processing location/technology.
#
# DOI:
#
# https://doi.org/10.1002/jsfa.12936
#
# This is why the gari factor in this script is marked
# "provisional" rather than definitive.
#
#
# ============================================================
# IMPORTANT: CROP EQUIVALENT ≠ GRAIN EQUIVALENT
# ============================================================
#
# A separate Nigerian/FAO "grain equivalent" methodology gives
# factors such as:
#
#     maize          1.00
#     millet         0.93
#     sorghum        0.96
#     rice           1.00
#     wheat          0.92
#     cassava        0.30
#     sweet potato   0.30
#     yam            0.25
#     plantain       0.21
#     beans          0.96
#
# These factors should NOT be inserted into this script.
#
# They are intended to standardise different commodities to a
# common grain/energy-equivalent basis.
#
# They do not represent physical processing conversions.
#
# For example:
#
#     cassava factor = 0.30
#
# does NOT mean:
#
#     1 kg gari = 0.30 kg cassava
#
# The current analysis instead asks:
#
#     "How much of the underlying crop is represented by the
#      quantity of food consumed?"
#
# This is the more appropriate approach for crop-specific
# comparisons between Nigerian states and zones.
#
#
# ============================================================
# ITEMS STILL REQUIRING RESEARCH
# ============================================================
#
# The following products deliberately have NA conversion
# factors until an appropriate source/method is identified:
#
#     20  Maize on cob
#     17  Yam flour
#     18  Cassava flour
#     25  Bread
#     26  Cake
#     27  Buns/Pofpof/Donuts
#     28  Biscuits
#     29  Meat pie/Sausage roll
#     43  Unshelled groundnuts
#     50  Palm oil
#     52  Groundnut oil
#     71  Tomato puree
#     130 Sugar
#
# These should be researched individually rather than assigning
# arbitrary factors.
#
# Composite foods such as bread, cake and biscuits require a
# recipe/composition approach because the entire consumed weight
# is not wheat.
#
# ============================================================
