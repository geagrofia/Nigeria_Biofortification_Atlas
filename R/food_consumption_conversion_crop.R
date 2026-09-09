# Nigeria GHS Wave 5

# Food product -> crop equivalent conversion factors

#

# PURPOSE

# -------

# Convert GHS Wave 5 food-product consumption estimates

# (kg/person/year) into crop-equivalent consumption estimates.

#

# The primary purpose is COMPARISON BETWEEN NIGERIAN STATES/ZONES,

# rather than precise estimation of national crop requirements.

#

# IMPORTANT:

# The conversion factors below are NOT all equally well established.

# They are therefore classified as:

#

# "direct"       = no processing conversion required

# "provisional"  = literature provides a reasonable basis, but

# factor needs checking for the Nigerian context

# "research"     = factor still needs to be established

# "exclude"      = no meaningful crop equivalent assigned

#

# The analysis should retain the original food-product consumption

# alongside the crop-equivalent estimate.

library(tidyverse)

# -------------------------------------------------------------------

# 1. CONCEPTUAL APPROACH

# -------------------------------------------------------------------

# We distinguish between:

#

# A. DIRECT CROP PRODUCTS

#

# Example:

# Cassava roots -> cassava

# Yam roots     -> yam

# Millet        -> millet

#

# factor = 1.00

#

#

# B. PROCESSED PRODUCTS

#

# Example:

# Gari -> cassava

# Wheat flour -> wheat

# Bread -> wheat

#

# factor = kg crop required per kg food product

#

#

# C. PRODUCTS WITH NO CLEAR SINGLE CROP EQUIVALENT

#

# Example:

# meat

# fish

# milk

# soft drinks

#

# crop = NA

#

#

# D. PRODUCTS FOR WHICH MORE RESEARCH IS REQUIRED

#

# We retain them in the lookup but leave factor = NA until

# a defensible factor is identified.

#

#

# IMPORTANT METHODOLOGICAL CHOICE

# --------------------------------

# The PRIMARY analysis should NOT convert all crops to a common

# "grain equivalent".

#

# Instead:

#

# maize products  -> maize equivalent

# cassava products -> cassava equivalent

# wheat products  -> wheat equivalent

# cowpea products -> cowpea equivalent

#

# This is more appropriate when the objective is comparing

# consumption patterns between Nigerian states/zones.

#

# A separate FAO/NISER grain-equivalent analysis could be performed

# later as a sensitivity/alternative standardisation.

# -------------------------------------------------------------------

# 2. FOOD PRODUCT CLASSIFICATION

# -------------------------------------------------------------------

crop_lookup <- tribble(
  ~item_cd , ~product                     , ~crop          , ~conversion_type ,

        10 , "Guinea corn / sorghum"      , "Sorghum"      , "direct"         ,
        11 , "Millet"                     , "Millet"       , "direct"         ,
        13 , "Rice - local"               , "Rice"         , "direct"         ,
        14 , "Rice - imported"            , "Rice"         , "direct"         ,

        16 , "Maize flour"                , "Maize"        , "provisional"    ,
        17 , "Yam flour"                  , "Yam"          , "research"       ,
        18 , "Cassava flour"              , "Cassava"      , "research"       ,
        19 , "Wheat flour"                , "Wheat"        , "provisional"    ,

        20 , "Maize - unshelled/on cob"   , "Maize"        , "research"       ,
        22 , "Maize - shelled/off cob"    , "Maize"        , "direct"         ,
        23 , "Other grains/flour"         , NA_character_  , "exclude"        ,

        25 , "Bread"                      , "Wheat"        , "research"       ,
        26 , "Cake"                       , "Wheat"        , "research"       ,
        27 , "Buns/Pofpof/Donuts"         , "Wheat"        , "research"       ,
        28 , "Biscuits"                   , "Wheat"        , "research"       ,
        29 , "Meat pie/Sausage roll"      , "Wheat"        , "research"       ,

        30 , "Cassava roots"              , "Cassava"      , "direct"         ,
        31 , "Yam roots"                  , "Yam"          , "direct"         ,
        32 , "Gari - white"               , "Cassava"      , "provisional"    ,
        33 , "Gari - yellow"              , "Cassava"      , "provisional"    ,
        34 , "Cocoyam"                    , "Cocoyam"      , "direct"         ,
        35 , "Plantains"                  , "Plantain"     , "direct"         ,
        36 , "Sweet potatoes"             , "Sweet potato" , "direct"         ,
        37 , "Potatoes"                   , "Potato"       , "direct"         ,
        38 , "Other roots/tubers"         , NA_character_  , "exclude"        ,

        40 , "Soya beans"                 , "Soybean"      , "direct"         ,
        41 , "Brown beans"                , "Cowpea"       , "direct"         ,
        42 , "White beans"                , "Cowpea"       , "direct"         ,
        43 , "Groundnuts - unshelled"     , "Groundnut"    , "research"       ,
        44 , "Groundnuts - shelled"       , "Groundnut"    , "direct"         ,
        45 , "Other nuts/seeds/pulses"    , NA_character_  , "exclude"        ,
        46 , "Coconut"                    , "Coconut"      , "direct"         ,
        47 , "Kola nut"                   , NA_character_  , "exclude"        ,
        48 , "Cashew nut"                 , NA_character_  , "exclude"        ,

        50 , "Palm oil"                   , "Oil palm"     , "research"       ,
        51 , "Butter/Margarine"           , NA_character_  , "exclude"        ,
        52 , "Groundnut oil"              , "Groundnut"    , "research"       ,
        53 , "Other oil/fat"              , NA_character_  , "exclude"        ,
        54 , "Shea butter"                , NA_character_  , "exclude"        ,
        56 , "Animal fat"                 , NA_character_  , "exclude"        ,

        60 , "Bananas"                    , "Banana"       , "direct"         ,
        61 , "Orange/Tangerine"           , NA_character_  , "exclude"        ,
        62 , "Mangoes"                    , NA_character_  , "exclude"        ,
        63 , "Avocado pear"               , NA_character_  , "exclude"        ,
        64 , "Pineapples"                 , NA_character_  , "exclude"        ,
        66 , "Other fruits"               , NA_character_  , "exclude"        ,
        67 , "Pawpaw"                     , NA_character_  , "exclude"        ,
        68 , "Watermelon"                 , NA_character_  , "exclude"        ,
        69 , "Apples"                     , NA_character_  , "exclude"        ,

        70 , "Tomatoes"                   , NA_character_  , "exclude"        ,
        71 , "Tomato puree - canned"      , "Tomato"       , "research"       ,
        72 , "Onions"                     , NA_character_  , "exclude"        ,
        73 , "Garden eggs/Eggplant"       , NA_character_  , "exclude"        ,
        74 , "Okra - fresh"               , NA_character_  , "exclude"        ,
        75 , "Okra - dried"               , NA_character_  , "exclude"        ,
        76 , "Fresh pepper"               , NA_character_  , "exclude"        ,
        77 , "Dry pepper"                 , NA_character_  , "exclude"        ,
        78 , "Leaves"                     , NA_character_  , "exclude"        ,
        79 , "Other vegetables"           , NA_character_  , "exclude"        ,

        80 , "Chicken"                    , NA_character_  , "exclude"        ,
        81 , "Duck"                       , NA_character_  , "exclude"        ,
        82 , "Other domestic poultry"     , NA_character_  , "exclude"        ,
        83 , "Agricultural eggs"          , NA_character_  , "exclude"        ,
        84 , "Local eggs"                 , NA_character_  , "exclude"        ,
        85 , "Other eggs"                 , NA_character_  , "exclude"        ,

        90 , "Beef"                       , NA_character_  , "exclude"        ,
        91 , "Mutton"                     , NA_character_  , "exclude"        ,
        92 , "Pork"                       , NA_character_  , "exclude"        ,
        93 , "Goat"                       , NA_character_  , "exclude"        ,
        94 , "Wild game/Bush meat"        , NA_character_  , "exclude"        ,
        95 , "Canned beef"                , NA_character_  , "exclude"        ,
        96 , "Other meat"                 , NA_character_  , "exclude"        ,

       100 , "Fish - fresh"               , NA_character_  , "exclude"        ,
       101 , "Fish - frozen"              , NA_character_  , "exclude"        ,
       102 , "Fish - smoked"              , NA_character_  , "exclude"        ,
       103 , "Fish - dried"               , NA_character_  , "exclude"        ,
       104 , "Snails"                     , NA_character_  , "exclude"        ,
       105 , "Seafood"                    , NA_character_  , "exclude"        ,
       106 , "Canned fish/seafood"        , NA_character_  , "exclude"        ,
       107 , "Other fish/seafood"         , NA_character_  , "exclude"        ,

       110 , "Fresh milk"                 , NA_character_  , "exclude"        ,
       111 , "Milk powder"                , NA_character_  , "exclude"        ,
       112 , "Baby milk powder"           , NA_character_  , "exclude"        ,
       113 , "Milk - tinned"              , NA_character_  , "exclude"        ,
       114 , "Cheese/wara"                , NA_character_  , "exclude"        ,
       115 , "Other milk products"        , NA_character_  , "exclude"        ,

       120 , "Coffee"                     , NA_character_  , "exclude"        ,
       121 , "Chocolate drinks"           , NA_character_  , "exclude"        ,
       122 , "Tea"                        , NA_character_  , "exclude"        ,

       130 , "Sugar"                      , "Sugar"        , "direct"         ,
       132 , "Honey"                      , NA_character_  , "exclude"        ,
       133 , "Other sweets"               , NA_character_  , "exclude"        ,

       141 , "Salt"                       , NA_character_  , "exclude"        ,
       142 , "Ogbono - unground"          , NA_character_  , "exclude"        ,
       143 , "Ogbono - ground"            , NA_character_  , "exclude"        ,
       144 , "Ground pepper"              , NA_character_  , "exclude"        ,
       145 , "Melon - shelled"            , NA_character_  , "exclude"        ,
       146 , "Melon - unshelled"          , NA_character_  , "exclude"        ,
       147 , "Melon - ground"             , NA_character_  , "exclude"        ,
       148 , "Other spices"               , NA_character_  , "exclude"        ,

       150 , "Bottled water"              , NA_character_  , "exclude"        ,
       151 , "Sachet water"               , NA_character_  , "exclude"        ,
       152 , "Malt drinks"                , NA_character_  , "exclude"        ,
       153 , "Soft drinks"                , NA_character_  , "exclude"        ,
       154 , "Fruit juice"                , NA_character_  , "exclude"        ,
       155 , "Other non-alcoholic drinks" , NA_character_  , "exclude"        ,

       160 , "Beer"                       , NA_character_  , "exclude"        ,
       161 , "Palm wine"                  , NA_character_  , "exclude"        ,
       162 , "Pito"                       , NA_character_  , "exclude"        ,
       163 , "Gin"                        , NA_character_  , "exclude"        ,
       164 , "Other alcoholic beverages"  , NA_character_  , "exclude"        ,

       601 , "Guava"                      , NA_character_  , "exclude"
)

# -------------------------------------------------------------------

# 3. CURRENT CONVERSION FACTORS

# -------------------------------------------------------------------

# factor = kg of underlying crop required per kg of reported

# food product.

#

# factor_low / factor_mid / factor_high are included from the outset

# because sensitivity analysis is important for this application.

#

# IMPORTANT:

# NA means that the factor still needs investigation.

#

# "direct" products generally use factor = 1.

#

# The factors below should be treated as WORKING values rather than

# final values until the underlying source/context has been checked.

conversion_lookup <- tribble(
  ~product                            , ~crop                              , ~factor_low   , ~factor_mid , ~factor_high , ~status , ~basis , ~source ,

  "Guinea corn / sorghum"             , "Sorghum"                          , 1.00          , 1.00        , 1.00         ,
  "direct"                            , "reported grain product"           , "GHS product" ,

  "Millet"                            , "Millet"                           , 1.00          , 1.00        , 1.00         ,
  "direct"                            , "reported grain product"           , "GHS product" ,

  "Rice - local"                      , "Rice"                             , 1.00          , 1.00        , 1.00         ,
  "direct"                            , "milled rice equivalent"           , "GHS product" ,

  "Rice - imported"                   , "Rice"                             , 1.00          , 1.00        , 1.00         ,
  "direct"                            , "milled rice equivalent"           , "GHS product" ,

  "Maize flour"                       , "Maize"                            , 1.05          , 1.14        , 1.25         ,
  "provisional"                       , "kg maize grain / kg maize flour"  ,
  "FAO cereal extraction literature"  ,

  "Maize - shelled/off cob"           , "Maize"                            , 1.00          , 1.00        , 1.00         ,
  "direct"                            , "reported shelled grain"           , "GHS product" ,

  "Wheat flour"                       , "Wheat"                            , 1.18          , 1.32        , 1.39         ,
  "provisional"                       , "kg wheat grain / kg wheat flour"  ,
  "FAO cereal extraction literature"  ,

  "Cassava roots"                     , "Cassava"                          , 1.00          , 1.00        , 1.00         ,
  "direct"                            , "fresh cassava roots"              , "GHS product" ,

  "Gari - white"                      , "Cassava"                          , 3.5           , 4.5         , 6.0          ,
  "provisional"                       , "kg fresh cassava roots / kg gari" ,
  "FAO cassava processing literature" ,

  "Gari - yellow"                     , "Cassava"                          , 3.5           , 4.5         , 6.0          ,
  "provisional"                       , "kg fresh cassava roots / kg gari" ,
  "FAO cassava processing literature" ,

  "Brown beans"                       , "Cowpea"                           , 1.00          , 1.00        , 1.00         ,
  "direct"                            , "dry cowpea grain"                 , "GHS product" ,

  "White beans"                       , "Cowpea"                           , 1.00          , 1.00        , 1.00         ,
  "direct"                            , "dry cowpea grain"                 , "GHS product" ,

  "Plantains"                         , "Plantain"                         , 1.00          , 1.00        , 1.00         ,
  "direct"                            , "reported plantain product"        , "GHS product" ,

  "Bananas"                           , "Banana"                           , 1.00          , 1.00        , 1.00         ,
  "direct"                            , "reported banana product"          , "GHS product" ,

  "Sweet potatoes"                    , "Sweet potato"                     , 1.00          , 1.00        , 1.00         ,
  "direct"                            , "fresh sweet potato"               , "GHS product"
)

# -------------------------------------------------------------------

# 4. LITERATURE NOTES

# -------------------------------------------------------------------

# CASSAVA / GARI

# ----------------

#

# FAO:

#

# Traditional gari conversion rate from fresh cassava roots:

# approximately 15-20%.

#

# This corresponds to approximately:

#

# 1 kg gari = 5.0-6.7 kg fresh roots

#

# FAO's detailed processing example gives:

#

# 100 kg fresh unwashed roots

# -> 22 kg residual gari

#

# equivalent to:

#

# 1 kg gari = 4.55 kg fresh roots

#

# The difference illustrates the considerable variability caused

# by variety, moisture, peeling, pressing, fermentation and drying.

#

# Sources:

#

# FAO, "An overview of traditional processing and utilization of

# cassava in Africa"

# https://www.fao.org/4/x5458e/x5458e05.htm

#

# FAO, "Roots, tubers, plantains and bananas in human nutrition",

# Chapter 5: Processing of roots and tubers

# https://www.fao.org/4/x5415e/x5415e05.htm

#

# WORKING VALUE:

#

# gari -> cassava = 4.5

#

# Sensitivity range currently proposed:

#

# 3.5 - 6.0

#

# NOTE:

# This should be checked against Nigerian gari-processing studies

# before being considered final.

# WHEAT FLOUR

# -----------

#

# FAO cereal tables give wheat extraction rates approximately:

#

# medium extraction flour = 85%

# white/low extraction flour = 72%

#

# Thus:

#

# 1 kg flour = 1 / extraction rate kg wheat

#

# approximately:

#

# 1.18 kg wheat at 85% extraction

# 1.39 kg wheat at 72% extraction

#

# Working central value:

#

# 1 / 0.76 ~= 1.32

#

# Source:

#

# FAO, Cereals

# https://www.fao.org/4/X5557E/x5557e04.htm

#

# IMPORTANT:

# The actual GHS wheat flour product should ideally be matched to

# the type of flour represented.

# MAIZE FLOUR

# -----------

#

# FAO cereal processing literature provides maize extraction rates

# that vary substantially by product and milling method.

#

# A working value of approximately 1.14 kg maize grain per kg flour

# is currently proposed, with a provisional range of 1.05-1.25.

#

# This needs checking against Nigerian maize flour/meal processing

# literature.

# SORGHUM

# -------

#

# The GHS product "Guinea corn / sorghum" is treated as grain,

# therefore factor = 1.00.

#

# If a future GHS product represents sorghum flour, FAO literature

# indicates extraction around 90% in one cereal table:

#

# 1 kg sorghum flour ~= 1.11 kg grain

#

# Source:

#

# FAO, Cereals

# https://www.fao.org/4/X5557E/x5557e04.htm

# MILLET

# ------

#

# GHS "Millet" is treated as grain:

#

# factor = 1.00

#

# FAO cereal tables give approximately 90% extraction for unspecified

# millet flour.

#

# Therefore, if a future processed millet product is encountered:

#

# 1 kg millet flour ~= 1.11 kg millet grain

#

# Source:

#

# FAO, Cereals

# https://www.fao.org/4/X5557E/x5557e04.htm

# RICE

# ----

#

# The current GHS products are:

#

# Rice - local

# Rice - imported

#

# They are treated as consumed milled rice, so:

#

# factor = 1.00

#

# This means the resulting indicator is:

#

# kg milled-rice equivalent/person/year

#

# NOT:

#

# kg paddy/person/year

#

# If paddy-equivalent consumption is required later, IRRI gives

# typical milling recoveries:

#

# laboratory:       68-72%

# modern mill:      65-70%

# village mill:     50-55%

#

# Thus a central modern-milling assumption of 67.5% would give:

#

# 1 kg milled rice ~= 1.48 kg paddy

#

# Source:

#

# IRRI Rice Knowledge Bank, Milling yields

# https://www.knowledgebank.irri.org/step-by-step-production/postharvest/milling/producing-good-quality-milled-rice/milling-yields

#

# Source:

#

# IRRI Rice Knowledge Bank, Milling

# https://www.knowledgebank.irri.org/step-by-step-production/postharvest/milling

# COWPEA

# ------

#

# GHS:

#

# Brown beans

# White beans

#

# are provisionally grouped as:

#

# Cowpea

#

# Since the reported product is dry beans rather than bean flour,

# no processing conversion is currently applied:

#

# factor = 1.00

#

# If the GHS categories prove to include mixtures of bean species,

# this classification should be revisited.

#

# Cowpea flour is different: published processing studies report

# flour yields around 86-90%, but this is NOT relevant to the

# current brown/white bean GHS products.

# PLANTAIN

# --------

#

# Plantains are treated as a direct product:

#

# factor = 1.00

#

# Some food-composition sources report an edible portion of roughly

# 65% after removing peel.

#

# DO NOT automatically apply this to the GHS data.

#

# The GHS conversion factor already converts the respondent's

# reported unit into kg. Applying an additional edible-portion

# factor could double-count the adjustment.

#

# The exact definition of the GHS plantain quantity should therefore

# be established before applying an edible-portion correction.

# BANANA

# ------

#

# Treated as direct product:

#

# factor = 1.00

#

# As with plantain, no additional edible-portion adjustment should

# be made until the GHS unit/conversion definition is established.

# SWEET POTATO

# ------------

#

# Treated as direct crop product:

#

# factor = 1.00

#

# No grain-equivalent conversion is applied in the primary analysis.

# -------------------------------------------------------------------

# 5. IMPORTANT UNRESOLVED PRODUCTS

# -------------------------------------------------------------------

# The following require further research:

#

# 1. Maize - unshelled/on cob

#

# Need conversion from reported kg of whole cob to maize grain.

#

# 2. Cassava flour

#

# Need to establish whether the desired endpoint is:

#

# dry cassava equivalent

# OR

# fresh cassava-root equivalent.

#

# These are not interchangeable without a moisture/dry-matter

# assumption.

#

# 3. Yam flour

#

# Need yam-root -> flour conversion.

#

# 4. Bread

#

# Need wheat-grain -> bread conversion.

# This should account for water added during bread production.

#

# 5. Cake

#

# Need wheat contribution per kg cake.

# Cake contains substantial non-wheat ingredients.

#

# 6. Buns/Pofpof/Donuts

#

# Need wheat contribution per kg product.

#

# 7. Biscuits

#

# Need wheat contribution per kg product.

#

# 8. Meat pie/Sausage roll

#

# Need wheat contribution per kg product.

#

# 9. Groundnuts - unshelled

#

# Need shelling conversion.

#

# 10. Palm oil

#

# Need to decide whether the endpoint should be:

#

# oil-palm fruit equivalent

# palm kernel equivalent

# or simply "palm oil equivalent".

#

# 11. Groundnut oil

#

# Need oil extraction factor:

#

# kg groundnut -> kg oil

#

# 12. Other grains/flour

# 13. Other roots/tubers

# 14. Other nuts/seeds/pulses

#

# Should not be allocated to a particular crop without additional

# information.

# -------------------------------------------------------------------

# 6. WHY WE ARE NOT USING THE NISER/FAO GRAIN-EQUIVALENT FACTORS

# -------------------------------------------------------------------

# A Nigerian literature source reproduces an NISER/FAO grain-equivalent

# conversion table approximately as follows:

#

# Maize          1.00

# Millet         0.93

# Sorghum        0.96

# Rice           1.00

# Wheat          0.92

# Cassava        0.30

# Sweet potato   0.30

# Plantain       0.21

# Beans          0.96

# Groundnut      1.51

#

# These factors are intended to express commodities in a common

# grain-equivalent basis, often using food-energy values.

#

# They should NOT be confused with processing conversion factors.

#

# For example:

#

# cassava factor = 0.30

#

# does NOT mean:

#

# 1 kg cassava flour = 0.30 kg cassava roots

#

# It means that cassava is being expressed relative to a selected

# grain-equivalent/base commodity.

#

# Therefore these factors should only be used in a separate

# sensitivity/alternative standardisation analysis.

#

# SOURCE TO INVESTIGATE:

#

# Nigerian literature reproducing NISER/FAO conversion factors.

#

# The relevant table was identified in:

#

# Emmanuel Ada Ojoko, PhD thesis,

# Federal University / Nigerian agricultural research literature.

#

# This source should be independently checked and preferably replaced

# with the original NISER/FAO source if it can be located.

# -------------------------------------------------------------------

# 7. SUGGESTED DATA STRUCTURE FOR THE FINAL ANALYSIS

# -------------------------------------------------------------------

# Keep the original food-product estimate:

#

# kg_person_year

#

# Add:

#

# crop

# conversion_factor

# crop_equivalent_kg_person_year

#

# Thus:

#

# crop_equivalent =

# food_product_consumption * conversion_factor

#

#

# Example:

#

# 50 kg gari/person/year

# factor = 4.5

#

# = 225 kg fresh cassava equivalent/person/year

# -------------------------------------------------------------------

# 8. JOIN TO THE EXISTING FOOD RESULTS

# -------------------------------------------------------------------

# Assuming your existing results are called `food_results`

# and contain:

#

# state

# zone

# item_cd

# product

# kg_person_year

#

# then:

food_crop_results <- food_results %>%
  left_join(
    crop_lookup,
    by = c("item_cd", "product")
  ) %>%
  left_join(
    conversion_lookup %>%
      select(
        product,
        crop,
        factor_low,
        factor_mid,
        factor_high,
        status,
        basis,
        source
      ),
    by = c("product", "crop")
  ) %>%
  mutate(
    crop_equivalent_low = kg_person_year * factor_low,
    crop_equivalent_mid = kg_person_year * factor_mid,

    crop_equivalent_high = kg_person_year * factor_high
  )

# -------------------------------------------------------------------

# 9. STATE-LEVEL CROP RESULTS

# -------------------------------------------------------------------

crop_state_results <- food_crop_results %>%
  filter(!is.na(crop)) %>%
  group_by(state, crop) %>%
  summarise(
    food_kg_person_year = sum(kg_person_year, na.rm = TRUE),

    crop_equivalent_low = sum(crop_equivalent_low, na.rm = TRUE),

    crop_equivalent_mid = sum(crop_equivalent_mid, na.rm = TRUE),

    crop_equivalent_high = sum(crop_equivalent_high, na.rm = TRUE),

    .groups = "drop"
  )

# -------------------------------------------------------------------

# 10. ZONE-LEVEL CROP RESULTS

# -------------------------------------------------------------------

crop_zone_results <- food_crop_results %>%
  filter(!is.na(crop)) %>%
  group_by(zone, crop) %>%
  summarise(
    food_kg_person_year = sum(kg_person_year, na.rm = TRUE),

    crop_equivalent_low = sum(crop_equivalent_low, na.rm = TRUE),

    crop_equivalent_mid = sum(crop_equivalent_mid, na.rm = TRUE),

    crop_equivalent_high = sum(crop_equivalent_high, na.rm = TRUE),

    .groups = "drop"
  )

# -------------------------------------------------------------------

# 11. DIAGNOSTIC: PRODUCTS STILL NEEDING A FACTOR

# -------------------------------------------------------------------

products_missing_factors <- food_crop_results %>%
  filter(
    !is.na(crop),
    is.na(factor_mid)
  ) %>%
  distinct(
    item_cd,
    product,
    crop,
    conversion_type,
    status,
    basis
  ) %>%
  arrange(crop, product)

products_missing_factors

# -------------------------------------------------------------------

# 12. DIAGNOSTIC: PRODUCTS EXCLUDED FROM CROP ANALYSIS

# -------------------------------------------------------------------

excluded_products <- crop_lookup %>%
  filter(conversion_type == "exclude") %>%
  select(item_cd, product)

excluded_products

# -------------------------------------------------------------------

# 13. RECOMMENDED SENSITIVITY ANALYSIS

# -------------------------------------------------------------------

# Once the missing factors have been researched, calculate:

#

# LOW

# CENTRAL

# HIGH

#

# crop-equivalent estimates.

#

# The key question is NOT:

#

# "What is the exact amount of cassava consumed?"

#

# but:

#

# "Does the relative ranking/difference between Nigerian

# states/zones remain similar across plausible conversion

# assumptions?"

#

# For example:

#

# State A: 180 / 220 / 270 kg cassava equivalent

# State B: 150 / 190 / 235 kg cassava equivalent

#

# If A remains above B throughout the plausible range,

# the state comparison is robust.

#

# If the ordering changes, the result should be flagged as

# sensitive to the processing conversion.

# -------------------------------------------------------------------

# 14. SOURCE LIST

# -------------------------------------------------------------------

# FAO – An overview of traditional processing and utilization

# of cassava in Africa

#

# https://www.fao.org/4/x5458e/x5458e05.htm

#

# Key information:

# Traditional fresh-root -> gari conversion approximately 15-20%.

# FAO – Roots, tubers, plantains and bananas in human nutrition

# Chapter 5: Processing of roots and tubers

#

# https://www.fao.org/4/x5415e/x5415e05.htm

#

# Key information:

# Detailed gari processing example:

# 100 kg fresh unwashed roots -> 22 kg gari.

# FAO – Cereals

#

# https://www.fao.org/4/X5557E/x5557e04.htm

#

# Key information:

# Wheat extraction:

# approximately 72% for white low-extraction flour,

# approximately 85% for medium-extraction flour.

#

# Sorghum:

# approximately 90% extraction.

#

# Millet:

# approximately 90% extraction.

# FAO – Technical Conversion Factors

#

# https://www.fao.org/fileadmin/templates/ess/documents/methodology/tcf.pdf

#

# Contains country-specific technical conversion factors for

# crops and derived products, including cereal flour, pulses,

# groundnuts and oils.

#

# This should be searched for Nigeria-specific values and used

# where appropriate rather than relying only on generic factors.

# IRRI Rice Knowledge Bank – Milling yields

#

# https://www.knowledgebank.irri.org/step-by-step-production/postharvest/milling/producing-good-quality-milled-rice/milling-yields

#

# Key information:

#

# Laboratory milling recovery: 68-72%

# Modern multi-stage mill: 65-70%

# Village single-stage mill: 50-55%

# FAO – Quantification of Root Crops in National Food Balance

# Sheets and Problems Encountered

#

# https://www.fao.org/4/y9422e/y9422e04.htm

#

# Important methodological discussion:

# processed root crops should first be converted to a base

# commodity, but different commodities may subsequently be

# converted to a common grain/energy equivalent for food-balance

# comparisons.

#

# This supports our decision to distinguish PROCESSING CONVERSION

# from GRAIN-EQUIVALENT CONVERSION.

# -------------------------------------------------------------------

# 15. RESEARCH PRIORITIES

# -------------------------------------------------------------------

# HIGHEST PRIORITY

#

# 1. Gari -> fresh cassava roots

# 2. Cassava flour -> cassava roots

# 3. Maize on cob -> maize grain

# 4. Wheat flour -> wheat grain

# 5. Bread -> wheat grain

# 6. Groundnut in shell -> shelled groundnut

#

#

# SECOND PRIORITY

#

# 7. Cake -> wheat

# 8. Buns/Pofpof/Donuts -> wheat

# 9. Biscuits -> wheat

# 10. Meat pie/Sausage roll -> wheat

# 11. Yam flour -> yam

# 12. Groundnut oil -> groundnut

# 13. Palm oil -> oil-palm crop equivalent

#

#

# LOW PRIORITY FOR THE CURRENT OBJECTIVE

#

# Banana, plantain, millet, sorghum, cowpea, sweet potato,

# rice and direct maize grain products can initially remain

# at factor = 1.00 because the GHS product itself is already

# the crop/commodity we want to compare.

#

#

# FINAL PRINCIPLE

# ---------------

#

# Use the simplest defensible conversion that is consistent

# across all states and zones.

#

# Avoid introducing additional corrections (such as edible

# portion or moisture corrections) unless we can establish

# exactly what quantity the GHS conversion factor represents.

#

# This minimizes the risk of introducing artificial differences

# between Nigerian states through the conversion methodology.
