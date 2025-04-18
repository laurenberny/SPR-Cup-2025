######################################
#### CLEANING WAVE 4 & 7 DATASETS ####
######################################
library(here)
library(rio)
library(tidyverse)
raw_4 <- import(here("data/36498-4001-Data.rda"))
raw_4_weights <- import(here("data/36498-4321-Data.rda"))
raw_7 <- import(here("data/36498-7001-Data.rda"))
raw_7_weights <- import(here("data/36498-7331-Data.rda"))

#### CREATE NEW LABELS ####
lab_4 <- c(
 #new name = old name
 "id" = "PERSONID",
 "soc_pos" = "R04_AM0040",
 "region" = "R04X_CB_REGION",
 "ethnicity" = "R04R_A_HISP_IMP",
 "race" = "R04R_A_RACECAT3_IMP",
 "sexuality" = "R04R_A_SEXORIENT2",
 "sex" = "R04R_A_SEX",
 "age" = "R04R_A_AGECAT6",
 "eprod_30" = "R04R_A_P30D_EPRODS",
 "eprod_every_day" = "R04R_A_EDY_EPRODS",
 "eprod_some_days" = "R04R_A_SDY_EPRODS",
 "eprod_no_threshold" = "R04R_A_CUR_EDSD_EPRODS",
 "eprod_established" = "R04R_A_CUR_ESTD_EPRODS",
 "var_strat" = "VARSTRAT",
 "var_psu" = "VARPSU",
 "weight" = "R04_A_C04WGT"
)

lab_7 <- c(
 #new name = old name
 "id" = "PERSONID",
 "soc_pos" = "R07_AM0040",
 "region" = "R07X_CB_REGION",
 "ethnicity" = "R07R_A_HISP_IMP",
 "race" = "R07R_A_RACECAT3_IMP",
 "sexuality" = "R07R_A_SEXORIENT2",
 "sex" = "R07R_A_SEX_IMP",
 "age" = "R07R_A_AGECAT6",
 "eprod_30" = "R07R_A_P30D_EPRODS",
 "eprod_every_day" = "R07R_A_EDY_EPRODS",
 "eprod_some_days" = "R07R_A_SDY_EPRODS",
 "eprod_no_threshold" = "R07R_A_CUR_EDSD_EPRODS",
 "eprod_established" = "R07R_A_CUR_ESTD_EPRODS",
 "var_strat" = "VARSTRAT",
 "var_psu" = "VARPSU",
 "weight" = 	"R07_A_C07WGT"
)
#replicate weights rename rep_weight_#
#starts_with() followed by rename

#### JOIN AND PARE DOWN ####
## wave 4 dataset ##
#join weights and rename columns
wave_4 <- raw_4 %>% 
 left_join(raw_4_weights, by = "PERSONID") %>% 
 select(all_of(lab_4), starts_with("R04_A_C04WGT")) %>% 
 rename_with(
  .cols = starts_with("R04_A_C04WGT"),
  .fn = ~ str_replace(., "R04_A_C04WGT(\\d+)", "rep_weight_\\1")
 ) %>% 
 #add time cols
 mutate(wave = 4,
        period = 0)

## wave 4 dataset ##
# fix IDs as characters - throwing issues due to trailing spaces
raw_7 <- raw_7 %>% 
 mutate(PERSONID = trimws(as.character(PERSONID)))
raw_7_weights <- raw_7_weights %>% 
 mutate(PERSONID = trimws(as.character(PERSONID)))
# join weights and rename columns
wave_7 <- raw_7 %>% 
 left_join(raw_7_weights, by = "PERSONID") %>% 
 select(all_of(lab_7), starts_with("R07_A_C07WGT")) %>% 
 rename_with(
  .cols = starts_with("R07_A_C07WGT"),
  .fn = ~ str_replace(., "R07_A_C07WGT(\\d+)", "rep_weight_\\1")
 ) %>% 
 #add time cols
 mutate(wave = 7,
        period = 1)

#strip attributes - generating from data frame
attributes(wave_4) <- attributes(data.frame(wave_4))
attributes(wave_7) <- attributes(data.frame(wave_7))

#clean up
rm(raw_4, raw_7, raw_4_weights, raw_7_weights, lab_4, lab_7)

#### RECODE WAVE 4 DATASET ####
wave_4 <- wave_4 %>% 
 #simple recode of existing variables
 mutate(soc_pos = case_when(
  soc_pos == "(01) 1 = 10" ~ 10,
  soc_pos == "(02) 2 = 9" ~ 9,
  soc_pos == "(03) 3 = 8" ~ 8,
  soc_pos == "(04) 4 = 7" ~ 7,
  soc_pos == "(05) 5 = 6" ~ 6,
  soc_pos == "(06) 6 = 5" ~ 5,
  soc_pos == "(07) 7 = 4" ~ 4,
  soc_pos == "(08) 8 = 3" ~ 3,
  soc_pos == "(09) 9 = 2" ~ 2,
  soc_pos == "(10) 10 = 1" ~ 1
 ),
 region = case_when(
  region == "(1) 1 = Northeast" ~ "Northeast",
  region == "(2) 2 = Midwest" ~ "Midwest",
  region == "(3) 3 = South" ~ "South",
  region == "(4) 4 = West" ~ "West"
 ),
 ethnicity = ifelse(ethnicity == "(1) 1 = Hispanic", "Hispanic", "Not Hispanic"),
 race = case_when(
  race == "(1) 1 = White alone" ~ "White",
  race == "(2) 2 = Black alone" ~ "Black",
  race == "(3) 3 = Other" ~ "Other"
 ),
 sexuality = case_when(
  sexuality == "(1) 1 = Lesbian, Gay, Bisexual, Something else" ~ "Not straight",
  sexuality == "(2) 2 = Straight" ~ "Straight"
 ),
 sex = ifelse(sex == "(1) 1 = Male", "Male", "Female"),
 age = case_when(
  age == "(1) 1 = 18 to 24 years old" ~ "18-24",
  age == "(2) 2 = 25 to 34 years old" ~ "25-34",
  age == "(3) 3 = 35 to 44 years old" ~ "35-44",
  age == "(4) 4 = 45 to 54 years old" ~ "45-54",
  age == "(5) 5 = 55 to 64 years old" ~ "55-64",
  age == "(6) 6 = 65 or more years old" ~ "65+"
 ),
 eprod_30 = ifelse(eprod_30 == "(1) 1 = Yes", 1, 0),
 eprod_every_day = ifelse(eprod_every_day == "(1) 1 = Yes", 1, 0),
 eprod_some_days = ifelse(eprod_some_days == "(1) 1 = Yes", 1, 0),
 eprod_no_threshold = ifelse(eprod_no_threshold == "(1) 1 = Yes", 1, 0),
 eprod_established = ifelse(eprod_established == "(1) 1 = Yes", 1, 0),
 #create indicators
 soc_pos_1 = ifelse(soc_pos == 1, 1, 0),
 soc_pos_2 = ifelse(soc_pos == 2, 1, 0),
 soc_pos_3 = ifelse(soc_pos == 3, 1, 0),
 soc_pos_4 = ifelse(soc_pos == 4, 1, 0),
 soc_pos_5 = ifelse(soc_pos == 5, 1, 0),
 soc_pos_6 = ifelse(soc_pos == 6, 1, 0),
 soc_pos_7 = ifelse(soc_pos == 7, 1, 0),
 soc_pos_8 = ifelse(soc_pos == 8, 1, 0),
 soc_pos_9 = ifelse(soc_pos == 9, 1, 0),
 soc_pos_10 = ifelse(soc_pos == 10, 1, 0),
 northeast = ifelse(region == "Northeast", 1, 0),
 midwest = ifelse(region == "Midwest", 1, 0),
 south = ifelse(region == "South", 1, 0),
 west = ifelse(region == "West", 1, 0),
 hispanic = ifelse(ethnicity == "Hispanic", 1, 0),
 white = ifelse(race == "White", 1, 0),
 black = ifelse(race == "Black", 1, 0),
 other = ifelse(race == "Other", 1, 0),
 straight = ifelse(sexuality == "Straight", 1, 0),
 gay = ifelse(sexuality == "Not straight", 1, 0),
 female = ifelse(sex == "Female", 1, 0),
 male = ifelse(sex == "Male", 1, 0),
 age_18_24 = ifelse(age == "18-24", 1, 0),
 age_25_44 = ifelse(age == "25-34" | age == "35-44", 1, 0),
 age_45_plus = ifelse(age == "45-54" | age == "65+", 1, 0),
 #missing data flags
 missing_dem = if_any(2:8, is.na),
 missing_outcome = if_any(starts_with("eprod_"), is.na),
 missing_wght = if_any(starts_with("rep_weight_"), is.na),
 missing_any = case_when(
  missing_dem == 1 ~ 1,
  missing_outcome == 1 ~ 1,
  missing_wght == 1 ~ 1,
  TRUE ~ 0
 )
 )

#### RECODE WAVE 7 DATASET ####
wave_7 <- wave_7 %>% 
 #simple recode of existing variables
 mutate(soc_pos = case_when(
  soc_pos == "(01) 1 = 10" ~ 10,
  soc_pos == "(02) 2 = 9" ~ 9,
  soc_pos == "(03) 3 = 8" ~ 8,
  soc_pos == "(04) 4 = 7" ~ 7,
  soc_pos == "(05) 5 = 6" ~ 6,
  soc_pos == "(06) 6 = 5" ~ 5,
  soc_pos == "(07) 7 = 4" ~ 4,
  soc_pos == "(08) 8 = 3" ~ 3,
  soc_pos == "(09) 9 = 2" ~ 2,
  soc_pos == "(10) 10 = 1" ~ 1
 ),
 region = case_when(
  region == "(1) 1 = Northeast" ~ "Northeast",
  region == "(2) 2 = Midwest" ~ "Midwest",
  region == "(3) 3 = South" ~ "South",
  region == "(4) 4 = West" ~ "West"
 ),
 ethnicity = ifelse(ethnicity == "(1) 1 = Hispanic", "Hispanic", "Not Hispanic"),
 race = case_when(
  race == "(1) 1 = White alone" ~ "White",
  race == "(2) 2 = Black alone" ~ "Black",
  race == "(3) 3 = Other" ~ "Other"
 ),
 sexuality = case_when(
  sexuality == "(1) 1 = Lesbian, Gay, Bisexual, Something else" ~ "Not straight",
  sexuality == "(2) 2 = Straight" ~ "Straight"
 ),
 sex = ifelse(sex == "(1) 1 = Male", "Male", "Female"),
 age = case_when(
  age == "(1) 1 = 18 to 24 years old" ~ "18-24",
  age == "(2) 2 = 25 to 34 years old" ~ "25-34",
  age == "(3) 3 = 35 to 44 years old" ~ "35-44",
  age == "(4) 4 = 45 to 54 years old" ~ "45-54",
  age == "(5) 5 = 55 to 64 years old" ~ "55-64",
  age == "(6) 6 = 65 or more years old" ~ "65+"
 ),
 eprod_30 = ifelse(eprod_30 == "(1) 1 = Yes", 1, 0),
 eprod_every_day = ifelse(eprod_every_day == "(1) 1 = Yes", 1, 0),
 eprod_some_days = ifelse(eprod_some_days == "(1) 1 = Yes", 1, 0),
 eprod_no_threshold = ifelse(eprod_no_threshold == "(1) 1 = Yes", 1, 0),
 eprod_established = ifelse(eprod_established == "(1) 1 = Yes", 1, 0),
 #create indicators
 soc_pos_1 = ifelse(soc_pos == 1, 1, 0),
 soc_pos_2 = ifelse(soc_pos == 2, 1, 0),
 soc_pos_3 = ifelse(soc_pos == 3, 1, 0),
 soc_pos_4 = ifelse(soc_pos == 4, 1, 0),
 soc_pos_5 = ifelse(soc_pos == 5, 1, 0),
 soc_pos_6 = ifelse(soc_pos == 6, 1, 0),
 soc_pos_7 = ifelse(soc_pos == 7, 1, 0),
 soc_pos_8 = ifelse(soc_pos == 8, 1, 0),
 soc_pos_9 = ifelse(soc_pos == 9, 1, 0),
 soc_pos_10 = ifelse(soc_pos == 10, 1, 0),
 northeast = ifelse(region == "Northeast", 1, 0),
 midwest = ifelse(region == "Midwest", 1, 0),
 south = ifelse(region == "South", 1, 0),
 west = ifelse(region == "West", 1, 0),
 hispanic = ifelse(ethnicity == "Hispanic", 1, 0),
 white = ifelse(race == "White", 1, 0),
 black = ifelse(race == "Black", 1, 0),
 other = ifelse(race == "Other", 1, 0),
 straight = ifelse(sexuality == "Straight", 1, 0),
 gay = ifelse(sexuality == "Not straight", 1, 0),
 female = ifelse(sex == "Female", 1, 0),
 male = ifelse(sex == "Male", 1, 0),
 age_18_24 = ifelse(age == "18-24", 1, 0),
 age_25_44 = ifelse(age == "25-34" | age == "35-44", 1, 0),
 age_45_plus = ifelse(age == "45-54" | age == "65+", 1, 0),
 #missing data flags
 missing_dem = if_any(2:8, is.na),
 missing_outcome = if_any(starts_with("eprod_"), is.na),
 missing_wght = if_any(starts_with("rep_weight_"), is.na),
 missing_any = case_when(
  missing_dem == 1 ~ 1,
  missing_outcome == 1 ~ 1,
  missing_wght == 1 ~ 1,
  TRUE ~ 0
 )
 )

#### COMBINE DATASET ####
full <- bind_rows(wave_4, wave_7)

#### MISSING DATA INFO ####
# 43,621 observations are missing data
sum(full$missing_any)
# 1,199 observations are missing weights
sum(full$missing_wght)
# 67 observations are missing one of the potential outcome variables (might change if we modify)
sum(full$missing_outcome)
# 43,144 observations are missing demographic info
sum(full$missing_dem)
# 99% of this is coming from the social positionality variable (N = 43,144)
# these are high in both wave 4 (N = 22,756) and wave 7 (N = 20,388)
sum(is.na(full$soc_pos))

#### SAVE R DATASET ####
# Includes wave 4, wave 7, and the combined version
save(wave_4, wave_7, full,
     file = "data/combined_clean.RData")

#check it works
#load("data/combined_clean.RData") #works!