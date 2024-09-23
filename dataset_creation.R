# PURPOSE: MAKE THE DATASET FOR ANALYSIS


#load libraries
library(tidyverse)
library(anthro)
library(zscorer)



# LOAD THE DATA
# first load the census wide df with all household information then load the safety and efficacy df


# encryption document received from Joe
encryptions <- read.csv("data/household_id_encryptions.csv") %>%
  rename(hhid = household_id)


# load safety and efficacy df to have height and weight
repeat_individual_questionnaire <- read.csv('data/repeat_individual_questionnaire.csv')
submissions <- read.csv('data/Submissions.csv') %>%
  left_join(encryptions, by=c('household_id' = 'anonymized_household_id'))
# comes from AWS: bohemia-dflake/bohemia-safety-efficacy/crf.zip


# wealth index df from published paper: https://dfverse.csuc.cat/dfset.xhtml?persistentId=doi:10.34810/df682
# this specific dfset was obtained through old slack messages via Joe
wealth <- read.csv('data/wealth_index.csv') %>%
  left_join(encryptions, by=c('household_id' = 'hhid')) %>%
  rename(hhid = household_id)


# comes from  bohemia-dflake/bohemia-minicensus in AWS
clean_minicensus_main <- read.csv('data/clean_minicensus_main.csv') %>%
  left_join(encryptions, by=c('hh_id' = 'anonymized_household_id'))
minicensus <- clean_minicensus_main %>%
  dplyr::select("hhid", "hh_member_num", "hh_n_constructions", 
                "hh_n_constructions_sleep", 
                "n_nets_in_hh", "hh_owns_cattle_or_pigs", 
                "hh_n_pigs_less_than_6_weeks", 
                "hh_n_pigs_greater_than_6_weeks", "hh_n_cows_less_than_1_year", 
                "hh_n_cows_greater_than_1_year", "hh_main_building_type",
                "hh_main_wall_material", "cook_main_water_source", 
                "cook_time_to_water", 
                "hh_main_energy_source_for_lighting", "hh_possessions")



# MERGE DATA 

wse <- repeat_individual_questionnaire %>%
  left_join(submissions,by=c('parent_key'='key')) %>%
  left_join(wealth, by=c('hhid')) %>%
  select(-'id.x', -'id.y') 

df_joined <- inner_join(wse, minicensus, by=c('hhid')) 



# DICHOTOMIZE DATA FOR ANALYSIS ###########



## HOUSING AND SLEEPING CONDITIONS

df_joined$hh_member_num_cat <- cut(df_joined$hh_member_num, breaks =c(-Inf, 5, 10, 15, Inf), labels =c("1-5", "5-10", "10-15", "greater than 15"), include.lowest = TRUE)

df_joined <- df_joined %>%
  mutate( hh_member_num_binary = ifelse( hh_member_num_cat == "1-5", "1-5", "6 or more" ) )
df_joined$hh_member_num_binary <- factor(df_joined$hh_member_num_binary, levels = c("1-5", "6 or more") )

df_joined$hh_n_constructions_cat <- cut(df_joined$hh_n_constructions, breaks =c(-Inf, 0, 1, 5, Inf), labels =c("0", "1", "2-5", "greater than 5"), include.lowest = TRUE)

df_joined$hh_n_constructions_sleep_cat <- cut(df_joined$hh_n_constructions_sleep, breaks =c(-Inf, 0, 1, 5, Inf), labels =c("0", "1", "2-5", "greater than 5"), include.lowest = TRUE)

df_joined$n_nets_in_hh_cat <- cut(df_joined$n_nets_in_hh, breaks =c(-Inf, 0, 1, 5, 10, Inf), labels =c("0", "1", "2-5", "6-10", 'greater than 10'), include.lowest = TRUE)



## OWNERSHIP OF ASSETS 

# hh_owns_cattle_or_pigs is already a category so we are good



## HOUSEHOLD BUILDING TYPE

# Function to categorize housing types
categorize_housing <- function(type) {
  if (is.na(type)) {
    return(NA)
  } else if (grepl("conventional_house|convencional|apartment|flat", type, ignore.case = TRUE)) {
    return("conventional_house")
  } else if (grepl("hut|PAU-|PAU |PAUS ", type, ignore.case = TRUE)) {
    return("hut")
  } else if (grepl("PRECARI|adobe|capi|hut|tijolo|outros|other|agua", type, ignore.case = TRUE)) {
    return("precarious")
  } else if (grepl("traditional_mud_house|tradicional", type, ignore.case = TRUE)) {
    return("traditional_mud_house")
  } else if (grepl("outros|other", type, ignore.case = TRUE)) {
    return("other")
  } else {
    return("other")
  }
}

# Apply the categorization function to each housing type
df_joined$hh_main_building_type_cat <- sapply(df_joined$hh_main_building_type, categorize_housing)



## HOUSEHOLD WALL MATERIALS

# list materials of households and make household wall material classifications

# Count the number of wall materials
df_joined <- df_joined %>%
  mutate(hh_main_wall_material_count = ifelse(is.na(hh_main_wall_material), NA, lengths(strsplit(hh_main_wall_material, " ")))) %>%
  mutate(hh_main_wall_material_cat = 
           case_when(is.na(hh_main_wall_material_count) ~ NA,
                     hh_main_wall_material_count == 1 ~ '1',
                     hh_main_wall_material_count == 2 ~ '2',
                     hh_main_wall_material_count == 3 ~ '3',
                     hh_main_wall_material_count > 3 ~ '>4')) 

df_joined$hh_main_wall_material_cat <- factor( df_joined$hh_main_wall_material_cat, levels = c("1", "2", "3", ">4") )



## MAIN WATER SOURCES FOR CONSUMPTION

# create improved and unimproved water sources
df_joined$cook_main_water_source_cat <- case_when(
  df_joined$cook_main_water_source %in% c("piped_water_house",
                                          "piped_water_compound",
                                          "piped_water_neighbor",
                                          "fountain",
                                          "protected_well_in_backyard",
                                          "protected_well_out_backyard",
                                          "rainwater",
                                          "hole_protected_hand_pump_yard",
                                          "hole_man_pump_inside_household") ~ "Improved",
  df_joined$cook_main_water_source %in% c("unprotected_well_in_household",
                                          "unprotected_well_out_household",
                                          "water_from_river",
                                          "lake",
                                          "lagoon") ~ "Unimproved",
  TRUE ~ NA_character_  # for other cases
)



## TIME FOR TAKING MAIN WATER SOURCE

# changed the categories because of finding only more than hour significant
desired_order_water_time <- c("under_10_min", "between_10_30_min", "between_30_60_min", "more_than_hour")  # 
df_joined <- df_joined %>%
  mutate(cook_time_to_water_cat = case_when(
    is.na(cook_time_to_water) ~ NA_character_,
    cook_time_to_water %in% c("under_10_min", "between_10_30_min", "between_30_60_min") ~ "less_than_hour",
    TRUE ~ "more_than_hour"
  ))

# Main energy source for lighting
# minimised the amount of categories due to finding the significance of only electricity
df_joined <- df_joined %>%
  mutate(hh_main_energy_source_for_lighting_cat = case_when(
    is.na(hh_main_energy_source_for_lighting) ~ NA_character_,
    hh_main_energy_source_for_lighting == 'electricity' ~ ' electricity',
    TRUE ~ 'other'
  ))



# OWNSERSHIP OF ELECTRONIC DEVICES

possessions <- c('cell_phone', 'tv', 'radio')
df_joined <- df_joined %>%
  mutate(hh_possessions_cat = str_count(hh_possessions, 
                                        paste(possessions, collapse = "|")))

df_joined$hh_possessions_cat <- factor(df_joined$hh_possessions_cat, levels = c("0", "1", "2", "3") )




# CREATE AGE AND STUNTING VARIABLES

df <- df_joined %>%
  mutate(age_in_days = round(age * 356.25, 0),
         age_in_months = round(age_in_days/30.4, 0), 
         weight = coalesce(weight, child_weight),
         height = coalesce(height, child_height),
         sex_name = ifelse(sex == "Female", "f", "m"),
         dob=as.Date(dob),
         sex = ifelse(sex == "Female", 2, 1)) %>%
  filter(!is.na(extid),
         !is.na(weight),
         !is.na(height),
         height <= 120 & height >= 45,
         age_in_months <= 60,
         age != 0) %>%
  select(household_id, hhid, extid, current_visit, ward_name, cluster, todays_date, 
         dob, sex, sex_name, age, age_in_months, age_in_days, weight, height, 
         wealth_index_rank, hh_member_num, hh_member_num_cat, hh_member_num_binary,
         hh_n_constructions_cat, hh_n_constructions_sleep_cat, n_nets_in_hh_cat, 
         hh_owns_cattle_or_pigs, hh_main_building_type_cat,
         hh_main_wall_material_count, 
         hh_main_wall_material_cat, 
         cook_main_water_source_cat, cook_time_to_water_cat, cook_time_to_water, 
         hh_main_energy_source_for_lighting_cat, hh_possessions_cat) 

num_kids_before_cleaning <- nrow(df)

# multiple zscore calculators have been tried: whoanthro, childsds; they yield the same as the anthro package below. Therefore, the anthro package is used.

# run the WHO's anthro package and save their output in a variable (library(anthro))

df <- df %>%
  mutate(with(
    df,
    anthro_zscores(
      sex = sex_name,
      age = age_in_days,
      weight = weight,
      lenhei = height
    )
  )) %>% 
  filter(!is.na(zlen)) %>% 
  select(-fss, -zss, -fts, -zts, -fac, -zac, -fhc, -zhc, -fbmi, -zbmi, -zwfl, 
         -fwfl, -fwei, -zwei, -flen, -csex, -cmeasure, -cbmi, -clenhei) %>%
  mutate(stunted = zlen <= -2) # zlen is length/height-for-age z-score

num_kids_post_some_cleaning <- nrow(df)


# calculate height-for-age, weight-for-age, weight-for-height
# https://cran.r-project.org/web/packages/zscorer/readme/README.html
df <- df %>%
  mutate(whz = getCohortWGS(data = df,
                            sexObserved = "sex",
                            firstPart = "weight",
                            secondPart = "height",
                            index = "wfh"),
         haz = getCohortWGS(data = df,
                            sexObserved = "sex",
                            firstPart = "height",
                            secondPart = "age_in_months",
                            index = "hfa"),
         waz = getCohortWGS(data = df,
                            sexObserved = "sex",
                            firstPart = "weight",
                            secondPart = "age_in_months",
                            index = "wfa")) %>%
  filter(waz > -5 & waz < 5,
         haz > -5 & haz < 5,
         whz > -5 & whz < 5)

num_kids_post_cleaning <- nrow(df)

# make age groups: Category 1: 0-5, Category 2: 6-11, Category 3: 12-17, Category 4: 18-24 months
df <- df %>% 
  mutate(age_in_months = age_in_days/30.4)  %>%
  mutate(age_category = case_when(
    age_in_months >= 0  & age_in_months < 12 ~ '0-11',
    age_in_months >= 12 & age_in_months < 24 ~ '12-23',
    age_in_months >= 24 & age_in_months < 36 ~ '24-35',
    age_in_months >= 36 & age_in_months < 48 ~ '36-47',
    age_in_months >= 48 & age_in_months <= 61 ~ '48-60',
    TRUE ~ NA_character_),
    stunted_numeric = as.numeric(stunted)) %>%
  group_by(hhid) %>% # randomly select 1 kid per household
  sample_n(size = 1)

num_kids_post_all_cleaning <- nrow(df)

#write.csv(df, file = 'stunting_df.csv')
df <- read_csv('data/stunting_df.csv')



# MORE EDITS TO THE DATASET

# household member and wall material edits were made to the dataset

df$hh_member_num_cat <- factor(df$hh_member_num_cat, levels = c("1-5", "5-10", "10-15", "greater than 15"))
df$hh_member_num_binary <- factor(df$hh_member_num_binary, levels = c("1-5", "6 or more"))
df$hh_main_wall_material_cat <- factor(df$hh_main_wall_material_cat, levels = c("1", "2", "3", ">4"))
df$hh_possessions_cat <- factor(df$hh_possessions_cat, levels = c("0", "1", "2", "3"))



# later on wasting and underweight were incorporated into the analysis so this was added next

df <- df %>% 
  mutate(with(
    df,
    anthro_zscores(
      sex = sex_name,
      age = age_in_days,
      weight = weight,
      lenhei = height
    )
  )) %>% 
  select(-fss, -zss, -fts, -zts, -fac, -zac, -fhc, -zhc, -fbmi,
         -zbmi,
         -fwfl, -fwei, -flen, -csex, -cmeasure, -cbmi, -clenhei) %>%
  mutate(wasted = zwfl <= -2,
         underweight = zwei <= -2,
         wasted_numeric = as.numeric(wasted),
         underweight_numeric = as.numeric(underweight)) %>% 
  mutate(undernutrition = ifelse(wasted == 'TRUE' | stunted == 'TRUE' | underweight == 'TRUE', 'TRUE', 'FALSE'))

# some renaming
df <- df %>% 
  mutate(ward_name = case_when(
  ward_name == 'Campo . Campo sede' ~ 'Campo Sede',
  ward_name == 'Campo . Catale' ~ 'Catale',
  ward_name == 'Campo . Luala' ~ 'Luala',
  ward_name == 'Campo . Mungane' ~ 'Mungane',
  ward_name == 'Mopeia sede . Cuacua' ~ 'Mopeia sede/Cuacua',
  ward_name == 'Mopeia sede . Nzanza' ~ 'Nzanza',
  ward_name == 'Mopeia sede . Rovuma/Conho' ~ 'Rovuma/Conho',
  ward_name == 'Mopeia sede . Sambalendo' ~ 'Sambalendo/Chimuara'), 
  wealth_index_rank = case_when(
    wealth_index_rank == 'Rank 1 (richest)' ~ 'Least poor',
    wealth_index_rank == "Rank 2" ~ 'Less poor',
    wealth_index_rank == "Rank 3" ~ 'Moderately poor',
    wealth_index_rank == "Rank 4" ~ 'Poorer',
    wealth_index_rank == 'Rank 5 (poorest)' ~ 'Poorest'))

# forgot to keep hh_member_num so re-add the column
needed_cols <- clean_minicensus_main %>%
  select("hhid", "hh_member_num", "hh_n_constructions", 
         "hh_n_constructions_sleep", 
         "n_nets_in_hh",
         "hh_n_pigs_less_than_6_weeks", 
         "hh_n_pigs_greater_than_6_weeks", "hh_n_cows_less_than_1_year", 
         "hh_n_cows_greater_than_1_year", "hh_main_building_type",
         "hh_main_wall_material", "cook_main_water_source", 
         "hh_main_energy_source_for_lighting", "hh_possessions") %>% 
  filter(hhid %in% df$hhid)

df <- left_join(df, needed_cols, by = 'hhid')

#write.csv(df, file = 'undernutrition_df.csv')

# zlen is a measurement for stunting (age for height) - long term
# zwei is a measurement for underweight (weight for age)
# zwfl is a measurement of wasting (weight for height) - short term