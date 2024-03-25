# set the directory
setwd("C:\\Users\\Nika\\OneDrive\\Documents\\Master's\\Dissertation\\Data Section")

#load libraries
library(dplyr)
library(readr)
library(ggplot2)
library(anthro)
library(tidyverse)
library(aod)
library(rgdal)
library(ggthemes)
library(sp)
library(tidyr)
library(readr)

# load data
individuals <- read_csv('Data Files for Git/repeat_individual_questionnaire.csv')

households <- read_csv('Data Files for Git/Submissions.csv')

wealth <- read_csv('Data Files for Git/wealth_index.csv')

wih <- individuals %>%
  left_join(households,by=c('parent_key'='key')) %>%
  left_join(wealth, by='household_id') %>%
  select(-'id.x', -'id.y')



encryption <- read_csv('Data Files for Git/household_id_encryptions.csv') %>%
  mutate(hh_id = anonymized_household_id)



minicensus_data <- read.csv('Data Files for Git/minicensus_data.csv')
minicensus_household_final <- read.csv('Data Files for Git/minicensus_household_final.csv')
minicensus_wealth_index <- read.csv('Data Files for Git/minicensus_wealth_index.csv')

minicensus <- merge(minicensus_data, minicensus_household_final, by = c('hh_id', 'hh_n_constructions', 'hh_n_constructions_sleep', 'n_nets_in_hh'), all=TRUE) %>%
  merge(minicensus_wealth_index, by = c('hh_id', 'hh_n_constructions', 'n_nets_in_hh', 'hh_owns_cattle_or_pigs'), all=TRUE) %>%
  merge(encryption, by='hh_id', all=TRUE)



data_joined <- left_join(minicensus, wih, by=c('household_id')) 


# this will make the variables smaller for analysis

## housing and sleeping conditions
data_joined$hh_member_num_cat <- cut(data_joined$hh_member_num, breaks =c(-Inf, 5, 10, 15, Inf), labels =c("1-5", "5-10", "10-15", "greater than 15"), include.lowest = TRUE)

data_joined$hh_n_constructions_cat <- cut(data_joined$hh_n_constructions, breaks =c(-Inf, 0, 1, 5, Inf), labels =c("0", "1", "2-5", "greater than 5"), include.lowest = TRUE)

data_joined$hh_n_constructions_sleep_cat <- cut(data_joined$hh_n_constructions_sleep, breaks =c(-Inf, 0, 1, 5, Inf), labels =c("0", "1", "2-5", "greater than 5"), include.lowest = TRUE)

data_joined$n_nets_in_hh_cat <- cut(data_joined$n_nets_in_hh, breaks =c(-Inf, 0, 1, 5, 10, Inf), labels =c("0", "1", "2-5", "6-10", 'greater than 10'), include.lowest = TRUE)


## Ownership of assets: 

# Combine information about pigs and cows into separate columns
data_joined <- data_joined %>%
  mutate(hh_pigs = case_when(
    hh_owns_cattle_or_pigs == 'No' | hh_owns_cattle_or_pigs == 0 ~ 0,
    is.na(hh_n_pigs_less_than_6_weeks) & is.na(hh_n_pigs_greater_than_6_weeks) ~ NA_real_,
    TRUE ~ coalesce(as.numeric(hh_n_pigs_less_than_6_weeks), 0) + coalesce(as.numeric(hh_n_pigs_greater_than_6_weeks), 0)
  ))

data_joined <- data_joined %>%
  mutate(hh_cows = case_when(
    hh_owns_cattle_or_pigs == 'No' | hh_owns_cattle_or_pigs == 0 ~ 0,
    is.na(hh_n_cows_less_than_1_year) & is.na(hh_n_cows_greater_than_1_year) ~ NA_real_,
    TRUE ~ coalesce(as.numeric(hh_n_cows_less_than_1_year), 0) + coalesce(as.numeric(hh_n_cows_greater_than_1_year), 0)
  ))

# Create new columns with categorized values for pigs
data_joined$hh_pigs_cat <- cut(data_joined$hh_pigs, 
                               breaks = c(-Inf, 0, 1, 5, 10, 15, 20, Inf), 
                               labels = c("0", "1", "2-5", "6-10", "11-15", "16-20", "greater than 20"),
                               include.lowest = TRUE)

# Create new columns with categorized values for cows
data_joined$hh_cows_cat <- cut(data_joined$hh_cows, 
                               breaks = c(-Inf, 0, 1, 5, 10, 15, 20, 25, Inf), 
                               labels = c("0", "1", "2-5", "6-10", "11-15", "16-20", "21-25", "greater than 5"),
                               include.lowest = TRUE)


# Household Building Types
data_joined <- data_joined %>%
  mutate(building_type_cat = case_when(
    building_type_apartment == 1 | building_type_flat == 1 ~ "apartment/flat",
    building_type_conventional_house == 1 ~ "conventional_house",
    building_type_hut == 1 ~ "hut",
    building_type_precarious == 1 ~ "precarious",
    building_type_traditional_mud_house == 1 ~ "traditional_mud_house",
    building_type_other == 1 ~ "other",
    TRUE ~ NA_character_
  ))


# list materials of households and make household wall material classifications
data_joined <- data_joined %>%
  mutate(wall_material_cat = case_when(
    wall_material_cement_blocks == 1 | wall_material_brick_block == 1 | wall_material_adobe_block == 1 
    ~ "blocks - cement, adobe, brick",
    wall_material_reed == 1 | wall_material_bamboo == 1 | wall_material_palm_tree == 1 | wall_material_tinned_wood == 1 | wall_material_bark == 1 ~ "natural - bark, reed, bamboo, palm_tree, tinned_wood",
    wall_material_tin == 1 | wall_material_zinc == 1 ~ "tin/zinc",
    wall_material_wood == 1 ~ "wood",
    wall_material_cardboard == 1 | wall_material_paper == 1 | wall_material_plastic_bags == 1~ "miscellaneous: cardboard, paper, plastic_bags",
    wall_material_other == 1 ~ "other",
    TRUE ~ NA_character_
  ))


# Main water sources for consumption
data_joined$cook_main_water_source_cat <- case_when(
  data_joined$cook_main_water_source %in% c("PIPED_WATER_HOUSE",
                                            "PIPED_WATER_COMPOUND",
                                            "PIPED_WATER_NEIGHBOR",
                                            "FOUNTAIN",
                                            "PROTECTED_WELL_IN_BACKYARD",
                                            "PROTECTED_WELL_OUT_BACKYARD",
                                            "RAINWATER",
                                            "HOLE_PROTECTED_HAND_PUMP_YARD",
                                            "HOLE_MAN_PUMP_INSIDE_HOUSEHOLD") ~ "Improved",
  data_joined$cook_main_water_source %in% c("UNPROTECTED_WELL_IN_HOUSEHOLD",
                                            "UNPROTECTED_WELL_OUT_HOUSEHOLD",
                                            "WATER_FROM_RIVER",
                                            "LAKE",
                                            "LAGOON") ~ "Unimproved",
  TRUE ~ NA_character_  # for other cases
)


# Time for taking main water
data_joined$water_time_cat <- coalesce(data_joined$water_time_under_10_min, 
                                       data_joined$water_time_between_10_30_min, 
                                       data_joined$water_time_between_30_60_min, 
                                       data_joined$water_time_more_than_hour)
desired_order_water_time <- c("under_10_min", "between_10_30_min", "between_30_60_min", "more_than_hour")  # 
data_joined$water_time_cat <- factor(data_joined$water_time_cat, levels = desired_order_water_time)



# Main energy source for lighting
data_joined <- data_joined %>%
  mutate(lighting_cat = case_when(
    lighting_energy_electricity == 1 ~ "electricity",
    lighting_energy_generator == 1 ~ "generator",
    lighting_energy_solar_panel == 1 ~ "solar_panel",
    lighting_energy_gas == 1 | lighting_energy_oil == 1 ~ "gas",
    lighting_energy_oil == 1 ~ "oil",
    lighting_energy_candles == 1 ~ "candles",
    lighting_energy_batteries == 1 ~ "batteries",
    lighting_energy_firewood == 1 ~ "firewood",
    lighting_energy_other == 1 ~ "other",
    TRUE ~ NA_character_
  ))


# Ownership of electronic devices
data_joined$hh_possessions_cat <- sapply(strsplit(tolower(data_joined$hh_possessions), " "), function(x) {
  if (is.na(x[1])) {
    return(NA)
  } else {
    return(sum(c("tv", "radio", "cell_phone") %in% x))
  }
})





data <- data_joined %>%
  mutate(age_in_days = age * 356.25,
         age_in_months = age_in_days/30.4, 
         weight = coalesce(weight, child_weight),
         height = coalesce(height, child_height),
         sex = ifelse(sex == "Female", "f", "m"),
         dob=as.Date(dob)) %>%
  filter(!is.na(extid),
         !is.na(weight),
         !is.na(height),
         age <= 5) %>%
  select(household_id, hh_id, extid, current_visit, ward_name, cluster, todays_date, dob, sex, age, age_in_months, age_in_days, weight, height, hh_member_num, hh_n_constructions, n_nets_in_hh, member_per_sleep, hh_n_cows, hh_n_pigs, animals, hh_main_building_type, building_type_apartment, building_type_conventional_house, building_type_flat, building_type_hut, building_type_precarious, building_type_traditional_mud_house, building_type_other, hh_main_wall_material, hh_wall_adobe_block, hh_wall_bamboo, hh_wall_tin, hh_wall_bark, hh_wall_tinned_wood, hh_wall_palm_tree, hh_wall_brick_block, hh_wall_Other, cook_main_water_source, water_source_piped_water_house, water_source_piped_water_compound, water_source_piped_water_neighbor, water_source_fountain, water_source_protected_well_in_backyard, water_source_protected_well_out_backyard, water_sourcel_unprotected_well_in_household, water_source_unprotected_well_out_household, water_source_hole_man_pump_inside_household, water_source_hole_protected_hand_pump_yard, water_source_surface, water_source_rainwater, water_source_mineral_bottled_water, water_source_water_tank_truck, water_source_other, water_time_under_10_min, water_time_between_10_30_min, water_time_between_30_60_min, water_time_more_than_hour, lighting_energy_electricity, lighting_energy_generator, lighting_energy_solar_panel, lighting_energy_gas, lighting_energy_oil, lighting_energy_candles, lighting_energy_batteries, lighting_energy_firewood, lighting_energy_other, cook_time_to_water, hh_main_energy_source_for_lighting, hh_possession_Radio, hh_possession_TV, hh_possession_Cell_phone, any_deaths_past_year, hh_head_age, hh_head_gender, wealth_index_score, wealth_index_std_score, wealth_index_rank, hh_member_num_cat, hh_n_constructions_cat, hh_n_constructions_sleep_cat, n_nets_in_hh_cat, hh_pigs_cat, hh_cows_cat, building_type_cat, wall_material_cat, cook_main_water_source_cat, water_time_cat, lighting_cat, hh_possessions_cat) 



# multiple zscore calculators have been tried: whoanthro, childsds; they yield the same as the anthro package below. Therefore, the anthro package is used.

# run the WHO's anthro package and save their output in a variable (library(anthro))
who_output <- with(
  data,
  anthro_zscores(
    sex = sex,
    age = age_in_days,
    weight = weight,
    lenhei = height
  )
)

# make a zlen column in data that comes from who_output 
# zlen is length/height-for-age z-score
data$zlen <- who_output$zlen

data <- data %>%
  mutate(stunted = zlen <= -2)



# Now we will add the distance variables

# Get latitude and longitude variables associated with each household_id to calculate the following variables:
# if(FALSE){
#   # Below section is how locations were generated, does not need to be run again
#   census_wide_households <- read_csv('Data Section/tmp/crf/census_wide_households.csv')
#   household_locations <- census_wide_households %>% dplyr::select(household_id = hh_id,
#                                                                   lng, lat)
#   write_csv(household_locations, 'Data Section/tmp/crf/household_locations.csv')
# }
locations <- read_csv('Data Files for Git/household_locations.csv')

# Read in household ID encryptions
encryptions <- read_csv('Data Files for Git/household_id_encryptions.csv')

# Decrypt the household IDs
locations <- locations %>%
  dplyr::rename(anonymized_household_id = household_id) %>%
  left_join(encryptions) %>%
  dplyr::select(-anonymized_household_id)

# The above is now a table with latitude and longitude and household ID which can be joined to the clean data
data <- left_join(data, locations)

# Now "clean" has lng and lat columns

# Read in all health facilities
# hf <- rgdal::readOGR('Data Section/tmp/crf/health_facilities/', 'health_facilities')
# save(hf, file = 'Data Section/tmp/crf/hf.RData')
library(sp)
load('Data Files for Git/hf.RData')
# Sanity plot
#plot(hf)

# health_dist: distance from the household to the health facility (Centro de Saúde de Mopeia Sede) [-17.979471355490286, 35.712640789708786]
# Create an object just for mopeia sede health facility
# mshf <- hf[hf@data$name == 'Centro de Saude de Mopeia Sede',]

mshf <- hf

# Project to UTM coordinates so as to measure meters
proj_crs <- CRS("+proj=utm +zone=36 +south +ellps=WGS84 +datum=WGS84 +units=m +no_defs")
clean_spatial <- data %>% mutate(x = lng, y = lat)
coordinates(clean_spatial) <- ~x+y
proj4string(clean_spatial) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
clean_spatial_projected <- spTransform(clean_spatial, proj_crs)
mshf_projected <- spTransform(mshf, proj_crs)

# Calculate distance in meters
d <- rgeos::gDistance(clean_spatial_projected, mshf_projected, byid = TRUE)

# calculate minimum distance from house to hospital
min_dist <- apply(d, 2, min) 

# Pop the distances into the dataframe
data$health_dist <- as.numeric(min_dist)

# Sanity check the result
ggplot(data = data,
       aes(x = lng,
           y = lat,
           color = health_dist)) +
  geom_point() +
  scale_color_gradient2(mid = 'red', high = 'black')

# road_dist: distance from the household to the road
# roads <- rgdal::readOGR('Data Section/tmp/crf/mopeia_roads/', 'mopeia_roads')
# save(roads, file = 'Data Section/tmp/crf/roads.RData')
load('Data Files for Git/roads.RData')
roads_projected <- spTransform(roads, proj_crs)

# sanity plot
#plot(roads_projected)

# Calculate distance to all roads in meters
d <- rgeos::gDistance(clean_spatial_projected, roads_projected, byid = TRUE)
dx <- apply(d, 2, min)

# Pop the distances into the dataframe
data$road_dist <- as.numeric(dx)

# Sanity plot
roads_gg <- fortify(roads, id = 'osm_id')
ggplot() +
  geom_path(data = roads_gg,
            aes(x = long, y = lat, group = group)) +
  geom_point(data = data,
             aes(x = lng, y = lat, color = road_dist),
             size = 0.2) +
  scale_color_gradient2(mid = 'red', high = 'black')

# create variable admin_dist: distance from the household to the administrative post (government building/ Governo de Distrito de Mopeia) with the lat/lon points [-17.981304088784032, 35.710798061452984]
admin <- tibble(x = 35.710798061452984,
                y = -17.981304088784032)
coordinates(admin) <- ~x+y
proj4string(admin) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
admin_projected <- spTransform(admin, proj_crs)

# Calculate distance in meters
d <- rgeos::gDistance(clean_spatial_projected, admin_projected, byid = TRUE)



# Pop the distances into the dataframe
data$admin_dist <- as.numeric(d)

#make th distance from meters to kilometers
data$admin_km <- data$admin_dist / 1000
data$health_km <- data$health_dist / 1000
data$road_km <- data$road_dist / 1000

# make age groups: Category 1: 0-5, Category 2: 6-11, Category 3: 12-17, Category 4: 18-24 months
data <- data %>% 
  mutate(age_in_months = age_in_days/30.4)  %>%
  mutate(age_category = case_when(
    age_in_months >= 0 & age_in_months < 6 ~ '0-5',
    age_in_months >= 6 & age_in_months < 12 ~ '6-11',
    age_in_months >= 12 & age_in_months < 24 ~ '12-23',
    age_in_months >= 24 & age_in_months < 36 ~ '24-35',
    age_in_months >= 36 & age_in_months < 48 ~ '36-47',
    age_in_months >= 48 & age_in_months <= 61 ~ '48-60',
    TRUE ~ NA_character_),
    stunted_numeric = as.numeric(stunted))



# make this into a csv file and save to computer 
#write.csv(data, "C:\\Users\\Nika\\OneDrive\\Documents\\Master's\\Dissertation\\Data Section\\PUblished Paper\\Published Full Dataset.csv", row.names=FALSE)




# keep only the earliest observations that have weight and height 
# (if someone has many keep earliest)
data_individual <- data %>% 
  arrange(todays_date) %>%
  dplyr::distinct(extid, .keep_all = TRUE)


ggplot(data_individual, aes(x = health_km, y = stunted_numeric)) +
  geom_point(aes(color = factor(stunted)), size = 3) +
  labs(title = "Scatter Plot of Stunted vs Distance",
       x = "Distance",
       y = "Stunted",
       color = "Stunted") +
  theme_minimal()




# # remove the crazy heights and weights
# #data <- data %>% filter(merged_height <= 220, merged_weight <= 80)
# # Calculate the IQR for height
# Q1 <- quantile(data$merged_height, 0.25)
# Q3 <- quantile(data$merged_height, 0.75)
# IQR <- Q3 - Q1
# 
# # Define a range for outliers
# lower_bound <- Q1 - 1.5 * IQR
# upper_bound <- Q3 + 1.5 * IQR
# 
# # Identify outliers based on IQR method
# outliers_iqr <- data[data$merged_height < lower_bound | data$merged_height > upper_bound, ]

# Create a scatter plot of height vs weight
ggplot(data, aes(x = height, y = weight)) +
  geom_point() +
  labs(title = "Filtered Scatter Plot of Height vs Weight",
       x = "Height",
       y = "Weight")

# Create the growth chart
ggplot(data, aes(x = age_in_months)) +
  geom_line(aes(y = merged_height, color = "Height"), size = 1.5) +
  geom_line(aes(y = merged_weight, color = "Weight"), size = 1.5) +
  labs(title = "Growth Chart", x = "Age (Months)", y = "Measurement") +
  scale_color_manual(values = c("Height" = "blue", "Weight" = "red")) +
  theme_minimal()








