# set the directory
setwd("C:\\Users\\Nika\\OneDrive\\Documents\\Master's\\Dissertation\\Data Section\\Published Paper")

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
library(lme4)

# load data

data_full <- read.csv('Published Full Dataset.csv') 


# # remove outliers from the 'height' and 'weight' columns
# 
# # Define the IQR and bounds for height
# Q1_height <- quantile(data_full$height, 0.25)
# Q3_height <- quantile(data_full$height, 0.75)
# IQR_height <- Q3_height - Q1_height
# 
# lower_bound_height <- Q1_height - 1.5 * IQR_height
# upper_bound_height <- Q3_height + 1.5 * IQR_height
# 
# # Define the IQR and bounds for weight
# Q1_weight <- quantile(data_full$weight, 0.25)
# Q3_weight <- quantile(data_full$weight, 0.75)
# IQR_weight <- Q3_weight - Q1_weight
# 
# lower_bound_weight <- Q1_weight - 1.5 * IQR_weight
# upper_bound_weight <- Q3_weight + 1.5 * IQR_weight
# 
# # Remove outliers for both height and weight
# data_filtered <- data_full %>%
#   filter(height >= lower_bound_height & height <= upper_bound_height,
#          weight >= lower_bound_weight & weight <= upper_bound_weight)



# keep only the earliest observations that have weight and height 
# (if someone has many keep earliest)
data_ind <- data_full %>% 
  arrange(todays_date) %>%
  dplyr::distinct(extid, .keep_all = TRUE)


ggplot(data_ind, aes(x = health_km, y = stunted_numeric)) +
  geom_point(aes(color = factor(stunted)), size = 3) +
  labs(title = "Scatter Plot of Stunted vs Distance",
       x = "Distance",
       y = "Stunted",
       color = "Stunted") +
  theme_minimal()


# Create a scatter plot of height vs weight
ggplot(data_ind, aes(x = weight, y = height)) +
  geom_point() +
  labs(title = "Relationship between Height and Weight",
       x = "Height",
       y = "Weight") +
  theme_minimal()

# Create the growth chart
ggplot(data_ind, aes(x = age_in_months)) +
  geom_point(aes(y = height, color = "Height"), size = 0.5) +
  geom_point(aes(y = weight, color = "Weight"), size = 0.5) +
  labs(title = "Growth Chart", x = "Age (Months)", y = "Measurement") +
  scale_color_manual(values = c("Height" = "blue", "Weight" = "red")) +
  theme_minimal()






model_data <- data_ind %>%
  dplyr::select(stunted_numeric, cluster, hh_member_num_cat, hh_n_constructions_cat, hh_n_constructions_sleep_cat, n_nets_in_hh_cat, hh_pigs_cat, hh_cows_cat, building_type_cat, wall_material_cat, cook_main_water_source_cat, water_time_cat, lighting_cat, hh_possessions_cat, hh_head_age, hh_head_gender)

# # Remove rows with missing values
# model_data <- na.omit(model_data)
# 
# # Fit the initial model with a random intercept for 'cluster'
# full_model <- lmer(stunted_numeric ~ . + (1 | cluster), data = data_ind)
# 
# 
# tab_model(full_model,
#           # pred.labels = c("Intercept", "Distance from Nearest Health Facility", "No. of People in Household", "Poor Category"),
#           dv.labels = "Model of Stunting",
#           string.ci = "Conf. Int (95%)",
#           string.p = "P-Value")