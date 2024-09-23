# PURPOSE: ANALYSIS


#load libraries
library(tidyverse)
library(anthro)
library(aod)
library(ggthemes)
library(sp)
library(zscorer)
library(lme4)
library(patchwork)
library(sjPlot)
library(parameters)
library(kableExtra)
library(webshot2)
library(magick)
library(rgeos)
library(ggrepel)
library(scales)
library(gridExtra)

# LOAD DATA
df <- read_csv('data/undernutrition_df.csv')



# ADD THE DISTANCE DATA (not in dataset because has locations)

### the following can only be run on computers with rgeos
# this section is all distance related analysis
#########################################################

# let's add the distance variables
# Now we will add the distance variables

# Get latitude and longitude variables associated with each household_id to calculate the following variables:
# if(FALSE){
#   # Below section is how locations were generated, does not need to be run again
#   census_wide_households <- read_csv('Data Section/tmp/crf/census_wide_households.csv')
#   household_locations <- census_wide_households %>% dplyr::select(household_id = hh_id,
#                                                                   lng, lat)
#   write_csv(household_locations, 'Data Section/tmp/crf/household_locations.csv')
# }
locations <- read_csv("data/household_locations.csv")

# Read in household ID encryptions
encryptions <- read_csv("data/household_id_encryptions.csv")

# Decrypt the household IDs
locations <- locations %>%
  dplyr::rename(anonymized_household_id = household_id) %>%
  left_join(encryptions) %>%
  dplyr::select(-anonymized_household_id) %>% 
  rename(hhid = household_id)

# The above is now a table with latitude and longitude and household ID which can be joined to the clean data
df <- left_join(df, locations, by = 'hhid')

# Now "clean" has lng and lat columns

# Project to UTM coordinates so as to measure meters
proj_crs <- CRS("+proj=utm +zone=36 +south +ellps=WGS84 +datum=WGS84 +units=m +no_defs")
clean_spatial <- df %>% mutate(x = lng, y = lat)
coordinates(clean_spatial) <- ~x+y
proj4string(clean_spatial) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
clean_spatial_projected <- spTransform(clean_spatial, proj_crs)


# road_dist: distance from the household to the road
# roads <- rgdal::readOGR('Data Section/tmp/crf/mopeia_roads/', 'mopeia_roads')
# save(roads, file = 'Data Section/tmp/crf/roads.RData')
load("data/roads.RData")
roads_projected <- spTransform(roads, proj_crs)

# sanity plot
#plot(roads_projected)

# Calculate distance to all roads in meters
d <- rgeos::gDistance(clean_spatial_projected, roads_projected, byid = TRUE)
dx <- apply(d, 2, min)

# Pop the distances into the dataframe
df$road_dist <- as.numeric(dx)

# Sanity plot
roads_gg <- fortify(roads, id = 'osm_id')
ggplot() +
  geom_path(data = roads_gg,
            aes(x = long, y = lat, group = group)) +
  geom_point(data = df,
             aes(x = lng, y = lat, color = road_dist),
             size = 0.2) +
  scale_color_gradient2(mid = 'red', high = 'black')

#make the distance from meters to kilometers
df$road_km <- df$road_dist / 1000

# categorize distance
df <- df %>% 
  mutate(road_dist_cat = case_when(
    road_km <= 0.5 ~ '<= 0.5 km',
    road_km > 0.5 & road_km <= 1 ~ '0.5 - 1 km',
    road_km > 1 & road_km <= 3 ~ '1 - 3 km',
    road_km > 3 & road_km <= 6 ~ '3 - 6 km',
    road_km > 6 ~ '> 6 km'
  ))

df$road_dist_cat <- factor(as.character(df$road_dist_cat), 
                                       levels = c('<= 0.5 km', '0.5 - 1 km', 
                                                  '1 - 3 km', '3 - 6 km', 
                                                  '> 6 km'))



# ABSTRACT: FINDINGS

# find stunting, wasting, and underweight rates
rates <- df %>%
  summarize(
    stunting_rate = mean(stunted, na.rm = TRUE),
    wasting_rate = mean(wasted, na.rm = TRUE),
    underweight_rate = mean(underweight, na.rm = TRUE)
  )
stunting_rate <- rates %>% pull(stunting_rate)
wasting_rate <- rates %>% pull(wasting_rate)
underweight_rate <- rates %>% pull(underweight_rate)


# find undernutrition by age group
df %>% 
  group_by(age_category, undernutrition) %>% 
  tally() %>% 
  mutate(total = sum(n),
         perc = n/sum(n)*100)

# determine statistical significance for malnutrition by age
chisq.test(table(df$age_category, df$undernutrition))


# find undernutrition by sex
df %>% 
  group_by(sex_name, undernutrition) %>% 
  tally() %>% 
  mutate(total = sum(n),
         perc = n/sum(n)*100)

# determine statistical significance for malnutrition by gender
chisq.test(table(df$sex_name, df$undernutrition))



# RESULTS

## PREVALENCE OF STUNTING, WASTING, AND UNDERWEIGHT


# Calculate the 95% CI using prop.test for regular rates

# stunted
ci_s <- prop.test(x = round(stunting_rate * n), n = n, conf.level = 0.95)$conf.int
print(paste("95% CI:", round(ci_s[1], 3), "to", round(ci_s[2], 3)))

# wasted
ci_w <- prop.test(x = round(wasting_rate * n), n = n, conf.level = 0.95)$conf.int
print(paste("95% CI:", round(ci_w[1], 3), "to", round(ci_w[2], 3)))

# underweight
ci_u <- prop.test(x = round(underweight_rate * n), n = n, conf.level = 0.95)$conf.int
print(paste("95% CI:", round(ci_u[1], 3), "to", round(ci_u[2], 3)))


# Calculate severe rates and the 95% CI 

# stunting
sev_stunting_rate <- df %>% 
  filter(stunted == 'TRUE') %>% 
  mutate(sev_stunting = ifelse(zlen <= -3, 'TRUE', 'FALSE')) %>% 
  group_by(sev_stunting) %>% 
  tally %>% 
  mutate(p = n / sum(n)) %>% 
  filter(sev_stunting == 'TRUE') %>% 
  select(p)

ci_ss <- prop.test(x = round(sev_stunting_rate$p * n), n = n, conf.level = 0.95)$conf.int
print(paste("95% CI:", round(ci_ss[1], 3), "to", round(ci_ss[2], 3)))


# wasting

sev_wasted_rate <- df %>% 
  filter(wasted == 'TRUE') %>% 
  mutate(sev_wasted = ifelse(zwfl <= -3, 'TRUE', 'FALSE')) %>% 
  group_by(sev_wasted) %>% 
  tally %>% 
  mutate(p = n / sum(n)) %>% 
  filter(sev_wasted == 'TRUE') %>% 
  select(p)

ci_sw <- prop.test(x = round(sev_wasted_rate$p * n), n = n, conf.level = 0.95)$conf.int
print(paste("95% CI:", round(ci_sw[1], 3), "to", round(ci_sw[2], 3)))


# underweight

sev_underweight_rate <- df %>% 
  filter(underweight == 'TRUE') %>% 
  mutate(sev_underweight = ifelse(zwei <= -3, 'TRUE', 'FALSE')) %>% 
  group_by(sev_underweight) %>% 
  tally %>% 
  mutate(p = n / sum(n)) %>% 
  filter(sev_underweight == 'TRUE') %>% 
  select(p)

ci_su <- prop.test(x = round(sev_underweight_rate$p * n), n = n, conf.level = 0.95)$conf.int
print(paste("95% CI:", round(ci_su[1], 3), "to", round(ci_su[2], 3)))


# Calculate the proportions and confidence intervals for each metric and sex, ages, location, and wealth index

# Reshape the dataset to include all three metrics
df_long <- df %>%
  pivot_longer(cols = c(stunted, wasted, underweight), 
               names_to = "metric", 
               values_to = "value")

# sex
sex_summary_df <- df_long %>%
  group_by(sex_name, metric) %>%
  mutate(sex_name = ifelse(sex_name == 'f', 'Female', 'Male')) %>%
  summarise(p = mean(value == 'TRUE'),
            ci_low = t.test(value == 'TRUE', 
                            conf.level = 0.95)$conf.int[1],
            ci_high = t.test(value == 'TRUE', 
                             conf.level = 0.95)$conf.int[2]) %>%
  ungroup() 

# stunting difference
fs <- sex_summary_df %>% 
  filter(metric == 'stunted',
         sex_name == 'Female') %>% 
  pull(p)
ms <- sex_summary_df %>% 
  filter(metric == 'stunted',
         sex_name == 'Male') %>% 
  pull(p)
round((ms-fs)*100, 1)

#wasting difference
fw <- sex_summary_df %>% 
  filter(metric == 'wasted',
         sex_name == 'Female') %>% 
  pull(p)
mw <- sex_summary_df %>% 
  filter(metric == 'wasted',
         sex_name == 'Male') %>% 
  pull(p)
round((mw-fw)*100, 1)

# underweight difference
fu <- sex_summary_df %>% 
  filter(metric == 'underweight',
         sex_name == 'Female') %>% 
  pull(p)
mu <- sex_summary_df %>% 
  filter(metric == 'underweight',
         sex_name == 'Male') %>% 
  pull(p)
round((mu-fu)*100, 1)


# age
age_category_order <- c('0-11', '12-23', '24-35', '36-47', '48-60')

age_summary_df <- df_long %>%
  group_by(age_category, metric) %>%
  summarise(p = mean(value == 'TRUE'),
            ci_low = t.test(value == 'TRUE', 
                            conf.level = 0.95)$conf.int[1],
            ci_high = t.test(value == 'TRUE', 
                             conf.level = 0.95)$conf.int[2]) %>%
  ungroup() %>% 
  mutate(age_category = paste(age_category, 'Months'))

age_summary_df %>% 
  filter(metric == 'stunted') %>% 
  arrange(desc(p))
age_summary_df %>% 
  filter(metric == 'wasted') %>% 
  arrange(desc(p))
age_summary_df %>% 
  filter(metric == 'underweight') %>% 
  arrange(desc(p))


# ward
ward_summary_df <- df_long %>% 
  group_by(ward_name, metric) %>%
  summarise(p = mean(value == 'TRUE'),
            ci_low = t.test(value == 'TRUE', 
                            conf.level = 0.95)$conf.int[1],
            ci_high = t.test(value == 'TRUE', 
                             conf.level = 0.95)$conf.int[2]) %>%
  ungroup() 

ward_summary_df %>% 
  filter(metric == 'stunted') %>% 
  arrange(desc(p))
ward_summary_df %>% 
  filter(metric == 'wasted') %>% 
  arrange(desc(p))
ward_summary_df %>% 
  filter(metric == 'underweight') %>% 
  arrange(desc(p))


# wealth index
wealth_index_rank_order <- c('Least poor',
                             "Less poor",
                             "Moderately poor",
                             "Poorer",
                             "Poorest")

wealth_summary_df <- df_long %>%
  group_by(wealth_index_rank, metric) %>%
  summarise(p = mean(value == 'TRUE'),
            ci_low = t.test(value == 'TRUE', 
                            conf.level = 0.95)$conf.int[1],
            ci_high = t.test(value == 'TRUE', 
                             conf.level = 0.95)$conf.int[2]) %>%
  ungroup() %>% 
  mutate(metric = factor(metric,
                         level = c('stunted',
                                   'wasted',
                                   'underweight'),
                         ordered = TRUE))

wealth_summary_df %>% 
  filter(metric == 'stunted') %>% 
  arrange(desc(p))
wealth_summary_df %>% 
  filter(metric == 'wasted') %>% 
  arrange(desc(p))
wealth_summary_df %>% 
  filter(metric == 'underweight') %>% 
  arrange(desc(p))


# Figure 2a: Proportion of stunted, wasted, and underweight under 5 children by wealth in Mopeia
wealth_plot <- wealth_summary_df %>%
  ggplot(aes(x = wealth_index_rank, 
             y = p, 
             fill = metric)) +
  geom_bar(stat = 'identity', position = position_dodge(width = 0.9)) +
  geom_errorbar(aes(ymin = ci_low, ymax = ci_high), 
                position = position_dodge(width = 0.9), 
                width = 0.2) +
  geom_text(aes(label = sprintf("%.1f%%", p * 100, "%"),
                y = ci_high + 0.01),
            position = position_dodge(width = 0.9),  
            vjust = -0.05) +
  labs(y = 'Proportion',
       fill = '',
       title = 'A.') +
  scale_y_continuous(labels = percent_format(scale = 100), limits = c(0, 0.6)) +
  theme_minimal() +  
  scale_fill_manual(values = c("stunted" = '#E69F00',
                               "wasted" = '#56B4E9',
                               "underweight" = '#009E73'),
                    labels = c("stunted" = "Stunted",
                               "wasted" = "Wasted",
                               "underweight" = "Underweight")) +
  theme(axis.ticks.x = element_blank(), 
        axis.title.x = element_blank(), 
        #axis.text.y = element_text(size = 10),
        #axis.text.x = element_text(size = 7.5),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        legend.position = "bottom")

# Display the plot
print(wealth_plot)


# Figure 2b. Number of stunted, wasted, and underweight under 5 children by distance to tarmac road in Mopeia
df_long$metric <- factor(df_long$metric, levels = c("stunted", "wasted", "underweight"))


distance_plot <- ggplot(df_long, aes(x = road_dist, fill = metric)) +
  geom_histogram() +
  theme_minimal() +  
  scale_fill_manual(values = c("stunted" = '#E69F00',
                               "wasted" = '#56B4E9',
                               "underweight" = '#009E73'),
                    labels = c("stunted" = "Stunted",
                               "wasted" = "Wasted",
                               "underweight" = "Underweight")) +
  labs(x = 'Distance (km)', y = 'Count', fill = '', shape = '',
       title = 'B.') +
  theme(legend.position = "bottom")

# Display the plot
print(distance_plot)



## make the ward map with stunting rates

# create a table for ward name with count and frequency while making a new column of its type
ward_table <- df %>%
  group_by(ward_name) %>%
  summarize(
    n = n(),
    frequency = mean(stunted_numeric)
  )

# outline of Mopeia
load("data/mop.RData")
plot(mop)

#fortify the shape file so that it is ggplot compatible
mop_fortified <- fortify(mop, id = mop@data$NAME_0)

# map of stunting points on the Mopeia mop file
ggplot() +
  geom_polygon(data = mop_fortified, 
               aes(x = long,
                   y = lat,
                   group = group)) +
  geom_point(data = df,
             aes(x = lng,
                 y = lat,
                 color = stunted))

# Make choropleths (using wards) to make a better looking graph of stunting
df <- df %>% mutate(x = lng, y = lat)
cluster_points <- df %>% dplyr::select(x, y, ward_name)
coordinates(cluster_points) <- ~x+y
proj4string(cluster_points) <- proj4string(mop)
row.names(cluster_points) <- 1:nrow(cluster_points)
cluster_points$median_distance <- NA

# Remove outliers (will take a a couple of minutes)
for(i in 1:nrow(cluster_points)){
  this_point <- cluster_points[i,]
  other_points_in_ward <- cluster_points[cluster_points$ward_name == this_point$ward_name,]
  distances <- rgeos::gDistance(this_point, other_points_in_ward, byid = TRUE)
  median_distance <- median(distances)
  cluster_points$median_distance[i] <- median_distance
}

# Remove those with large median distance
cluster_points <- cluster_points[cluster_points$median_distance <= 0.2,]

# voronoi tesselation
v <- dismo::voronoi(cluster_points)
# inspect
plot(v)
# collapse
x = gUnaryUnion(v, id = v$ward_name, checkValidity = 2)
# match row names
ward_table <- data.frame(ward_table)
row.names(ward_table) <- as.character(ward_table$ward_name)
# make polygons dataframe
dfv <- SpatialPolygonsDataFrame(Sr = x, data = ward_table, match.ID = TRUE)
# inspect data
dfv@data
# reproject
proj4string(dfv) <- proj4string(mop)
# clip
r <- gIntersection(mop, dfv, byid = TRUE)
r <- SpatialPolygonsDataFrame(Sr = r, data = ward_table, match.ID = FALSE)
# Fortify
rf <- fortify(r, regions = r@data$ward_name)
# clean up
rf$ward_name <- gsub('30 ', '', rf$id)
# join with ward info
rf <- left_join(rf, ward_table)

# Calculate the centroids of each choropleth polygon
rf_centroids <- rf %>%
  group_by(ward_name) %>%
  summarize(x = median(long),
            y = median(lat),
            x1 = mean(long),
            y1 = mean(lat))

# Function to add line breaks at periods in ward names
wrap_ward_names <- function(x) {
  str_wrap(x, width = 14)  # Adjust the width value as needed to control the length of each line
}

# Point data for the center of village
x_coord <- 35.710798061452984
y_coord <- -17.981304088784032

# Create a data frame with the x and y coordinates and 'Type' as 'Village Center'
point_data <- data.frame(lng = x_coord, 
                         lat = y_coord,
                         Type = "Village Center")


# plot of prevalence of stunting in children under 5 in Wards
stunting_plot <- ggplot() + 
  geom_polygon(data = rf,
               aes(x = long,
                   y = lat,
                   group = group,
                   fill = frequency),
               color = 'white') + 
  scale_fill_gradient2_tableau(name = 'Prevalence of Stunting',
                               palette = 'Green-Blue Diverging',
                               labels = label_percent(scale = 100),
                               breaks = pretty(range(rf$frequency), n = 5)) +   
  geom_label_repel(data = rf_centroids,
                   aes(x = x, 
                       y = y, 
                       label = ward_name),
                   size = 3.5, color = "black", fontface = "bold") +
  ggthemes::theme_map() +
  theme(legend.position = c(0.05,-0.15),
        legend.spacing.x = unit(0.5, "cm"),  # Increase horizontal spacing between legend items
        legend.text = element_text(angle = 45, hjust = 1)) +  # Rotate legend labels
  guides(fill = guide_colorbar(title.position = "top", 
                               title.hjust = 0.5, 
                               title.vjust = 0.5,
                               direction = "horizontal")) +
  coord_fixed() +
  labs(title = "B.")




## make the ward map with wasting rates

# create a table for ward name with count and frequency while making a new column of its type
ward_wasting_table <- df %>%
  group_by(ward_name) %>%
  summarize(
    n = n(),
    frequency = mean(wasted_numeric)
  )

# map of Wasting points on the Mopeia mop file
ggplot() +
  geom_polygon(data = mop_fortified, 
               aes(x = long,
                   y = lat,
                   group = group)) +
  geom_point(data = df,
             aes(x = lng,
                 y = lat,
                 color = wasted))

# match row names
ward_wasting_table <- data.frame(ward_wasting_table)
row.names(ward_wasting_table) <- as.character(ward_wasting_table$ward_name)
# make polygons dataframe
dfv <- SpatialPolygonsDataFrame(Sr = x, data = ward_wasting_table, match.ID = TRUE)
# inspect data
dfv@data
# reproject
proj4string(dfv) <- proj4string(mop)
# clip
r <- gIntersection(mop, dfv, byid = TRUE)
r <- SpatialPolygonsDataFrame(Sr = r, data = ward_wasting_table, match.ID = FALSE)
# Fortify
rf <- fortify(r, regions = r@data$ward_name)
# clean up
rf$ward_name <- gsub('30 ', '', rf$id)
# join with ward info
rf <- left_join(rf, ward_wasting_table)

# Calculate the centroids of each choropleth polygon
rf_centroids <- rf %>%
  group_by(ward_name) %>%
  summarize(x = median(long),
            y = median(lat),
            x1 = mean(long),
            y1 = mean(lat))

# Function to add line breaks at periods in ward names
wrap_ward_names <- function(x) {
  str_wrap(x, width = 14)  # Adjust the width value as needed to control the length of each line
}


# plot of prevalence of Wasting in children under 5 in Wards
wasting_plot <- ggplot() + 
  geom_polygon(data = rf,
               aes(x = long,
                   y = lat,
                   group = group,
                   fill = frequency),
               color = 'white') + 
  scale_fill_gradient2_tableau(name = 'Prevalence of Wasting',
                               palette = 'Green-Blue Diverging',
                               labels = label_percent(scale = 100),
                               breaks = pretty(range(rf$frequency), n = 6)) +   
  geom_label_repel(data = rf_centroids,
                   aes(x = x, 
                       y = y, 
                       label = ward_name),
                   size = 3.5, color = "black", fontface = "bold") +
  ggthemes::theme_map() +
  theme(legend.position = c(0.05,-0.15),
        legend.spacing.x = unit(0.5, "cm"),  # Increase horizontal spacing between legend items
        legend.text = element_text(angle = 45, hjust = 1)) +  # Rotate legend labels
  guides(fill = guide_colorbar(title.position = "top", 
                               title.hjust = 0.5, 
                               title.vjust = 0.5,
                               direction = "horizontal")) +
  coord_fixed() +
  labs(title = "C.")



## make the ward map with Underweight rates

# create a table for ward name with count and frequency while making a new column of its type
ward_underweight_table <- df %>%
  group_by(ward_name) %>%
  summarize(
    n = n(),
    frequency = mean(underweight_numeric)
  )


# map of Underweight points on the Mopeia mop file
ggplot() +
  geom_polygon(data = mop_fortified, 
               aes(x = long,
                   y = lat,
                   group = group)) +
  geom_point(data = df,
             aes(x = lng,
                 y = lat,
                 color = underweight))

# match row names
ward_underweight_table <- data.frame(ward_underweight_table)
row.names(ward_underweight_table) <- as.character(ward_underweight_table$ward_name)
# make polygons dataframe
dfv <- SpatialPolygonsDataFrame(Sr = x, data = ward_underweight_table, match.ID = TRUE)
# inspect data
dfv@data
# reproject
proj4string(dfv) <- proj4string(mop)
# clip
r <- gIntersection(mop, dfv, byid = TRUE)
r <- SpatialPolygonsDataFrame(Sr = r, data = ward_underweight_table, match.ID = FALSE)
# Fortify
rf <- fortify(r, regions = r@data$ward_name)
# clean up
rf$ward_name <- gsub('30 ', '', rf$id)
# join with ward info
rf <- left_join(rf, ward_underweight_table)

# Calculate the centroids of each choropleth polygon
rf_centroids <- rf %>%
  group_by(ward_name) %>%
  summarize(x = median(long),
            y = median(lat),
            x1 = mean(long),
            y1 = mean(lat))

# Function to add line breaks at periods in ward names
wrap_ward_names <- function(x) {
  str_wrap(x, width = 14)  # Adjust the width value as needed to control the length of each line
}


# plot of prevalence of underweight in children under 5 in Wards
underweight_plot <- ggplot() + 
  geom_polygon(data = rf,
               aes(x = long,
                   y = lat,
                   group = group,
                   fill = frequency),
               color = 'white') + 
  scale_fill_gradient2_tableau(
    name = 'Prevalence of Underweight',
    palette = 'Green-Blue Diverging',
    labels = label_percent(scale = 100),
    breaks = pretty(range(rf$frequency), n = 5)  # Adding more breaks
  ) +  
  geom_label_repel(data = rf_centroids,
                   aes(x = x, 
                       y = y, 
                       label = ward_name),
                   size = 3.5, color = "black", fontface = "bold") +
  ggthemes::theme_map() +
  theme(legend.position = c(0.05,-0.15),
        legend.spacing.x = unit(0.5, "cm"),  # Increase horizontal spacing between legend items
        legend.text = element_text(angle = 45, hjust = 1)) +  # Rotate legend labels
  guides(fill = guide_colorbar(title.position = "top", 
                               title.hjust = 0.5, 
                               title.vjust = 0.5,
                               direction = "horizontal")) +
  coord_fixed() +
  labs(title = "D.")


# map of points
point_map <- ggplot() +
  geom_polygon(data = rf,
               aes(x = long,
                   y = lat,
                   group = group,
                   fill = ward_name),
               color = 'white') +
  geom_point(data = df,
             aes(x = lng,
                 y = lat)) +
  geom_path(data = roads_gg,
            aes(x = long, y = lat, group = group))  +
  # geom_label_repel(data = rf_centroids,
  #                  aes(x = x, 
  #                      y = y, 
  #                      label = ward_name),
  #                  size = 3.5, color = "black", fontface = "bold") +
  theme_void()  +
  labs(title = "A.",
       fill = 'Locality') #+
  #theme(legend.position = "none")  # Hide the legend




# CHARACTERISTICS OF STUDY PARTICIPANTS

n # sample size

# female sample
df %>% 
  mutate(sex_name = ifelse(sex_name == 'f', 'Female', 'Male')) %>% 
  group_by(sex_name) %>% 
  tally %>% 
  mutate(p = round(n / sum(n) * 100, 2)) 

# mean age of children
df %>% 
  summarise(mean_age = mean(age_in_months),
            sd_age = sd(age_in_months, na.rm = TRUE))

# least represented age group
df %>% 
  group_by(age_category) %>% 
  tally %>% 
  mutate(p = round(n / sum(n) * 100, 2)) %>% 
  arrange(n)

# Table # Sociodemographic characteristics of children aged  1-60 months  participating in the BOHEMIA clinical trial in Mopeia (n = 2,667)

# insert the distance quintiles HERE

wealth_table <- df %>% 
  group_by(wealth_index_rank) %>% 
  tally %>% 
  mutate(p = round(n / sum(n) * 100, 1)) 

distance_table <- df %>% 
  group_by(road_dist_cat) %>% 
  tally %>% 
  mutate(p = round(n / sum(n) * 100, 1)) 

age_table <- df %>% 
  group_by(age_category) %>% 
  tally %>% 
  mutate(p = round(n / sum(n) * 100, 1)) 

sex_table <- df %>% 
  mutate(sex_name = ifelse(sex_name == 'f', 'Female', 'Male')) %>% 
  group_by(sex_name) %>% 
  tally %>% 
  mutate(p = round(n / sum(n) * 100, 1)) 

ward_table <- df %>% 
  group_by(ward_name) %>% 
  tally %>% 
  mutate(p = round(n / sum(n) * 100, 1))

char_table <- bind_rows(
  wealth_table, distance_table, age_table, sex_table, ward_table
) %>% 
  mutate(Variables = coalesce(
    wealth_index_rank, road_dist_cat, age_category, sex_name, ward_name),
    'Frequency n (%)' = paste0(n, " (", p, ")")
  ) %>% 
  select(Variables, 'Frequency n (%)')

kbl(char_table, caption = 'Sociodemographic characteristics of children aged  1-60 months  participating in the BOHEMIA clinical trial in Mopeia (n = 2,667)') %>%
  kable_styling(bootstrap_options = c('striped', 'condensed', font_size = 12)) %>%
  pack_rows('Wealth index rank', 1, 5) %>% # pack_rows puts the rows in groups
  pack_rows('Distance to nearest tarmac road (kilometers)', 6, 10) %>% 
  pack_rows('Child sex', 11, 12) %>% 
  pack_rows('Child age (in months)', 13, 17) %>%
  pack_rows('Ward', 18, 25) %>%
  kable_minimal()



# SOCIOECONOMIC CHARACTERISTICS

# mean household members
df %>% 
  summarise(mean_hh_mem = round(mean(hh_member_num, na.rm = TRUE)))

# people who did not use electricity
round((df %>% 
  filter(hh_main_energy_source_for_lighting %in% c('BATERIA', 'batteries')) %>% 
  tally() ) /
  (df %>% 
     filter(hh_main_energy_source_for_lighting_cat == 'other') %>% 
     tally()),3)

# people who do own an electronic device
df %>% 
  group_by(hh_possessions_cat) %>% 
  filter(!is.na(hh_possessions_cat)) %>%
  tally() %>% 
  summarise(sum(n)) %>% 
  mutate(perc = `sum(n)`/2667*100)

df %>%
  mutate(hh_pos = case_when(
    grepl('cell_phone', hh_possessions) & grepl('radio', hh_possessions) & grepl('tv', hh_possessions) ~ 'cell_phone, radio, tv',
    grepl('cell_phone', hh_possessions) & grepl('radio', hh_possessions) ~ 'cell_phone, radio',
    grepl('cell_phone', hh_possessions) & grepl('tv', hh_possessions) ~ 'cell_phone, tv',
    grepl('radio', hh_possessions) & grepl('tv', hh_possessions) ~ 'radio, tv',
    
    grepl('cell_phone', hh_possessions) ~ 'cell_phone',
    grepl('tv', hh_possessions) ~ 'tv',
    grepl('radio', hh_possessions) ~ 'radio',
    hh_possessions == 'none' ~ 'none'
  )) %>% 
  filter(!is.na(hh_pos)) %>% 
  group_by(hh_pos) %>%
  tally %>%
  mutate(p = round(n/sum(n)*100, 1)) %>%
  arrange(n)


# table 2. Socioeconomic characteristics of the household of children aged 1-60 months participating in the BOHEMIA clinical trial in Mopeia, (n = 2,667   )

hh_memb_table <- df %>% 
  group_by(hh_member_num_binary) %>% 
  tally %>% 
  mutate(p = round(n / sum(n) * 100, 1)) 

hh_n_const_table <- df %>% 
  group_by(hh_n_constructions_cat) %>% 
  tally %>% 
  mutate(p = round(n / sum(n) * 100, 1)) 

hh_n_sleep_table <- df %>% 
  group_by(hh_n_constructions_sleep_cat) %>% 
  tally %>% 
  mutate(p = round(n / sum(n) * 100, 1)) 

nets_table <- df %>% 
  group_by(n_nets_in_hh_cat) %>% 
  tally %>% 
  mutate(p = round(n / sum(n) * 100, 1))

pigs_cows_table <- df %>% 
  group_by(hh_owns_cattle_or_pigs) %>% 
  tally %>% 
  mutate(p = round(n / sum(n) * 100, 1))

building_table <- df %>% 
  group_by(hh_main_building_type_cat) %>% 
  tally %>% 
  mutate(p = round(n / sum(n) * 100, 1)) 

wall_table <- df %>% 
  group_by(hh_main_wall_material_cat) %>% 
  tally %>% 
  mutate(p = round(n / sum(n) * 100, 1))

time_to_table <- df %>% 
  group_by(cook_time_to_water) %>% 
  tally %>% 
  mutate(p = round(n / sum(n) * 100, 1)) 

main_water_table <- df %>% 
  group_by(cook_main_water_source_cat) %>% 
  tally %>% 
  mutate(p = round(n / sum(n) * 100, 1)) 

energy_table <- df %>% 
  group_by(hh_main_energy_source_for_lighting_cat) %>% 
  tally %>% 
  mutate(p = round(n / sum(n) * 100, 1)) 

possessions_table <- df %>% 
  group_by(hh_possessions_cat) %>% 
  tally %>% 
  mutate(p = round(n / sum(n) * 100, 1)) 


socio_table <- bind_rows(
  hh_memb_table, hh_n_const_table, hh_n_sleep_table, nets_table, pigs_cows_table, 
  building_table, wall_table, time_to_table,
  main_water_table, energy_table, possessions_table
) %>%
  mutate(Variables = coalesce(
    hh_member_num_binary, hh_n_constructions_cat, hh_n_constructions_sleep_cat, 
    n_nets_in_hh_cat, hh_owns_cattle_or_pigs, hh_main_building_type_cat, 
    hh_main_wall_material_cat, cook_time_to_water, cook_main_water_source_cat,
    hh_main_energy_source_for_lighting_cat, hh_possessions_cat),
    'Frequency n (%)' = paste0(n, " (", p, ")")
  ) %>% 
  select(Variables, 'Frequency n (%)')


kbl(socio_table, caption = 'Socioeconomic characteristics of the household of children aged 1-60 months participating in the BOHEMIA clinical trial in Mopeia, (n = 2,667)') %>%
  kable_styling(bootstrap_options = c('striped', 'condensed', font_size = 12)) %>%
  pack_rows('Household members', 1, 2) %>%
  pack_rows('Household constructions', 3, 6) %>%
  pack_rows('Members per sleeping room', 7, 11) %>%
  pack_rows('Bed nets in household', 12, 15) %>%
  pack_rows('Ownership of cattle or pigs', 16, 18) %>%
  pack_rows('Main housing building type', 19, 22) %>%
  pack_rows('Wall material in the main house', 23, 26) %>%
  pack_rows('Time for taking main water source', 27, 30) %>%
  pack_rows('Main water source for consumption', 31, 33) %>%
  pack_rows('Main energy source for lighting', 34, 36) %>% 
  pack_rows('Ownership of electronic devices', 37, 41) %>%
  kable_minimal()



# FACTORS ASSOCIATED WITH STUNTING, WASTING, AND UNDERWEIGHT

## logistic regression of distance to roads
# stunting
road_lm <- lm(zlen ~ road_km, data = df)
summary(road_lm)

# wasting
road_lm_wasted <- lm(zwfl ~ road_km, data = df)
summary(road_lm_wasted)

# underweight
road_lm_underweight <- lm(zwei ~ road_km, data = df)
summary(road_lm_underweight)



# stunting and wealth
wealth_glm_stunted <- glm(stunted ~ wealth_index_rank, data = df, family = binomial)
summary(wealth_glm_stunted)

tab_model(wealth_glm_stunted,
          dv.labels = "Model of Wasting",
          string.ci = "Conf. Int (95%)",
          string.p = "P-Value",
          transform = "exp") 

# wasting and wealth
wealth_glm_wasted <- glm(wasted ~ wealth_index_rank, data = df, family = binomial)
summary(wealth_glm_wasted)

tab_model(wealth_glm_wasted,
          dv.labels = "Model of Wasting",
          string.ci = "Conf. Int (95%)",
          string.p = "P-Value",
          transform = "exp") 

# underweight and wealth
wealth_glm_underweight <- glm(underweight ~ wealth_index_rank, data = df, family = binomial)
summary(wealth_glm_underweight)

tab_model(wealth_glm_underweight,
          dv.labels = "Model of Underweight",
          string.ci = "Conf. Int (95%)",
          string.p = "P-Value",
          transform = "exp") 



# prep for step-wise regression analysis


# make the variable for all_covariates
all_covariates <- c("hh_member_num_cat", "hh_n_constructions_cat", "hh_n_constructions_sleep_cat", "hh_owns_cattle_or_pigs",
                    "hh_main_building_type_cat", "hh_main_wall_material_cat", "cook_time_to_water_cat", "cook_main_water_source_cat",
                    "hh_main_energy_source_for_lighting_cat", "hh_possessions_cat", "n_nets_in_hh_cat")

model_df <- df %>%
  group_by( extid ) %>% 
  arrange( current_visit ) %>% 
  slice_head( ) %>% 
  ungroup() %>%
  group_by( hhid ) %>% 
  mutate( n_kids = n() ) %>% 
  ungroup()


# STEPWISE REGRESSION ----

# make a null model
null_model <- glm(stunted_numeric ~ 1, family = binomial, data = model_df)
summary(null_model)

# initialize with null model formula :
current_formula <- formula( null_model )
current_model <- null_model
pool <- all_covariates
keep_going <- TRUE
i <- 1
while(keep_going) {
  message("Step ", i, " ... ", length(pool), " variables remaining")
  i <- i + 1
  n_current_model_coefficients <- length( current_model$coefficients )
  # fit all candidate models :
  candidates <- lapply(as.list(pool),
                       function(p) {
                         stats::update(current_model, formula = as.formula(paste(". ~ . + ", p)))
                       })
  # get p-values for each newly added covariate :
  p_values <- sapply(candidates,
                     function(m) {
                       n_coeffs <- length( m$coefficients ) - n_current_model_coefficients
                       min( tail( summary(m)$coefficients[,4], n_coeffs ) )
                     })
  if (min(p_values) < .05 ) {
    idx <- which.min(p_values)
    message("... adding ", pool[idx], " (min p-value ", p_values[idx], ")")
    current_formula <- as.formula( paste( c( current_formula, pool[idx] ), collapse = " + ") )
    current_model <- glm(current_formula, family = "binomial", data = model_df)
    # message("... updated model:")
    # print( summary(current_model) )
    pool <- pool[-idx]
  } else{
    message("Done! Remaining covariates are not significant")
    keep_going <- FALSE
  }
}

message("Final model:")
print( summary( current_model ) )

# here's what happens if we dichotomize hh_member_num :

model_df <- model_df %>%
  mutate( hh_member_num_binary = ifelse( hh_member_num_cat == "1-5", "1-5", "6 or more" ) )
model_df$hh_member_num_binary <- factor( model_df$hh_member_num_binary, levels = c("1-5", "6 or more") )

all_covariates <- c( "hh_member_num_binary", all_covariates[-1] )

# STEPWISE REGRESSION, TAKE 2 ----

# make a null model
null_model <- glm(stunted_numeric ~ 1, family = binomial, data = model_df)
summary(null_model)

# initialize with null model formula :
current_formula <- formula( null_model )
current_model <- null_model
pool <- all_covariates
keep_going <- TRUE
i <- 1
while(keep_going) {
  message("Step ", i, " ... ", length(pool), " variables remaining")
  i <- i + 1
  n_current_model_coefficients <- length( current_model$coefficients )
  # fit all candidate models :
  candidates <- lapply(as.list(pool),
                       function(p) {
                         stats::update(current_model, formula = as.formula(paste(". ~ . + ", p)))
                       })
  # get p-values for each newly added covariate :
  p_values <- sapply(candidates,
                     function(m) {
                       n_coeffs <- length( m$coefficients ) - n_current_model_coefficients
                       min( tail( summary(m)$coefficients[,4], n_coeffs ) )
                     })
  if (min(p_values) < .05 ) {
    idx <- which.min(p_values)
    message("... adding ", pool[idx], " (min p-value ", p_values[idx], ")")
    current_formula <- as.formula( paste( c( current_formula, pool[idx] ), collapse = " + ") )
    current_model <- glm(current_formula, family = "binomial", data = model_df)
    # message("... updated model:")
    # print( summary(current_model) )
    pool <- pool[-idx]
  } else{
    message("Done! Remaining covariates are not significant")
    keep_going <- FALSE
  }
}

message("Final model:")
print( summary( current_model ) )

tab_model(current_model,
          dv.labels = "Model of Stunting",
          string.ci = "Conf. Int (95%)",
          string.p = "P-Value")



### step-wise for wasting
# make a null model
null_model <- glm(wasted_numeric ~ 1, family = binomial, data = model_df)
summary(null_model)

# initialize with null model formula :
current_formula <- formula( null_model )
current_model <- null_model
pool <- all_covariates
keep_going <- TRUE
i <- 1
while(keep_going) {
  message("Step ", i, " ... ", length(pool), " variables remaining")
  i <- i + 1
  n_current_model_coefficients <- length( current_model$coefficients )
  # fit all candidate models :
  candidates <- lapply(as.list(pool),
                       function(p) {
                         stats::update(current_model, formula = as.formula(paste(". ~ . + ", p)))
                       })
  # get p-values for each newly added covariate :
  p_values <- sapply(candidates,
                     function(m) {
                       n_coeffs <- length( m$coefficients ) - n_current_model_coefficients
                       min( tail( summary(m)$coefficients[,4], n_coeffs ) )
                     })
  if (min(p_values) < .05 ) {
    idx <- which.min(p_values)
    message("... adding ", pool[idx], " (min p-value ", p_values[idx], ")")
    current_formula <- as.formula( paste( c( current_formula, pool[idx] ), collapse = " + ") )
    current_model <- glm(current_formula, family = "binomial", data = model_df)
    # message("... updated model:")
    # print( summary(current_model) )
    pool <- pool[-idx]
  } else{
    message("Done! Remaining covariates are not significant")
    keep_going <- FALSE
  }
}

message("Final model:")
print( summary( current_model ) )

tab_model(current_model,
          dv.labels = "Model of Wasting",
          string.ci = "Conf. Int (95%)",
          string.p = "P-Value")


### step-wise for underweight
# make a null model
null_model <- glm(underweight_numeric ~ 1, family = binomial, data = model_df)
summary(null_model)

# initialize with null model formula :
current_formula <- formula( null_model )
current_model <- null_model
pool <- all_covariates
keep_going <- TRUE
i <- 1
while(keep_going) {
  message("Step ", i, " ... ", length(pool), " variables remaining")
  i <- i + 1
  n_current_model_coefficients <- length( current_model$coefficients )
  # fit all candidate models :
  candidates <- lapply(as.list(pool),
                       function(p) {
                         stats::update(current_model, formula = as.formula(paste(". ~ . + ", p)))
                       })
  # get p-values for each newly added covariate :
  p_values <- sapply(candidates,
                     function(m) {
                       n_coeffs <- length( m$coefficients ) - n_current_model_coefficients
                       min( tail( summary(m)$coefficients[,4], n_coeffs ) )
                     })
  if (min(p_values) < .05 ) {
    idx <- which.min(p_values)
    message("... adding ", pool[idx], " (min p-value ", p_values[idx], ")")
    current_formula <- as.formula( paste( c( current_formula, pool[idx] ), collapse = " + ") )
    current_model <- glm(current_formula, family = "binomial", data = model_df)
    # message("... updated model:")
    # print( summary(current_model) )
    pool <- pool[-idx]
  } else{
    message("Done! Remaining covariates are not significant")
    keep_going <- FALSE
  }
}

message("Final model:")
print( summary( current_model ) )

tab_model(current_model,
          dv.labels = "Model of Undernourishment",
          string.ci = "Conf. Int (95%)",
          string.p = "P-Value")



# DISCUSSION

# Figure 3. Undernutrition rates of under 5 children between Mozambique and Mopeia
undernutrition <- data.frame(mopeia_category = c('Stunted',
                                                 'Wasted',
                                                 'Underweight'),
                             mopeia_rates = c(41.7, 
                                              20.4, 
                                              24.3),
                             moz_2022_avg_rates = c(36.7,
                                                    15.4,
                                                    3.8),
                             moz_2003_avg_rates = c(41,
                                                    24,
                                                    4))

undernutrition_long <- undernutrition %>%
  pivot_longer(cols = c(mopeia_rates, moz_2022_avg_rates),  # Pivot these specific columns
               names_to = "category_type",  # New column for category type
               values_to = "rates") %>%  # New column for rates
  mutate(category_type = factor(category_type, 
                                levels = c("moz_2022_avg_rates",
                                           "mopeia_rates"),
                                ordered = TRUE)) %>% 
  mutate(mopeia_category = factor(mopeia_category,
                                  levels = c('Stunted',
                                             'Wasted',
                                             'Underweight'),
                                  ordered = TRUE))

ggplot(undernutrition_long, aes(x = mopeia_category, y = rates, fill = category_type)) +
  
  geom_bar(stat = 'identity', position = position_dodge(width = 0.9)) +
  geom_text(aes(label = paste0(round(rates, 1), "%")),
            position = position_dodge(width = 0.9),  
            vjust = 1.2, size = 3.2) +
  labs(y = "Rates",
       fill = "") +
  theme_minimal() +
  scale_fill_manual(values = c('moz_2022_avg_rates' = '#CC79A7',
                               'mopeia_rates' = '#00BFC4'),
                    labels = c('moz_2022_avg_rates' = paste0('Average rates in Mozambique\n(Source: World Bank 2022)'),
                               'mopeia_rates' = 'Average rates in Mopeia\n(Source: BOHEMIA 2021)')) +
  theme(legend.position = 'bottom',
        axis.title.x = element_blank())


